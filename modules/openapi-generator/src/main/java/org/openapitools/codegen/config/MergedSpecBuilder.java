package org.openapitools.codegen.config;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import com.google.common.collect.ImmutableMap;
import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.core.util.Json;
import io.swagger.v3.core.util.Yaml;
import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.servers.Server;
import io.swagger.v3.parser.core.models.ParseOptions;
import org.apache.commons.lang3.ObjectUtils;
import org.openapitools.codegen.auth.AuthParser;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class MergedSpecBuilder {

    /**
     * Controls how multiple spec files are merged together.
     *
     * <ul>
     *   <li>{@link #REF} (default) — original behaviour: generates a lightweight "index" spec
     *       where each path is a {@code $ref} pointing back to the originating spec file.
     *       Components stay in their original files and are resolved at generation time.
     *       This preserves backward compatibility.</li>
     *   <li>{@link #DEEP} — opt-in: fully parses and inlines all paths and components into a
     *       single self-contained spec. Detects and handles component/path+method conflicts
     *       according to {@link MergeConflictStrategy}.</li>
     * </ul>
     */
    public enum MergeMode {
        /** Original $ref-based shallow merge (default, backward-compatible). */
        REF,
        /** Full deep merge: inlines all components and paths into one spec. */
        DEEP
    }

    /**
     * Controls what happens when two specs define the same component name (schema, response, etc.)
     * or the same path+method with different definitions. Only applies when {@link MergeMode#DEEP}
     * is active.
     */
    public enum MergeConflictStrategy {
        /** Log a warning and keep the first definition (default). */
        WARN,
        /** Throw a {@link RuntimeException} and abort the merge. */
        FAIL
    }

    private static final Logger LOGGER = LoggerFactory.getLogger(MergedSpecBuilder.class);

    private static final Set<String> SPEC_EXTENSIONS = new HashSet<>(Arrays.asList(".yaml", ".yml", ".json"));

    private static final ObjectMapper JSON_MAPPER = new ObjectMapper();
    private static final ObjectMapper YAML_MAPPER = new ObjectMapper(new YAMLFactory());

    private final String inputSpecRootDirectory;
    private final String mergeFileName;
    private final String mergedFileInfoName;
    private final String mergedFileInfoDescription;
    private final String mergedFileInfoVersion;
    private final String auth;
    private MergeMode mergeMode = MergeMode.REF;
    private MergeConflictStrategy conflictStrategy = MergeConflictStrategy.WARN;

    public MergedSpecBuilder(final String rootDirectory, final String mergeFileName) {
        this(rootDirectory, mergeFileName, "merged spec", "merged spec", "1.0.0", null);
    }

    public MergedSpecBuilder(final String rootDirectory, final String mergeFileName,
                             final String mergedFileInfoName, final String mergedFileInfoDescription, final String mergedFileInfoVersion, final String auth) {
        this.inputSpecRootDirectory = rootDirectory;
        this.mergeFileName = mergeFileName;
        this.mergedFileInfoName = mergedFileInfoName;
        this.mergedFileInfoDescription = mergedFileInfoDescription;
        this.mergedFileInfoVersion = mergedFileInfoVersion;
        this.auth = auth;
    }

    /**
     * Sets the merge mode. {@link MergeMode#REF} (default) uses the original $ref-based approach
     * for backward compatibility. {@link MergeMode#DEEP} fully inlines all components and paths.
     *
     * @return this builder, for chaining
     */
    public MergedSpecBuilder withMergeMode(MergeMode mode) {
        this.mergeMode = mode;
        return this;
    }

    /**
     * Sets the strategy used when two specs define the same component name or path+method
     * with conflicting (non-identical) definitions. Only applies when {@link MergeMode#DEEP}
     * is active.
     *
     * @param strategy {@link MergeConflictStrategy#WARN} to log a warning and keep the first
     *                 definition (default), or {@link MergeConflictStrategy#FAIL} to throw a
     *                 {@link RuntimeException} and abort.
     * @return this builder, for chaining
     */
    public MergedSpecBuilder withConflictStrategy(MergeConflictStrategy strategy) {
        this.conflictStrategy = strategy;
        return this;
    }

    public String buildMergedSpec() {
        deleteMergedFileFromPreviousRun();
        List<String> specRelatedPaths = getAllSpecFilesInDirectory();
        if (specRelatedPaths.isEmpty()) {
            throw new RuntimeException("Spec directory doesn't contain any specification");
        }
        LOGGER.info("In spec root directory {} found specs {}", inputSpecRootDirectory, specRelatedPaths);

        if (mergeMode == MergeMode.DEEP) {
            return buildDeepMergedSpec(specRelatedPaths);
        }
        return buildRefMergedSpec(specRelatedPaths);
    }

    // -------------------------------------------------------------------------
    // REF mode — original $ref-based shallow merge (identical to master)
    // -------------------------------------------------------------------------

    private String buildRefMergedSpec(List<String> specRelatedPaths) {
        String openapiVersion = null;
        boolean isJson = false;
        ParseOptions options = new ParseOptions();
        options.setResolve(true);
        List<SpecWithPaths> allPaths = new ArrayList<>();
        List<Server> allServers = new ArrayList<>();

        for (String specRelatedPath : specRelatedPaths) {
            String specPath = inputSpecRootDirectory + File.separator + specRelatedPath;
            try {
                LOGGER.info("Reading spec: {}", specPath);

                OpenAPI result = new OpenAPIParser()
                        .readLocation(specPath, AuthParser.parse(auth), options)
                        .getOpenAPI();

                if (result == null) {
                    LOGGER.error("Failed to read file: {}. It would be ignored", specPath);
                    continue;
                }

                if (openapiVersion == null) {
                    openapiVersion = result.getOpenapi();
                    if (specRelatedPath.toLowerCase(Locale.ROOT).endsWith(".json")) {
                        isJson = true;
                    }
                }
                allServers.addAll(ObjectUtils.defaultIfNull(result.getServers(), Collections.emptyList()));
                allPaths.add(new SpecWithPaths(specRelatedPath, result.getPaths().keySet()));
            } catch (Exception e) {
                LOGGER.error("Failed to read file: {}. It would be ignored", specPath);
            }
        }

        Map<String, Object> mergedSpec = generateRefMergedSpec(openapiVersion, allPaths, allServers);
        String mergedFilename = this.mergeFileName + (isJson ? ".json" : ".yaml");
        Path mergedFilePath = Paths.get(inputSpecRootDirectory, mergedFilename);

        try {
            ObjectMapper objectMapper = isJson ? JSON_MAPPER : YAML_MAPPER;
            Files.write(mergedFilePath, objectMapper.writeValueAsBytes(mergedSpec), StandardOpenOption.CREATE, StandardOpenOption.WRITE);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

        return mergedFilePath.toString();
    }

    private Map<String, Object> generateRefMergedSpec(String openapiVersion, List<SpecWithPaths> allPaths, List<Server> allServers) {
        Map<String, Object> spec = generateHeader(openapiVersion, mergedFileInfoName, mergedFileInfoDescription, mergedFileInfoVersion, allServers);
        Map<String, Object> paths = new LinkedHashMap<>();
        spec.put("paths", paths);

        for (SpecWithPaths specWithPaths : allPaths) {
            for (String path : specWithPaths.paths) {
                String encodedPath = path.replace("/", "~1");
                paths.put(path, ImmutableMap.of("$ref", "./" + specWithPaths.specRelatedPath.replace('\\', '/') + "#/paths/" + encodedPath));
            }
        }

        return spec;
    }

    private static Map<String, Object> generateHeader(String openapiVersion, String title, String description, String version, List<Server> allServers) {
        Map<String, Object> map = new LinkedHashMap<>();
        map.put("openapi", openapiVersion);
        map.put("info", ImmutableMap.of("title", title, "description", description, "version", version));

        Set<ImmutableMap<String, String>> servers = allServers.stream()
                .map(Server::getUrl)
                .distinct()
                .map(url -> ImmutableMap.of("url", url))
                .collect(Collectors.toCollection(LinkedHashSet::new));
        if (servers.isEmpty()) {
            servers = Collections.singleton(ImmutableMap.of("url", "http://localhost:8080"));
        }
        map.put("servers", servers);
        return map;
    }

    private static class SpecWithPaths {
        private final String specRelatedPath;
        private final Set<String> paths;

        private SpecWithPaths(final String specRelatedPath, final Set<String> paths) {
            this.specRelatedPath = specRelatedPath;
            this.paths = paths;
        }
    }

    // -------------------------------------------------------------------------
    // DEEP mode — full inline merge with component conflict detection
    // -------------------------------------------------------------------------

    private String buildDeepMergedSpec(List<String> specRelatedPaths) {
        boolean isJson = false;
        ParseOptions options = new ParseOptions();
        options.setResolve(true);
        List<OpenAPI> parsedSpecs = new ArrayList<>();
        List<Server> allServers = new ArrayList<>();

        for (String specRelatedPath : specRelatedPaths) {
            String specPath = inputSpecRootDirectory + File.separator + specRelatedPath;
            try {
                LOGGER.info("Reading spec: {}", specPath);

                OpenAPI result = new OpenAPIParser()
                        .readLocation(specPath, AuthParser.parse(auth), options)
                        .getOpenAPI();

                if (result == null) {
                    LOGGER.error("Failed to read file: {}. It would be ignored", specPath);
                    continue;
                }

                if (parsedSpecs.isEmpty() && specRelatedPath.toLowerCase(Locale.ROOT).endsWith(".json")) {
                    isJson = true;
                }
                allServers.addAll(Optional.ofNullable(result.getServers()).orElse(Collections.emptyList()));
                parsedSpecs.add(result);
            } catch (Exception e) {
                LOGGER.error("Failed to read file: {}. It would be ignored", specPath);
            }
        }

        if (parsedSpecs.isEmpty()) {
            throw new RuntimeException("Spec directory doesn't contain any valid specification");
        }

        OpenAPI merged = mergeSpecs(parsedSpecs, allServers);

        String mergedFilename = this.mergeFileName + (isJson ? ".json" : ".yaml");
        Path mergedFilePath = Paths.get(inputSpecRootDirectory, mergedFilename);

        try {
            String content = isJson
                    ? Json.mapper().writerWithDefaultPrettyPrinter().writeValueAsString(merged)
                    : Yaml.mapper().writerWithDefaultPrettyPrinter().writeValueAsString(merged);
            Files.write(mergedFilePath, content.getBytes(StandardCharsets.UTF_8), StandardOpenOption.CREATE, StandardOpenOption.WRITE);
        } catch (JsonProcessingException e) {
            throw new RuntimeException("Failed to serialize merged spec", e);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

        return mergedFilePath.toString();
    }

    /**
     * Merges a list of parsed OpenAPI specs into a single spec.
     *
     * <p>Path items are merged by HTTP method: if two specs define the same URL path, their
     * operations are combined (e.g. GET from one file + POST from another) rather than one
     * overwriting the other. A warning is logged when the same path+method appears in multiple specs;
     * the first occurrence is kept.</p>
     *
     * <p>Component maps (schemas, responses, requestBodies, parameters, headers, examples,
     * links, callbacks, securitySchemes) are merged by name. Structurally identical duplicates
     * are silently deduplicated. A warning is logged if the same component name appears with
     * different definitions; the first definition is kept.</p>
     */
    OpenAPI mergeSpecs(List<OpenAPI> specs, List<Server> allServers) {
        OpenAPI merged = new OpenAPI();
        merged.openapi(specs.get(0).getOpenapi() != null ? specs.get(0).getOpenapi() : "3.0.3");

        Info info = new Info()
                .title(mergedFileInfoName)
                .description(mergedFileInfoDescription)
                .version(mergedFileInfoVersion);
        merged.info(info);

        List<String> distinctServerUrls = allServers.stream()
                .map(Server::getUrl)
                .filter(Objects::nonNull)
                .distinct()
                .collect(Collectors.toList());
        if (distinctServerUrls.isEmpty()) {
            merged.addServersItem(new Server().url("http://localhost:8080"));
        } else {
            distinctServerUrls.forEach(url -> merged.addServersItem(new Server().url(url)));
        }

        merged.setPaths(new io.swagger.v3.oas.models.Paths());
        merged.setComponents(new Components());

        for (OpenAPI spec : specs) {
            if (spec.getPaths() != null) {
                spec.getPaths().forEach((pathKey, incomingPathItem) -> {
                    PathItem existing = merged.getPaths().get(pathKey);
                    if (existing == null) {
                        merged.getPaths().addPathItem(pathKey, incomingPathItem);
                    } else {
                        mergePathItem(existing, incomingPathItem, pathKey);
                    }
                });
            }
            if (spec.getComponents() != null) {
                mergeComponents(merged.getComponents(), spec.getComponents());
            }
        }

        return merged;
    }

    /**
     * Merges HTTP method operations from {@code incoming} into {@code existing} for the same path URL.
     * Path-level metadata (summary, description, servers, parameters, extensions) is kept from
     * {@code existing} (i.e. the first spec that defined this path). A warning is logged for any
     * path+method that already exists in {@code existing}.
     */
    private void mergePathItem(PathItem existing, PathItem incoming, String pathKey) {
        if (incoming.readOperationsMap() == null) {
            return;
        }
        incoming.readOperationsMap().forEach((method, operation) -> {
            if (existing.readOperationsMap() != null && existing.readOperationsMap().containsKey(method)) {
                String message = String.format(Locale.ROOT,
                        "Path+method collision during spec merge: %s %s is defined in multiple specs with different definitions. Keeping the first definition.",
                        method, pathKey);
                if (conflictStrategy == MergeConflictStrategy.FAIL) {
                    throw new RuntimeException(message);
                }
                LOGGER.warn(message);
            } else {
                existing.operation(method, operation);
            }
        });
    }

    /**
     * Merges all component maps from {@code source} into {@code target}.
     * Identical definitions are silently deduplicated. Conflicting definitions (same name, different
     * structure) generate a warning and keep the first definition.
     */
    private void mergeComponents(Components target, Components source) {
        mergeComponentMap(target.getSchemas(), source.getSchemas(), "schema", target::addSchemas);
        mergeComponentMap(target.getResponses(), source.getResponses(), "response", target::addResponses);
        mergeComponentMap(target.getRequestBodies(), source.getRequestBodies(), "requestBody", target::addRequestBodies);
        mergeComponentMap(target.getParameters(), source.getParameters(), "parameter", target::addParameters);
        mergeComponentMap(target.getHeaders(), source.getHeaders(), "header", target::addHeaders);
        mergeComponentMap(target.getExamples(), source.getExamples(), "example", target::addExamples);
        mergeComponentMap(target.getLinks(), source.getLinks(), "link", target::addLinks);
        mergeComponentMap(target.getCallbacks(), source.getCallbacks(), "callback", target::addCallbacks);
        mergeComponentMap(target.getSecuritySchemes(), source.getSecuritySchemes(), "securityScheme", target::addSecuritySchemes);
    }

    private <T> void mergeComponentMap(Map<String, T> existing, Map<String, T> incoming,
                                       String typeName, java.util.function.BiConsumer<String, T> adder) {
        if (incoming == null) {
            return;
        }
        incoming.forEach((name, value) -> {
            if (existing != null && existing.containsKey(name)) {
                if (!Objects.equals(existing.get(name), value)) {
                    String message = String.format(Locale.ROOT,
                            "Component %s name conflict during spec merge: '%s' is defined in multiple specs with different definitions. Keeping the first definition.",
                            typeName, name);
                    if (conflictStrategy == MergeConflictStrategy.FAIL) {
                        throw new RuntimeException(message);
                    }
                    LOGGER.warn(message);
                }
                // identical or keeping first — either way, skip
            } else {
                adder.accept(name, value);
            }
        });
    }

    private List<String> getAllSpecFilesInDirectory() {
        Path rootDirectory = new File(inputSpecRootDirectory).toPath();
        try (Stream<Path> pathStream = Files.walk(rootDirectory)) {
            return pathStream
                    .filter(path -> !Files.isDirectory(path))
                    .filter(path -> {
                        String name = path.getFileName().toString().toLowerCase(Locale.ROOT);
                        return SPEC_EXTENSIONS.stream().anyMatch(name::endsWith);
                    })
                    .map(path -> rootDirectory.relativize(path).toString())
                    .sorted()
                    .collect(Collectors.toList());
        } catch (IOException e) {
            throw new RuntimeException("Exception while listing files in spec root directory: " + inputSpecRootDirectory, e);
        }
    }

    private void deleteMergedFileFromPreviousRun() {
        try {
            Files.deleteIfExists(Paths.get(inputSpecRootDirectory + File.separator + mergeFileName + ".json"));
        } catch (IOException ignored) {
        }
        try {
            Files.deleteIfExists(Paths.get(inputSpecRootDirectory + File.separator + mergeFileName + ".yaml"));
        } catch (IOException ignored) {
        }
    }
}
