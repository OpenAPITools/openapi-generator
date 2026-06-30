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
import io.swagger.v3.oas.models.parameters.Parameter;
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
     * with different definitions during a {@link MergeMode#DEEP} merge.
     *
     * <p>This strategy applies to component/model conflicts only. Path+method overlaps (the same
     * HTTP method on the same path in multiple specs) are always silently resolved by keeping the
     * first definition, regardless of this setting.</p>
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
     * Sets the strategy used when two specs define the same component name (schema, response, etc.)
     * with conflicting definitions. Only applies when {@link MergeMode#DEEP} is active.
     *
     * <p>Path+method overlaps are unaffected by this setting — they are always silently resolved
     * by keeping the first definition.</p>
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
    // Shared parsing helper
    // -------------------------------------------------------------------------

    /** Holds the results of parsing all spec files in a directory. */
    private static class ParsedSpecFiles {
        final List<OpenAPI> specs;
        final List<String> relativePaths; // successfully parsed, in the same order as specs
        final boolean isJson;
        final String openapiVersion;
        final List<Server> allServers;

        ParsedSpecFiles(List<OpenAPI> specs, List<String> relativePaths, boolean isJson,
                        String openapiVersion, List<Server> allServers) {
            this.specs = specs;
            this.relativePaths = relativePaths;
            this.isJson = isJson;
            this.openapiVersion = openapiVersion;
            this.allServers = allServers;
        }
    }

    private ParsedSpecFiles parseSpecFiles(List<String> specRelatedPaths) {
        ParseOptions options = new ParseOptions();
        options.setResolve(true);
        List<OpenAPI> specs = new ArrayList<>();
        List<String> relativePaths = new ArrayList<>();
        List<Server> allServers = new ArrayList<>();
        boolean isJson = false;
        String openapiVersion = null;

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
                if (specs.isEmpty() && specRelatedPath.toLowerCase(Locale.ROOT).endsWith(".json")) {
                    isJson = true;
                }
                if (openapiVersion == null) {
                    openapiVersion = result.getOpenapi();
                }
                allServers.addAll(ObjectUtils.defaultIfNull(result.getServers(), Collections.emptyList()));
                specs.add(result);
                relativePaths.add(specRelatedPath);
            } catch (Exception e) {
                LOGGER.error("Failed to read file: {}. It would be ignored", specPath);
            }
        }

        if (specs.isEmpty()) {
            throw new RuntimeException("Spec directory doesn't contain any valid specification");
        }

        return new ParsedSpecFiles(specs, relativePaths, isJson, openapiVersion, allServers);
    }

    // -------------------------------------------------------------------------
    // REF mode — original $ref-based shallow merge (identical to master)
    // -------------------------------------------------------------------------

    private String buildRefMergedSpec(List<String> specRelatedPaths) {
        ParsedSpecFiles parsed = parseSpecFiles(specRelatedPaths);

        List<SpecWithPaths> allPaths = new ArrayList<>();
        for (int i = 0; i < parsed.specs.size(); i++) {
            io.swagger.v3.oas.models.Paths specPaths = parsed.specs.get(i).getPaths();
            Set<String> pathKeys = specPaths != null ? specPaths.keySet() : Collections.emptySet();
            allPaths.add(new SpecWithPaths(parsed.relativePaths.get(i), pathKeys));
        }

        Map<String, Object> mergedSpec = generateRefMergedSpec(parsed.openapiVersion, allPaths, parsed.allServers);
        String mergedFilename = this.mergeFileName + (parsed.isJson ? ".json" : ".yaml");
        Path mergedFilePath = Paths.get(inputSpecRootDirectory, mergedFilename);

        try {
            ObjectMapper objectMapper = parsed.isJson ? JSON_MAPPER : YAML_MAPPER;
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
        ParsedSpecFiles parsed = parseSpecFiles(specRelatedPaths);

        OpenAPI merged = mergeSpecs(parsed.specs, parsed.allServers);

        String mergedFilename = this.mergeFileName + (parsed.isJson ? ".json" : ".yaml");
        Path mergedFilePath = Paths.get(inputSpecRootDirectory, mergedFilename);

        try {
            String content = parsed.isJson
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
     * operations are combined (e.g. GET from one file + POST from another). If the same HTTP
     * method already exists on a path, a {@link RuntimeException} is thrown — this is always
     * a configuration error with no valid use case.</p>
     *
     * <p>Component maps (schemas, responses, requestBodies, parameters, headers, examples,
     * links, callbacks, securitySchemes) are merged by name. Structurally identical duplicates
     * are silently deduplicated. A warning (or exception in FAIL mode) is raised when the same
     * component name appears with different definitions; the first definition is kept.</p>
     *
     * <p>Top-level {@code x-} vendor extensions are merged from all specs; the first definition
     * wins on key conflicts.</p>
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
            // Merge top-level x- vendor extensions (keep first on key conflict)
            if (spec.getExtensions() != null) {
                spec.getExtensions().forEach((key, value) -> {
                    if (merged.getExtensions() == null || !merged.getExtensions().containsKey(key)) {
                        merged.addExtension(key, value);
                    }
                });
            }
        }

        return merged;
    }

    /**
     * Merges HTTP method operations from {@code incoming} into {@code existing} for the same path URL.
     *
     * <p>Operations for methods not yet present in {@code existing} are added. If the same HTTP
     * method already exists on a path, a {@link RuntimeException} is always thrown — even if the
     * two definitions are identical. Unlike schema reuse (where identical duplicates are silently
     * deduplicated), there is no valid reason for two spec files to both define the same HTTP method
     * on the same path. An identical duplicate is still almost certainly a configuration error
     * (e.g. the same file included twice, or an accidental copy), and failing loudly is safer than
     * silently discarding one.</p>
     *
     * <p>Path-level metadata from {@code incoming} is merged into {@code existing}:</p>
     * <ul>
     *   <li>Parameters: added if not already present by name+in (first wins on conflict)</li>
     *   <li>Servers: added if not already present by URL</li>
     *   <li>Extensions: added if not already present by key</li>
     *   <li>Summary and description: always kept from the first ({@code existing}) PathItem</li>
     * </ul>
     */
    private void mergePathItem(PathItem existing, PathItem incoming, String pathKey) {
        if (incoming.readOperationsMap() == null) {
            return;
        }
        incoming.readOperationsMap().forEach((method, operation) -> {
            if (existing.readOperationsMap() != null && existing.readOperationsMap().containsKey(method)) {
                throw new RuntimeException(String.format(Locale.ROOT,
                        "Path+method conflict during spec merge: %s %s is defined in multiple specs. " +
                        "Unlike schema reuse, duplicate HTTP methods on the same path are not valid — " +
                        "check that your spec files do not overlap.",
                        method, pathKey));
            }
            existing.operation(method, operation);
        });

        // Merge path-level parameters (deduplicate by name+in, first wins)
        if (incoming.getParameters() != null) {
            List<Parameter> merged = existing.getParameters() != null
                    ? new ArrayList<>(existing.getParameters()) : new ArrayList<>();
            Set<String> existingKeys = merged.stream()
                    .map(p -> p.getName() + ":" + p.getIn())
                    .collect(Collectors.toSet());
            for (Parameter p : incoming.getParameters()) {
                if (existingKeys.add(p.getName() + ":" + p.getIn())) {
                    merged.add(p);
                }
            }
            existing.setParameters(merged);
        }

        // Merge path-level servers (deduplicate by URL)
        if (incoming.getServers() != null) {
            List<Server> merged = existing.getServers() != null
                    ? new ArrayList<>(existing.getServers()) : new ArrayList<>();
            Set<String> existingUrls = merged.stream()
                    .map(Server::getUrl).filter(Objects::nonNull).collect(Collectors.toSet());
            for (Server s : incoming.getServers()) {
                if (s.getUrl() == null || existingUrls.add(s.getUrl())) {
                    merged.add(s);
                }
            }
            existing.setServers(merged);
        }

        // Merge path-level extensions (first wins on key conflict)
        if (incoming.getExtensions() != null) {
            incoming.getExtensions().forEach((key, value) -> {
                if (existing.getExtensions() == null || !existing.getExtensions().containsKey(key)) {
                    existing.addExtension(key, value);
                }
            });
        }
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
