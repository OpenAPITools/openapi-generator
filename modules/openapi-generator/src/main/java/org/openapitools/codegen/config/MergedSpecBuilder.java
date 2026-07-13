package org.openapitools.codegen.config;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import com.google.common.collect.ImmutableMap;
import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.parser.OpenAPIResolver;
import io.swagger.v3.core.util.Json;
import io.swagger.v3.core.util.Yaml;
import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.security.SecurityRequirement;
import io.swagger.v3.oas.models.servers.Server;
import io.swagger.v3.oas.models.tags.Tag;
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
     * Controls what happens when a conflict is detected during a {@link MergeMode#DEEP} merge.
     *
     * <p>This strategy is applied consistently to every kind of merge conflict: component/model
     * name clashes (same name, different definition), duplicate HTTP methods on the same path
     * across specs, and duplicate {@code operationId}s. {@link #WARN} keeps the first definition
     * (and, for operationId clashes, renames the later one to keep the document valid);
     * {@link #FAIL} aborts the merge.</p>
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
    /** Absolute paths to individual spec files to merge. Non-null when using list mode. */
    private final List<String> inputSpecFiles;
    /** Output directory for the merged file. Used in list mode instead of inputSpecRootDirectory. */
    private final String outputDirectory;
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
        this.inputSpecFiles = null;
        this.outputDirectory = null;
        this.mergeFileName = mergeFileName;
        this.mergedFileInfoName = mergedFileInfoName;
        this.mergedFileInfoDescription = mergedFileInfoDescription;
        this.mergedFileInfoVersion = mergedFileInfoVersion;
        this.auth = auth;
    }

    /**
     * Constructs a builder that merges an explicit ordered list of spec files rather than
     * scanning a directory. The merged result is written to {@code outputDirectory}.
     *
     * @param inputSpecFiles  absolute paths to the spec files to merge, in merge order
     * @param outputDirectory directory where the merged file will be written
     * @param mergeFileName   base name (without extension) for the merged output file
     */
    public MergedSpecBuilder(final List<String> inputSpecFiles, final String outputDirectory, final String mergeFileName) {
        this(inputSpecFiles, outputDirectory, mergeFileName, "merged spec", "merged spec", "1.0.0", null);
    }

    public MergedSpecBuilder(final List<String> inputSpecFiles, final String outputDirectory, final String mergeFileName,
                             final String mergedFileInfoName, final String mergedFileInfoDescription,
                             final String mergedFileInfoVersion, final String auth) {
        this.inputSpecRootDirectory = null;
        this.inputSpecFiles = new ArrayList<>(inputSpecFiles);
        this.outputDirectory = outputDirectory;
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
     * Sets the strategy used when a conflict is detected during a {@link MergeMode#DEEP} merge —
     * conflicting component names, duplicate HTTP methods on the same path, or duplicate
     * {@code operationId}s. Only applies when {@link MergeMode#DEEP} is active.
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
        if (inputSpecFiles != null) {
            return buildMergedSpecFromList();
        }
        return buildMergedSpecFromDirectory();
    }

    private String buildMergedSpecFromList() {
        if (inputSpecFiles.isEmpty()) {
            throw new RuntimeException("inputSpecFiles list is empty — nothing to merge");
        }
        deleteMergedFileFromPreviousRun();
        LOGGER.info("Merging {} explicit spec files into {}", inputSpecFiles.size(), outputDirectory);
        return buildMergedSpec(inputSpecFiles, outputDirectory);
    }

    private String buildMergedSpecFromDirectory() {
        deleteMergedFileFromPreviousRun();
        List<String> specRelatedPaths = getAllSpecFilesInDirectory();
        if (specRelatedPaths.isEmpty()) {
            throw new RuntimeException("Spec directory doesn't contain any specification");
        }
        LOGGER.info("In spec root directory {} found specs {}", inputSpecRootDirectory, specRelatedPaths);
        // Resolve relative paths to absolute so the shared build logic works uniformly
        List<String> absolutePaths = specRelatedPaths.stream()
                .map(rel -> Paths.get(inputSpecRootDirectory, rel).toAbsolutePath().toString())
                .collect(Collectors.toList());
        return buildMergedSpec(absolutePaths, inputSpecRootDirectory);
    }

    /**
     * Core merge logic shared by both directory mode and list mode.
     *
     * @param absoluteSpecPaths absolute paths to each spec file to parse, in merge order
     * @param outputDir         directory where the merged output file will be written
     */
    private String buildMergedSpec(List<String> absoluteSpecPaths, String outputDir) {
        // In DEEP mode we inline everything into a single self-contained file, so external
        // ($ref to another file) references must be resolved and pulled into components up front;
        // otherwise they would dangle once the merged file is written to a different location.
        ParsedSpecFiles parsed = parseSpecFiles(absoluteSpecPaths, mergeMode == MergeMode.DEEP);
        if (mergeMode == MergeMode.DEEP) {
            return buildDeepMergedSpec(parsed, outputDir);
        }
        return buildRefMergedSpec(parsed, outputDir);
    }

    // -------------------------------------------------------------------------
    // Shared parsing helper
    // -------------------------------------------------------------------------

    /** Holds the results of parsing a set of spec files. */
    private static class ParsedSpecFiles {
        final List<OpenAPI> specs;
        final List<String> absolutePaths; // successfully parsed, in the same order as specs
        final boolean isJson;
        final String openapiVersion;
        final List<Server> allServers;

        ParsedSpecFiles(List<OpenAPI> specs, List<String> absolutePaths, boolean isJson,
                        String openapiVersion, List<Server> allServers) {
            this.specs = specs;
            this.absolutePaths = absolutePaths;
            this.isJson = isJson;
            this.openapiVersion = openapiVersion;
            this.allServers = allServers;
        }
    }

    /**
     * Parses each spec file given as an absolute path.
     *
     * @param absolutePaths        absolute paths to each spec file, in merge order
     * @param resolveExternalRefs  when {@code true} (DEEP mode), external ({@code $ref} to another
     *                             file) references are resolved and pulled into each spec's own
     *                             components so the eventual merged document is self-contained.
     *                             Path-level parameters are intentionally preserved (they are NOT
     *                             pushed down into operations) so {@code mergePathItem} can still
     *                             merge them.
     */
    private ParsedSpecFiles parseSpecFiles(List<String> absolutePaths, boolean resolveExternalRefs) {
        ParseOptions options = new ParseOptions();
        // Do NOT resolve here — with resolve:true the swagger-parser inlines path-level parameters
        // into each operation, making them invisible to mergePathItem. External-ref resolution for
        // DEEP mode is instead performed explicitly below via OpenAPIResolver with
        // addParametersToEachOperation(false), which keeps path-level parameters intact.
        options.setResolve(false);
        List<OpenAPI> specs = new ArrayList<>();
        List<String> parsedPaths = new ArrayList<>();
        List<Server> allServers = new ArrayList<>();
        boolean isJson = false;
        String openapiVersion = null;

        for (String absolutePath : absolutePaths) {
            try {
                LOGGER.info("Reading spec: {}", absolutePath);
                OpenAPI result = new OpenAPIParser()
                        .readLocation(absolutePath, AuthParser.parse(auth), options)
                        .getOpenAPI();
                if (result == null) {
                    LOGGER.error("Failed to read file: {}. It would be ignored", absolutePath);
                    continue;
                }
                if (resolveExternalRefs) {
                    // Resolve external references into this spec's components (rewriting them to
                    // internal '#/components/...' refs) while keeping path-level parameters where
                    // they are, so the merged output does not contain dangling cross-file refs.
                    result = new OpenAPIResolver(result, AuthParser.parse(auth), absolutePath,
                            new OpenAPIResolver.Settings().addParametersToEachOperation(false))
                            .resolve();
                }
                if (specs.isEmpty() && absolutePath.toLowerCase(Locale.ROOT).endsWith(".json")) {
                    isJson = true;
                }
                if (openapiVersion == null) {
                    openapiVersion = result.getOpenapi();
                }
                allServers.addAll(ObjectUtils.defaultIfNull(result.getServers(), Collections.emptyList()));
                specs.add(result);
                parsedPaths.add(absolutePath);
            } catch (Exception e) {
                LOGGER.error("Failed to read file: {}. It would be ignored", absolutePath);
            }
        }

        if (specs.isEmpty()) {
            throw new RuntimeException("No valid specifications found to merge");
        }

        return new ParsedSpecFiles(specs, parsedPaths, isJson, openapiVersion, allServers);
    }

    // -------------------------------------------------------------------------
    // REF mode — original $ref-based shallow merge (identical to master)
    // -------------------------------------------------------------------------

    private String buildRefMergedSpec(ParsedSpecFiles parsed, String outputDir) {
        // Normalize to an absolute path: relativize() requires both paths to be of the same type
        // (both absolute or both relative). The spec paths are always absolute, so a relative
        // outputDir (e.g. a plain directory name passed from the CLI/Gradle) would otherwise throw.
        Path outDirPath = Paths.get(outputDir).toAbsolutePath().normalize();

        List<SpecWithPaths> allPaths = new ArrayList<>();
        for (int i = 0; i < parsed.specs.size(); i++) {
            io.swagger.v3.oas.models.Paths specPaths = parsed.specs.get(i).getPaths();
            Set<String> pathKeys = specPaths != null ? specPaths.keySet() : Collections.emptySet();
            // Compute path relative to the output dir so $ref values are correct regardless of
            // whether the spec files are in the same directory or specified as arbitrary paths.
            String relPath = outDirPath.relativize(Paths.get(parsed.absolutePaths.get(i)).toAbsolutePath().normalize())
                    .toString().replace('\\', '/');
            allPaths.add(new SpecWithPaths(relPath, pathKeys));
        }

        Map<String, Object> mergedSpec = generateRefMergedSpec(parsed.openapiVersion, allPaths, parsed.allServers);
        String mergedFilename = this.mergeFileName + (parsed.isJson ? ".json" : ".yaml");
        Path mergedFilePath = outDirPath.resolve(mergedFilename);

        try {
            Files.createDirectories(outDirPath);
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

    private String buildDeepMergedSpec(ParsedSpecFiles parsed, String outputDir) {
        OpenAPI merged = mergeSpecs(parsed.specs, parsed.allServers);

        String mergedFilename = this.mergeFileName + (parsed.isJson ? ".json" : ".yaml");
        Path mergedFilePath = Paths.get(outputDir, mergedFilename);

        try {
            Files.createDirectories(mergedFilePath.getParent());
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
     * operations are combined (e.g. GET from one file + POST from another). Duplicate HTTP methods
     * on the same path, conflicting component definitions, and duplicate {@code operationId}s are
     * all treated as conflicts and handled according to the configured {@link MergeConflictStrategy}
     * ({@link MergeConflictStrategy#WARN} keeps the first definition; {@link MergeConflictStrategy#FAIL}
     * aborts).</p>
     *
     * <p>Component maps (schemas, responses, requestBodies, parameters, headers, examples,
     * links, callbacks, securitySchemes and OpenAPI 3.1 pathItems) are merged by name. Structurally
     * identical duplicates are silently deduplicated. A warning (or exception in FAIL mode) is
     * raised when the same component name appears with different definitions; the first definition
     * is kept.</p>
     *
     * <p>Root-level {@code security} from each source spec is propagated onto its operations before
     * merging so API-wide authorization is preserved. Root-level {@code tags}, {@code externalDocs}
     * and OpenAPI 3.1 {@code webhooks} are also carried over (first definition wins on collisions).
     * Top-level {@code x-} vendor extensions are merged from all specs; the first definition wins on
     * key conflicts.</p>
     */
    OpenAPI mergeSpecs(List<OpenAPI> specs, List<Server> allServers) {
        OpenAPI merged = new OpenAPI();
        merged.openapi(resolveOutputVersion(specs));

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
            // Root-level security applies to every operation that does not declare its own. Since
            // the merged document has no single root security that can represent multiple sources,
            // push each spec's root-level security down onto its own operations first so their
            // effective authorization is preserved after merging.
            propagateRootSecurityToOperations(spec);
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
            // Merge root-level tags (dedup by name, first definition wins)
            if (spec.getTags() != null) {
                for (Tag tag : spec.getTags()) {
                    boolean present = merged.getTags() != null && merged.getTags().stream()
                            .anyMatch(t -> Objects.equals(t.getName(), tag.getName()));
                    if (!present) {
                        merged.addTagsItem(tag);
                    }
                }
            }
            // Root-level external documentation: keep the first one encountered
            if (merged.getExternalDocs() == null && spec.getExternalDocs() != null) {
                merged.setExternalDocs(spec.getExternalDocs());
            }
            // Merge OpenAPI 3.1 webhooks by name using the same conflict handling as components
            if (spec.getWebhooks() != null) {
                mergeComponentMap(merged.getWebhooks(), spec.getWebhooks(), "webhook", merged::addWebhooks);
            }
        }

        resolveOperationIdConflicts(merged);
        return merged;
    }

    /**
     * Determines the OpenAPI version to declare on the merged document, and rejects incompatible
     * inputs.
     *
     * <p>3.0 and 3.1 differ in structural ways (e.g. {@code webhooks} and
     * {@code components.pathItems} only exist in 3.1, and {@code nullable} /
     * {@code exclusiveMinimum} change meaning), and the merge does not translate between them.
     * Silently combining them would produce a document that is invalid or subtly wrong. All source
     * specs must therefore share the same major.minor version; otherwise the merge fails fast.</p>
     *
     * <p>Patch-level differences (e.g. {@code 3.0.1} vs {@code 3.0.3}) are compatible; the highest
     * patch version encountered is used for the merged document.</p>
     *
     * @throws RuntimeException if the source specs declare different major.minor OpenAPI versions
     */
    private String resolveOutputVersion(List<OpenAPI> specs) {
        List<String> versions = specs.stream()
                .map(OpenAPI::getOpenapi)
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
        if (versions.isEmpty()) {
            return "3.0.3";
        }
        String firstMajorMinor = majorMinor(versions.get(0));
        for (String version : versions) {
            if (!majorMinor(version).equals(firstMajorMinor)) {
                throw new RuntimeException(String.format(Locale.ROOT,
                        "Cannot merge specs that declare incompatible OpenAPI versions %s. All specs must share "
                                + "the same major.minor version (e.g. all 3.0.x or all 3.1.x); mixing 3.0 and 3.1 "
                                + "is not supported because version-specific fields and semantics are not translated.",
                        versions.stream().distinct().collect(Collectors.toList())));
            }
        }
        // Same major.minor across all specs — declare the highest patch version encountered.
        String highest = versions.get(0);
        for (String version : versions) {
            if (compareVersions(version, highest) > 0) {
                highest = version;
            }
        }
        return highest;
    }

    /** Returns the {@code major.minor} portion of a dot-separated version string (e.g. "3.1"). */
    private String majorMinor(String version) {
        String[] parts = version.split("\\.");
        return parseVersionSegment(parts, 0) + "." + parseVersionSegment(parts, 1);
    }

    /**
     * Compares two dot-separated OpenAPI version strings (e.g. {@code "3.0.3"} vs {@code "3.1.0"})
     * numerically, segment by segment. Non-numeric or missing segments are treated as {@code 0}.
     *
     * @return a negative number, zero, or a positive number if {@code a} is lower than, equal to,
     *         or higher than {@code b}
     */
    private int compareVersions(String a, String b) {
        String[] aParts = a.split("\\.");
        String[] bParts = b.split("\\.");
        int length = Math.max(aParts.length, bParts.length);
        for (int i = 0; i < length; i++) {
            int aValue = parseVersionSegment(aParts, i);
            int bValue = parseVersionSegment(bParts, i);
            if (aValue != bValue) {
                return Integer.compare(aValue, bValue);
            }
        }
        return 0;
    }

    private int parseVersionSegment(String[] parts, int index) {
        if (index >= parts.length) {
            return 0;
        }
        try {
            return Integer.parseInt(parts[index].trim());
        } catch (NumberFormatException e) {
            return 0;
        }
    }

    /**
     * Detects operationId collisions across the whole merged document. Two operations on different
     * path/method combinations sharing the same {@code operationId} produce an invalid OpenAPI
     * document and can collide in generated code. Iterating paths in insertion order, the first
     * occurrence of each id is kept; a later collision is handled per {@link MergeConflictStrategy}:
     * {@link MergeConflictStrategy#FAIL} aborts, while {@link MergeConflictStrategy#WARN} (default)
     * logs a warning and renames the later operation by appending a numeric suffix (e.g. {@code _2})
     * until it is unique.
     */
    private void resolveOperationIdConflicts(OpenAPI merged) {
        if (merged.getPaths() == null) {
            return;
        }
        Set<String> usedOperationIds = new HashSet<>();
        for (Map.Entry<String, PathItem> pathEntry : merged.getPaths().entrySet()) {
            PathItem pathItem = pathEntry.getValue();
            if (pathItem == null || pathItem.readOperationsMap() == null) {
                continue;
            }
            for (Map.Entry<PathItem.HttpMethod, Operation> opEntry : pathItem.readOperationsMap().entrySet()) {
                Operation operation = opEntry.getValue();
                String operationId = operation.getOperationId();
                if (operationId == null || operationId.isEmpty()) {
                    continue;
                }
                if (!usedOperationIds.add(operationId)) {
                    String message = String.format(Locale.ROOT,
                            "operationId conflict during spec merge: '%s' (%s %s) is already used by another operation.",
                            operationId, opEntry.getKey(), pathEntry.getKey());
                    if (conflictStrategy == MergeConflictStrategy.FAIL) {
                        throw new RuntimeException(message);
                    }
                    String uniqueId = operationId;
                    int suffix = 2;
                    while (!usedOperationIds.add(uniqueId + "_" + suffix)) {
                        suffix++;
                    }
                    uniqueId = uniqueId + "_" + suffix;
                    LOGGER.warn("{} Renaming to '{}'.", message, uniqueId);
                    operation.setOperationId(uniqueId);
                }
            }
        }
    }

    /**
     * Pushes a spec's root-level security requirements down onto each of its operations that does
     * not already declare operation-level security. In OpenAPI, root-level {@code security} applies
     * to every operation unless overridden; an explicit empty list on an operation disables security.
     * Because the merged document combines specs that may each have different (or no) root-level
     * security, we cannot keep a single merged root security. Propagating to operations preserves
     * the effective authorization of every operation regardless of which spec it came from.
     *
     * <p>Operations that already declare their own {@code security} (including an explicit empty
     * list meaning "no auth") are left untouched.</p>
     */
    private void propagateRootSecurityToOperations(OpenAPI spec) {
        List<SecurityRequirement> rootSecurity = spec.getSecurity();
        if (rootSecurity == null || rootSecurity.isEmpty() || spec.getPaths() == null) {
            return;
        }
        spec.getPaths().values().forEach(pathItem -> {
            if (pathItem == null || pathItem.readOperations() == null) {
                return;
            }
            for (Operation operation : pathItem.readOperations()) {
                if (operation.getSecurity() == null) {
                    operation.setSecurity(new ArrayList<>(rootSecurity));
                }
            }
        });
    }

    /**
     * Merges HTTP method operations from {@code incoming} into {@code existing} for the same path URL.
     *
     * <p>Operations for methods not yet present in {@code existing} are added. If the same HTTP
     * method already exists on a path, the configured {@link MergeConflictStrategy} is applied:
     * {@link MergeConflictStrategy#FAIL} throws a {@link RuntimeException} and aborts, while
     * {@link MergeConflictStrategy#WARN} (default) logs a warning and keeps the first definition.
     * There is no valid use case for two spec files to both define the same HTTP method on the same
     * path, so this is treated as a conflict just like a component name clash.</p>
     *
     * <p>Path-level metadata from {@code incoming} is merged into {@code existing}:</p>
     * <ul>
     *   <li>Parameters: added if not already present (identity is the {@code $ref} when present,
     *       otherwise name+in); first wins on conflict</li>
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
                String message = String.format(Locale.ROOT,
                        "Path+method conflict during spec merge: %s %s is defined in multiple specs. " +
                        "Unlike schema reuse, duplicate HTTP methods on the same path are not valid — " +
                        "check that your spec files do not overlap. Keeping the first definition.",
                        method, pathKey);
                if (conflictStrategy == MergeConflictStrategy.FAIL) {
                    throw new RuntimeException(message);
                }
                LOGGER.warn(message);
                // WARN: keep the first (existing) operation, skip the incoming one.
                return;
            }
            existing.operation(method, operation);
        });

        // Merge path-level parameters (first wins on conflict). Identity is the $ref value when the
        // parameter is a reference; otherwise name+in. Without this, multiple distinct $ref
        // parameters would all collapse to the key "null:null" and all but the first would be lost.
        if (incoming.getParameters() != null) {
            List<Parameter> merged = existing.getParameters() != null
                    ? new ArrayList<>(existing.getParameters()) : new ArrayList<>();
            Set<String> existingKeys = merged.stream()
                    .map(MergedSpecBuilder::parameterIdentity)
                    .collect(Collectors.toSet());
            for (Parameter p : incoming.getParameters()) {
                if (existingKeys.add(parameterIdentity(p))) {
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
     * Identity used to deduplicate parameters during path-level merging. A {@code $ref} parameter
     * has no {@code name}/{@code in} of its own, so those would all collide as {@code "null:null"};
     * use the reference value as its identity instead. Inline parameters fall back to name+in.
     */
    private static String parameterIdentity(Parameter p) {
        if (p.get$ref() != null) {
            return "$ref:" + p.get$ref();
        }
        return p.getName() + ":" + p.getIn();
    }

    /**
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
        // OpenAPI 3.1 reusable path items — without this, operations referencing
        // '#/components/pathItems/...' would dangle in the merged output.
        mergeComponentMap(target.getPathItems(), source.getPathItems(), "pathItem", target::addPathItem);
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
        String targetDir = (outputDirectory != null) ? outputDirectory : inputSpecRootDirectory;
        try {
            Files.deleteIfExists(Paths.get(targetDir + File.separator + mergeFileName + ".json"));
        } catch (IOException ignored) {
        }
        try {
            Files.deleteIfExists(Paths.get(targetDir + File.separator + mergeFileName + ".yaml"));
        } catch (IOException ignored) {
        }
    }
}
