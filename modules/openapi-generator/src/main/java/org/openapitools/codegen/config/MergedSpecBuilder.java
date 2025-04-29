package org.openapitools.codegen.config;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import com.google.common.collect.ImmutableMap;
import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.parser.core.models.ParseOptions;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class MergedSpecBuilder {

    private static final Logger LOGGER = LoggerFactory.getLogger(MergedSpecBuilder.class);

    private final String inputSpecRootDirectory;
    private final String mergeFileName;
    private final String mergedFileInfoName;
    private final String mergedFileInfoDescription;
    private final String mergedFileInfoVersion;

    public MergedSpecBuilder(final String rootDirectory, final String mergeFileName) {
        this(rootDirectory, mergeFileName, "merged spec", "merged spec", "1.0.0");
    }

    public MergedSpecBuilder(final String rootDirectory, final String mergeFileName,
                             final String mergedFileInfoName, final String mergedFileInfoDescription, final String mergedFileInfoVersion) {
        this.inputSpecRootDirectory = rootDirectory;
        this.mergeFileName = mergeFileName;
        this.mergedFileInfoName = mergedFileInfoName;
        this.mergedFileInfoDescription = mergedFileInfoDescription;
        this.mergedFileInfoVersion = mergedFileInfoVersion;
    }

    public String buildMergedSpec() {
        deleteMergedFileFromPreviousRun();
        List<String> specRelatedPaths = getAllSpecFilesInDirectory();
        if (specRelatedPaths.isEmpty()) {
            throw new RuntimeException("Spec directory doesn't contain any specification");
        }
        LOGGER.info("In spec root directory {} found specs {}", inputSpecRootDirectory, specRelatedPaths);

        String openapiVersion = null;
        boolean isJson = false;
        ParseOptions options = new ParseOptions();
        options.setResolve(true);
        List<SpecWithPaths> allPaths = new ArrayList<>();

        for (String specRelatedPath : specRelatedPaths) {
            String specPath = inputSpecRootDirectory + File.separator + specRelatedPath;
            try {
                LOGGER.info("Reading spec: {}", specPath);

                OpenAPI result = new OpenAPIParser()
                        .readLocation(specPath, new ArrayList<>(), options)
                        .getOpenAPI();

                if (openapiVersion == null) {
                    openapiVersion = result.getOpenapi();
                    if (specRelatedPath.toLowerCase(Locale.ROOT).endsWith(".json")) {
                        isJson = true;
                    }
                }
                allPaths.add(new SpecWithPaths(specRelatedPath, result.getPaths().keySet()));
            } catch (Exception e) {
                LOGGER.error("Failed to read file: {}. It would be ignored", specPath);
            }
        }

        Map<String, Object> mergedSpec = generatedMergedSpec(openapiVersion, allPaths);
        String mergedFilename = this.mergeFileName + (isJson ? ".json" : ".yaml");
        Path mergedFilePath = Paths.get(inputSpecRootDirectory, mergedFilename);

        try {
            ObjectMapper objectMapper = isJson ? new ObjectMapper() : new ObjectMapper(new YAMLFactory());
            Files.write(mergedFilePath, objectMapper.writeValueAsBytes(mergedSpec), StandardOpenOption.CREATE, StandardOpenOption.WRITE);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

        return mergedFilePath.toString();
    }

    private Map<String, Object> generatedMergedSpec(String openapiVersion, List<SpecWithPaths> allPaths) {
        Map<String, Object> spec = generateHeader(openapiVersion, mergedFileInfoName, mergedFileInfoDescription, mergedFileInfoVersion);
        Map<String, Object> paths = new HashMap<>();
        spec.put("paths", paths);

        for (SpecWithPaths specWithPaths : allPaths) {
            for (String path : specWithPaths.paths) {
                String specRelatedPath = "./" + specWithPaths.specRelatedPath + "#/paths/" + path.replace("/", "~1");
                paths.put(path, ImmutableMap.of(
                        "$ref", specRelatedPath
                ));
            }
        }

        return spec;
    }

    private static Map<String, Object> generateHeader(String openapiVersion, String title, String description, String version) {
        Map<String, Object> map = new HashMap<>();
        map.put("openapi", openapiVersion);
        map.put("info", ImmutableMap.of(
                "title", title,
                "description", description,
                "version", version
        ));
        map.put("servers", Collections.singleton(
                ImmutableMap.of("url", "http://localhost:8080")
        ));
        return map;
    }

    private List<String> getAllSpecFilesInDirectory() {
        Path rootDirectory = new File(inputSpecRootDirectory).toPath();
        try (Stream<Path> pathStream = Files.walk(rootDirectory)) {
            return pathStream
                    .filter(path -> !Files.isDirectory(path))
                    .map(path -> rootDirectory.relativize(path).toString())
                    .collect(Collectors.toList());
        } catch (IOException e) {
            throw new RuntimeException("Exception while listing files in spec root directory: " + inputSpecRootDirectory, e);
        }
    }

    private void deleteMergedFileFromPreviousRun() {
        try {
            Files.deleteIfExists(Paths.get(inputSpecRootDirectory + File.separator + mergeFileName + ".json"));
        } catch (IOException e) {
        }
        try {
            Files.deleteIfExists(Paths.get(inputSpecRootDirectory + File.separator + mergeFileName + ".yaml"));
        } catch (IOException e) {
        }
    }

    private static class SpecWithPaths {
        private final String specRelatedPath;
        private final Set<String> paths;

        private SpecWithPaths(final String specRelatedPath, final Set<String> paths) {
            this.specRelatedPath = specRelatedPath;
            this.paths = paths;
        }
    }
}
