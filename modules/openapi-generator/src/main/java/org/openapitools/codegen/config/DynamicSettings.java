package org.openapitools.codegen.config;

import com.fasterxml.jackson.annotation.JsonAnySetter;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonUnwrapped;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import lombok.Getter;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.api.TemplateDefinition;
import org.openapitools.codegen.api.TemplateFileType;

import java.io.IOException;
import java.lang.reflect.Field;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.*;

/**
 * Represents a serialization helper of {@link org.openapitools.codegen.config.GeneratorSettings} and {@link org.openapitools.codegen.config.WorkflowSettings}. When used to deserialize any available Jackson binding input,
 * this will accumulate any "unknown properties" into {@link org.openapitools.codegen.config.GeneratorSettings#getAdditionalProperties()} as a side effect of calling
 * {@link org.openapitools.codegen.config.DynamicSettings#getGeneratorSettings()}.
 */
@SuppressWarnings({"unused", "WeakerAccess"})
public class DynamicSettings {
    /**
     * -- GETTER --
     * Gets all "custom" properties included in the config object.
     *
     * @return All user-specified custom properties.
     */
    @Getter @JsonAnySetter
    private Map<String, Object> dynamicProperties = new HashMap<>();

    @JsonUnwrapped
    @JsonDeserialize(builder = GeneratorSettings.Builder.class)
    private GeneratorSettings generatorSettings;

    @JsonUnwrapped
    @JsonDeserialize(builder = WorkflowSettings.Builder.class)
    private WorkflowSettings workflowSettings;

    /**
     * Gets the list of template files allowing user redefinition and addition of templating files.
     * This includes files from both the {@code files} map and the {@code filesDir} directory.
     * When both are specified, explicit {@code files} entries take precedence over auto-discovered
     * ones from {@code filesDir} if there is a template path conflict.
     *
     * @return A list of template files
     */
    public List<TemplateDefinition> getFiles() {
        // Collect explicitly defined files
        Map<String, TemplateDefinition> result = new LinkedHashMap<>();

        if (files != null) {
            for (Map.Entry<String, TemplateDefinition> kvp : files.entrySet()) {
                TemplateDefinition file = kvp.getValue();
                String templateFile = kvp.getKey();
                String destination = file.getDestinationFilename();
                if (TemplateFileType.SupportingFiles.equals(file.getTemplateType()) && StringUtils.isBlank(destination)) {
                    // this special case allows definitions such as LICENSE:{}
                    destination = templateFile;
                }
                TemplateDefinition definition = new TemplateDefinition(templateFile, file.getFolder(), destination);
                definition.setTemplateType(file.getTemplateType());
                result.put(templateFile, definition);
            }
        }

        // Discover files from filesDir, if specified
        if (StringUtils.isNotBlank(filesDir)) {
            List<TemplateDefinition> discovered = discoverFilesFromDirectory(filesDir);
            for (TemplateDefinition def : discovered) {
                // Explicit files entries take precedence
                result.putIfAbsent(def.getTemplateFile(), def);
            }
        }

        return new ArrayList<>(result.values());
    }

    @SuppressWarnings("MismatchedQueryAndUpdateOfCollection")
    @JsonProperty("files")
    private Map<String, TemplateDefinition> files;

    @JsonProperty("filesDir")
    private String filesDir;

    // Maps subdirectory names (lowercase) to TemplateFileType for auto-discovery
    private static final Map<String, TemplateFileType> DIRECTORY_TYPE_MAPPING;
    static {
        Map<String, TemplateFileType> m = new HashMap<>();
        m.put("api", TemplateFileType.API);
        m.put("model", TemplateFileType.Model);
        m.put("apidocs", TemplateFileType.APIDocs);
        m.put("modeldocs", TemplateFileType.ModelDocs);
        m.put("apitests", TemplateFileType.APITests);
        m.put("modeltests", TemplateFileType.ModelTests);
        m.put("supportingfiles", TemplateFileType.SupportingFiles);
        DIRECTORY_TYPE_MAPPING = Collections.unmodifiableMap(m);
    }

    /**
     * Scans a directory and discovers template files, mapping subdirectory names to template types.
     * <ul>
     *   <li>Files at the root of the directory default to {@link TemplateFileType#SupportingFiles}</li>
     *   <li>Files under subdirectories named after a {@link TemplateFileType} (case-insensitive) are
     *       assigned that type (e.g. {@code api/}, {@code model/}, {@code apiDocs/})</li>
     *   <li>Files under unrecognized subdirectory names are treated as {@link TemplateFileType#SupportingFiles}
     *       with the subdirectory path used as the output folder</li>
     *   <li>For API/Model types, only immediate children of the type directory are included</li>
     *   <li>For SupportingFiles (including root and unrecognized dirs), nesting is preserved as the output folder</li>
     * </ul>
     *
     * @param dirPath path to the directory to scan
     * @return list of discovered template definitions
     */
    static List<TemplateDefinition> discoverFilesFromDirectory(String dirPath) {
        List<TemplateDefinition> result = new ArrayList<>();
        Path baseDir = Paths.get(dirPath).normalize();

        if (!Files.isDirectory(baseDir)) {
            return result;
        }

        try {
            Files.walkFileTree(baseDir, EnumSet.of(FileVisitOption.FOLLOW_LINKS), Integer.MAX_VALUE, new SimpleFileVisitor<Path>() {
                @Override
                public FileVisitResult visitFile(Path filePath, BasicFileAttributes attrs) {
                    if (!Files.isRegularFile(filePath)) {
                        return FileVisitResult.CONTINUE;
                    }

                    Path relative = baseDir.relativize(filePath);
                    String templateFile = relative.toString().replace('\\', '/');
                    String fileName = filePath.getFileName().toString();

                    // Determine the destination filename: strip .mustache suffix if present
                    String destinationFilename = fileName.endsWith(".mustache")
                            ? fileName.substring(0, fileName.length() - ".mustache".length())
                            : fileName;

                    // Determine template type and folder from directory structure
                    TemplateFileType templateType = TemplateFileType.SupportingFiles;
                    String folder = "";

                    if (relative.getNameCount() == 1) {
                        // File at root of filesDir -> SupportingFiles, no folder
                        templateType = TemplateFileType.SupportingFiles;
                    } else {
                        // Has at least one parent directory
                        String topDir = relative.getName(0).toString();
                        String topDirLower = topDir.toLowerCase(Locale.ROOT);
                        TemplateFileType mappedType = DIRECTORY_TYPE_MAPPING.get(topDirLower);

                        if (mappedType != null) {
                            templateType = mappedType;

                            if (mappedType == TemplateFileType.SupportingFiles) {
                                // For supportingFiles subdir, preserve nested structure as folder
                                if (relative.getNameCount() > 2) {
                                    Path folderPath = relative.subpath(1, relative.getNameCount() - 1);
                                    folder = folderPath.toString().replace('\\', '/');
                                }
                            }
                            // For API/Model/etc types, folder is left empty (generator handles placement)
                        } else {
                            // Unrecognized subdirectory -> SupportingFiles with the full relative dir as folder
                            templateType = TemplateFileType.SupportingFiles;
                            Path folderPath = relative.subpath(0, relative.getNameCount() - 1);
                            folder = folderPath.toString().replace('\\', '/');
                        }
                    }

                    TemplateDefinition definition = new TemplateDefinition(templateFile, folder, destinationFilename);
                    definition.setTemplateType(templateType);
                    result.add(definition);

                    return FileVisitResult.CONTINUE;
                }
            });
        } catch (IOException e) {
            throw new RuntimeException("Failed to scan filesDir: " + dirPath, e);
        }

        return result;
    }

    /**
     * Gets the {@link org.openapitools.codegen.config.GeneratorSettings} included in the config object.
     *
     * @return A new instance of settings
     */
    public GeneratorSettings getGeneratorSettings() {
        excludeSettingsFromDynamicProperties();
        GeneratorSettings.Builder builder = GeneratorSettings.newBuilder(generatorSettings);

        // This allows us to put any unknown top-level properties into additionalProperties of the generator object.
        for (Map.Entry<String, Object> entry : dynamicProperties.entrySet()) {
            builder.withAdditionalProperty(entry.getKey(), entry.getValue());
        }

        return builder.build();
    }

    /**
     * Gets the {@link org.openapitools.codegen.config.WorkflowSettings} included in the config object.
     *
     * @return A new instance of settings
     */
    public WorkflowSettings getWorkflowSettings() {
        excludeSettingsFromDynamicProperties();
        return WorkflowSettings.newBuilder(workflowSettings)
                .build();
    }

    /**
     * <p>Constructor for DynamicSettings.</p>
     */
    @JsonCreator
    public DynamicSettings() {
    }

    private void excludeSettingsFromDynamicProperties() {
        Set<String> fieldNames = new HashSet<>();
        for (Field field : GeneratorSettings.class.getDeclaredFields()) {
            fieldNames.add(field.getName());
        }
        for (Field field : WorkflowSettings.class.getDeclaredFields()) {
            fieldNames.add(field.getName());
        }
        dynamicProperties.keySet().removeAll(fieldNames);
    }
}
