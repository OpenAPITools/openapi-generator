package org.openapitools.codegen.config;

import com.fasterxml.jackson.annotation.JsonAnySetter;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonUnwrapped;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import org.apache.commons.lang3.StringUtils;

import java.lang.reflect.Field;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Represents a serialization helper of {@link GeneratorSettings} and {@link WorkflowSettings}. When used to deserialize any available Jackson binding input,
 * this will accumulate any "unknown properties" into {@link GeneratorSettings#getAdditionalProperties()} as a side effect of calling
 * {@link DynamicSettings#getGeneratorSettings()}.
 */
@SuppressWarnings({"unused", "WeakerAccess"})
public class DynamicSettings {
    @JsonAnySetter
    private Map<String, Object> dynamicProperties = new HashMap<>();

    @JsonUnwrapped
    @JsonDeserialize(builder = GeneratorSettings.Builder.class)
    private GeneratorSettings generatorSettings;

    @JsonUnwrapped
    @JsonDeserialize(builder = WorkflowSettings.Builder.class)
    private WorkflowSettings workflowSettings;

    /**
     * Gets the {@link TemplateFile} allowing user redefinition and addition of templating files
     * @return A lis
     */
    public List<TemplateFile> getFiles() {
        if (files == null) return new ArrayList<>();

        return files.entrySet().stream().map(kvp -> {
            TemplateFile file = kvp.getValue();
            String templateFile = kvp.getKey();
            String destination = file.getDestinationFilename();
            if (TemplateFileType.SupportingFiles.equals(file.getTemplateType()) && StringUtils.isBlank(destination)) {
                // this special case allows definitions such as LICENSE:{}
                destination = templateFile;
            }
            return new TemplateFile(templateFile, file.getFolder(), destination, file.getTemplateType());
        }).collect(Collectors.toList());
    }

    @JsonProperty("files")
    private Map<String, TemplateFile> files;

    /**
     * Gets the {@link GeneratorSettings} included in the config object.
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
     * Gets the {@link WorkflowSettings} included in the config object.
     *
     * @return A new instance of settings
     */
    public WorkflowSettings getWorkflowSettings() {
        excludeSettingsFromDynamicProperties();
        return WorkflowSettings.newBuilder(workflowSettings)
                .build();
    }

    @JsonCreator
    public DynamicSettings() { }

    /**
     * Gets all "custom" properties included in the config object.
     *
     * @return All user-specified custom properties.
     */
    public Map<String, Object> getDynamicProperties() {
        return dynamicProperties;
    }

    private void excludeSettingsFromDynamicProperties(){
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
