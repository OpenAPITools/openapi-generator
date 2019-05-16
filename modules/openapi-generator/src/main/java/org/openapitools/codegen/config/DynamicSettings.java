package org.openapitools.codegen.config;

import com.fasterxml.jackson.annotation.JsonAnySetter;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonUnwrapped;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;

import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * Represents a serialization helper of {@link GeneratorSettings}. When used to deserialize any available Jackson binding input,
 * this will accumulate any "unknown properties" into {@link GeneratorSettings#getAdditionalProperties()} as a side effect of calling
 * {@link DynamicSettings#getGeneratorSettings()}.
 */
@SuppressWarnings("unused")
public class DynamicSettings {
    @JsonAnySetter
    private Map<String, Object> dynamicProperties = new HashMap<>();

    @JsonUnwrapped
    @JsonDeserialize(builder = GeneratorSettings.Builder.class)
    private GeneratorSettings generatorSettings;

    public GeneratorSettings getGeneratorSettings() {
        Set<String> fieldNames = new HashSet<>();
        Field[] fields = GeneratorSettings.class.getDeclaredFields();
        for (Field field : fields) {
            fieldNames.add(field.getName());
        }
        dynamicProperties.keySet().removeAll(fieldNames);

        return GeneratorSettings.newBuilder(generatorSettings)
                .withAdditionalProperties(dynamicProperties)
                .build();
    }

    @JsonCreator
    public DynamicSettings() { }

    public Map<String, Object> getDynamicProperties() {
        return dynamicProperties;
    }
}
