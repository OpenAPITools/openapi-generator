package io.swagger.codegen.utils;

import io.swagger.codegen.CodegenModel;

import java.util.List;
import java.util.Map;

public class ModelUtils {
    /**
     * Searches for the model by name in the map of models and returns it
     *
     * @param name Name of the model
     * @param models Map of models
     * @return model
     */
    public static CodegenModel getModelByName(final String name, final Map<String, Object> models) {
        final Object data = models.get(name);
        if (data instanceof Map) {
            final Map<?, ?> dataMap = (Map<?, ?>) data;
            final Object dataModels = dataMap.get("models");
            if (dataModels instanceof List) {
                final List<?> dataModelsList = (List<?>) dataModels;
                for (final Object entry : dataModelsList) {
                    if (entry instanceof Map) {
                        final Map<?, ?> entryMap = (Map<?, ?>) entry;
                        final Object model = entryMap.get("model");
                        if (model instanceof CodegenModel) {
                            return (CodegenModel) model;
                        }
                    }
                }
            }
        }
        return null;
    }
}
