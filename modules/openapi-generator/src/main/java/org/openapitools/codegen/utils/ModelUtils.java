package org.openapitools.codegen.utils;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.media.MediaType;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.RequestBody;
import org.openapitools.codegen.CodegenModel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;


public class ModelUtils {
    static Logger LOGGER = LoggerFactory.getLogger(ModelUtils.class);

    /**
     * Searches for the model by name in the map of models and returns it
     *
     * @param name   Name of the model
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

    public static List<String> getUnusedSchemas(OpenAPI openAPI) {
        List<String> unusedSchemas = new ArrayList<String>();

        // no model defined
        if (openAPI.getComponents().getSchemas() == null) {
            openAPI.getComponents().setSchemas(new HashMap<String, Schema>());
        }

        // operations
        Map<String, PathItem> paths = openAPI.getPaths();
        Map<String, Schema> schemas = openAPI.getComponents().getSchemas();

        if (paths != null) {
            for (String pathname : paths.keySet()) {
                PathItem path = paths.get(pathname);
                Map<PathItem.HttpMethod, Operation> operationMap = path.readOperationsMap();
                if (operationMap != null) {
                    for (PathItem.HttpMethod method : operationMap.keySet()) {
                        Operation operation = operationMap.get(method);
                        RequestBody requestBody = operation.getRequestBody();

                        if (requestBody == null) {
                            continue;
                        }

                        //LOGGER.info("debugging resolver: " + requestBody.toString());
                        if (requestBody.getContent() == null) {
                            continue;
                        }

                        // go through "content"
                        for (String mimeType : requestBody.getContent().keySet()) {
                            if ("application/x-www-form-urlencoded".equalsIgnoreCase(mimeType) ||
                                    "multipart/form-data".equalsIgnoreCase(mimeType)) {
                                // remove the schema that's automatically created by the parser
                                MediaType mediaType = requestBody.getContent().get(mimeType);
                                if (mediaType.getSchema().get$ref() != null) {
                                    LOGGER.debug("mark schema (form parameters) as unused: " + getSimpleRef(mediaType.getSchema().get$ref()));
                                    unusedSchemas.add(getSimpleRef(mediaType.getSchema().get$ref()));
                                }
                            }
                        }
                    }
                }
            }
        }

        return unusedSchemas;
    }

    protected static String getSimpleRef(String ref) {
        if (ref.startsWith("#/components/")) {
            ref = ref.substring(ref.lastIndexOf("/") + 1);
        } else if (ref.startsWith("#/definitions/")) {
            ref = ref.substring(ref.lastIndexOf("/") + 1);
        }

        return ref;
    }
}
