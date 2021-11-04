package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.Operation;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Locale;
import java.util.Map;

public class PureCloudPythonClientCodegen extends PythonClientCodegen {
    private static String OPERATION_ID_PROPERTY_NAME = "x-purecloud-method-name";

    protected Logger LOGGER = LoggerFactory.getLogger(PureCloudPythonClientCodegen.class);

    public PureCloudPythonClientCodegen() {
        super();

        // Use default templates
        embeddedTemplateDir = templateDir = "python";

        reservedWords.add("property");
    }


    @Override
    public String getName() {
        return "purecloudpython";
    }

    @Override
    /**
     * Get the operation ID or use default behavior if blank.
     *
     * @param operation the operation object
     * @param path the path of the operation
     * @param httpMethod the HTTP method of the operation
     * @return the (generated) operationId
     */
    protected String getOrGenerateOperationId(Operation operation, String path, String httpMethod) {
        if (operation.getExtensions().containsKey(OPERATION_ID_PROPERTY_NAME)) {
            String operationId = operation.getExtensions().get(OPERATION_ID_PROPERTY_NAME).toString();
            if (!StringUtils.isBlank(operationId)) {
                return operationId;
            }
        }

        return super.getOrGenerateOperationId(operation, path, httpMethod);
    }

    @Override
    public void processOpts() {
        super.processOpts();
    }

    @Override
    public String escapeReservedWord(String name) {
        return "pc" + capitalizeFirstLetter(name);
    }

    private String capitalizeFirstLetter(String s) {
        return s.substring(0, 1).toUpperCase(Locale.getDefault()) + s.substring(1);
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        List<Object> models = (List<Object>) objs.get("models");
        for (Object _mo : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            CodegenModel cm = (CodegenModel) mo.get("model");
            cm.classFilename = cm.classFilename.replaceAll("\\_{1,}$", "_");
        }
        return objs;
    }

    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        super.postProcessParameter(parameter);
        if (parameter.isMap) {
            parameter.dataType = "object";
        }
        if (parameter.dataType.equals("object")) {
            if (isEmpty(parameter.jsonSchema)) {
                parameter.dataType = "Empty";
            }
        }
    }

    private boolean isEmpty(String jsonSchema) {
        return jsonSchema.contains("#/components/schemas/Empty");
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        super.postProcessOperationsWithModels(objs, allModels);
        if (objs != null) {
            Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
            if (operations != null) {
                List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
                for (CodegenOperation operation : ops) {
                    if (operation.returnType != null) {
                        if (isEmpty(operation.responses.get(0).jsonSchema)) {
                            operation.returnType = "Empty";
                        }
                    }
                }
            }
        }

        return objs;
    }

    @Override
    public String toApiFilename(String name) {
        return super.toApiFilename(name).replace("__", "_");
    }

    @Override
    public String toApiVarName(String name) {
        return super.toApiVarName(name).replace("__", "_");
    }

    @Override
    public String toApiName(String name) {
        return super.toApiName(name).replace("__", "_");
    }
}
