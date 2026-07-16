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
        apiDocTemplateFiles.put("api_json.mustache", ".json");
        operationTemplateFiles.put("operation_example.mustache", "-example.txt");
        typeMapping.put("file", "str");
        supportingFiles.add(new SupportingFile("pyproject.mustache", "", "pyproject.toml"));
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
            // Determine the python "type hinting" data type for model properties
            List<CodegenProperty> vars = cm.vars;
            if (vars != null) {
                for (CodegenProperty cp : vars) {
                    if (cp.dataType.contains("list[")) {
                        String item = cp.dataType.substring(cp.dataType.indexOf('[')+1, cp.dataType.indexOf(']'));
                        if (item.contains("list[")) {
                            item = cp.dataType.substring(cp.dataType.indexOf('[')+1, cp.dataType.indexOf(']') +1);
                            String innerItem = item.substring(item.indexOf('[')+1, item.indexOf(']'));
                            String newDataType = "List" + "[" + "List" + "[" + innerItem + "]" + "]";
                            cp.vendorExtensions.put("x-dataType", newDataType);
                        } else if (Character.isUpperCase(item.charAt(0))) {
                            String newDataType = "List" + "[" + "'" + item + "'" + "]";
                            cp.vendorExtensions.put("x-dataType", newDataType);
                        } else if (item.contains("dict(")) {
                            String key =  item.substring(item.indexOf('(') + 1, item.indexOf(','));
                            String value = item.substring(item.indexOf(' ') + 1, item.indexOf(')'));
                            if (Character.isUpperCase(value.charAt(0))) {
                                String newValue = "'" + value + "'";
                                String newDataType = "List" + "[" + "Dict" + "[" + key + "," + " " + newValue + "]" + "]";
                                cp.vendorExtensions.put("x-dataType", newDataType);
                            } else {
                                String newDataType = "List" + "[" + "Dict" + "[" + key + "," + " " + value + "]" + "]";
                                cp.vendorExtensions.put("x-dataType", newDataType);
                            }
                        } else {
                            String newDataType = "List" + "[" + item + "]";
                            cp.vendorExtensions.put("x-dataType", newDataType);
                        }
                    } else if (cp.dataType.contains("dict(")) {
                        String key =  cp.dataType.substring(cp.dataType.indexOf('(') + 1, cp.dataType.indexOf(','));
                        String value =  cp.dataType.substring(cp.dataType.indexOf(' ') + 1, cp.dataType.indexOf(')'));
                        if (value.contains("dict(")) {
                            value =  cp.dataType.substring(cp.dataType.indexOf(' ') + 1, cp.dataType.indexOf(')') + 1);
                            String innerKey =  value.substring(value.indexOf('(') + 1, value.indexOf(','));
                            String innerValue =  value.substring(value.indexOf(' ') + 1, value.indexOf(')'));
                            if (Character.isUpperCase(innerValue.charAt(0))) {
                                String newDataType = "Dict" + "[" + key + "," + " " + "Dict" + "[" + innerKey + "," + " " + "'" + innerValue + "'" + "]" + "]";
                                cp.vendorExtensions.put("x-dataType", newDataType);
                            } else {
                                String newDataType = "Dict" + "[" + key + "," + " " + "Dict" + "[" + innerKey + "," + " " + innerValue + "]" + "]";
                                cp.vendorExtensions.put("x-dataType", newDataType);
                            }
                        } else if (value.contains("list[")) {
                            String item = cp.dataType.substring(cp.dataType.indexOf('[') + 1, cp.dataType.indexOf(']'));
                            if (Character.isUpperCase(item.charAt(0))) {
                                String newDataType = "Dict" + "[" + key + "," + " " + "List" + "[" + "'" + item + "'" + "]";
                                cp.vendorExtensions.put("x-dataType", newDataType);
                            } else {
                                String newDataType = "Dict" + "[" + key + "," + " " + "List" + "[" + item + "]";
                                cp.vendorExtensions.put("x-dataType", newDataType);
                            }
                        } else if (Character.isUpperCase(value.charAt(0))) { // value is a model
                            String newValue = "'" + value + "'";
                            String newDataType = "Dict" + "[" + key + "," + " " + newValue + "]";
                            cp.vendorExtensions.put("x-dataType", newDataType);
                        } else {
                            String newDataType = "Dict" + "[" + key + "," + " " + value + "]";
                            cp.vendorExtensions.put("x-dataType", newDataType);
                        }
                    } else if (cp.isModel || Character.isUpperCase(cp.dataType.charAt(0))) {
                        String newDataType = "'" + cp.dataType + "'";
                        cp.vendorExtensions.put("x-dataType", newDataType);
                    } else {
                        cp.vendorExtensions.put("x-dataType", cp.dataType);
                    }
                }
            }
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
        // DEVTOOLING-1755: Fix Regex Patterns
        // Python now requires double backslash in regex patterns.
        // Although openapi-generator preserves double backslash when processing the Platform API Swagger specification,
        // the double backslash is transformed into a single backslash when the pattern property value is inserted via the mustache template.
        // Modify openapi-generator to change double backslash to quadruple backslash so that proper value is used in Python SDK source code.
        fixPatternParameter(parameter);
    }

    // DEVTOOLING-1755: Fix Regex Patterns
    public void fixPatternParameter(CodegenParameter parameter){
        if(parameter.pattern != null) {
            parameter.pattern = parameter.pattern.replace("\\", "\\\\");
            if(parameter.vendorExtensions != null && parameter.vendorExtensions.get("x-regex") != null) {
                String veRegex = String.valueOf(parameter.vendorExtensions.get("x-regex"));
                if(veRegex != null) {
                    veRegex = veRegex.replace("\\", "\\\\");
                    parameter.vendorExtensions.put("x-regex", veRegex);
                }
            }
        }
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);
        // DEVTOOLING-1755: Fix Regex Patterns
        fixPatternModelProperty(property);
    }

    // DEVTOOLING-1755: Fix Regex Patterns
    public void fixPatternModelProperty(CodegenProperty property){
        if(property.pattern != null) {
            property.pattern = property.pattern.replace("\\", "\\\\");
            if(property.vendorExtensions != null && property.vendorExtensions.get("x-regex") != null) {
                String veRegex = String.valueOf(property.vendorExtensions.get("x-regex"));
                if(veRegex != null) {
                    veRegex = veRegex.replace("\\", "\\\\");
                    property.vendorExtensions.put("x-regex", veRegex);
                }
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
                    List<CodegenParameter> allParams = operation.allParams;
                    // Determine the python "type hinting" data type for required operation parameters
                    for (CodegenParameter p : allParams) {
                        if (p.required) {
                            if (p.dataType.contains("list[")) {
                                String item = p.dataType.substring(p.dataType.indexOf('[')+1, p.dataType.indexOf(']'));
                                String newDataType = "List[" + "'" + item + "']";
                                p.vendorExtensions.put("x-dataType", newDataType);
                            } else if (p.isModel || Character.isUpperCase(p.dataType.charAt(0))) {
                                String newDataType = "'" + p.dataType + "'";
                                p.vendorExtensions.put("x-dataType", newDataType);
                            } else {
                                p.vendorExtensions.put("x-dataType", p.dataType);
                            }
                        }
                    }
                    // Determine the python "type hinting" data type for operation return types
                    if (operation.returnType != null) {
                        if (operation.returnType.contains("list[")) {
                            String item = operation.returnType.substring(operation.returnType.indexOf('[')+1, operation.returnType.indexOf(']'));
                            if (item.contains("list[")) {
                                item = operation.returnType.substring(operation.returnType.indexOf('[')+1, operation.returnType.indexOf(']') +1);
                                String innerItem = item.substring(item.indexOf('[')+1, item.indexOf(']'));
                                String newDataType = "List" + "[" + "List" + "[" + innerItem + "]" + "]";
                                operation.vendorExtensions.put("x-returnType", newDataType);
                            } else if (Character.isUpperCase(item.charAt(0))) {
                                String newDataType = "List" + "[" + "'" + item + "'" + "]";
                                operation.vendorExtensions.put("x-returnType", newDataType);
                            } else if (item.contains("dict(")) {
                                String key =  item.substring(item.indexOf('(') + 1, item.indexOf(','));
                                String value = item.substring(item.indexOf(' ') + 1, item.indexOf(')'));
                                if (Character.isUpperCase(value.charAt(0))) {
                                    String newValue = "'" + value + "'";
                                    String newDataType = "List" + "[" + "Dict" + "[" + key + "," + " " + newValue + "]" + "]";
                                    operation.vendorExtensions.put("x-returnType", newDataType);
                                } else {
                                    String newDataType = "List" + "[" + "Dict" + "[" + key + "," + " " + value + "]" + "]";
                                    operation.vendorExtensions.put("x-returnType", newDataType);
                                }
                            } else {
                                String newDataType = "List" + "[" + item + "]";
                                operation.vendorExtensions.put("x-returnType", newDataType);
                            }
                        } else if (operation.returnType.contains("dict(")) {
                            String key =  operation.returnType.substring(operation.returnType.indexOf('(') + 1, operation.returnType.indexOf(','));
                            String value =  operation.returnType.substring(operation.returnType.indexOf(' ') + 1, operation.returnType.indexOf(')'));
                            if (value.contains("dict(")) {
                                value =  operation.returnType.substring(operation.returnType.indexOf(' ') + 1, operation.returnType.indexOf(')') + 1);
                                String innerKey =  value.substring(value.indexOf('(') + 1, value.indexOf(','));
                                String innerValue =  value.substring(value.indexOf(' ') + 1, value.indexOf(')'));
                                String newDataType = "Dict" + "[" + key + "," + " " + "Dict" + "[" + innerKey + "," + " " + innerValue + "]" + "]";
                                operation.vendorExtensions.put("x-returnType", newDataType);
                            } else if (value.contains("list[")) {
                                String item = operation.returnType.substring(operation.returnType.indexOf('[') + 1, operation.returnType.indexOf(']'));
                                if (Character.isUpperCase(item.charAt(0))) {
                                    String newDataType = "Dict" + "[" + key + "," + " " + "List" + "[" + "'" + item + "'" + "]";
                                    operation.vendorExtensions.put("x-returnType", newDataType);
                                } else {
                                    String newDataType = "Dict" + "[" + key + "," + " " + "List" + "[" + item + "]";
                                    operation.vendorExtensions.put("x-returnType", newDataType);
                                }
                            } else if (Character.isUpperCase(value.charAt(0))) { // value is a model
                                String newValue = "'" + value + "'";
                                String newDataType = "Dict" + "[" + key + "," + " " + newValue + "]";
                                operation.vendorExtensions.put("x-returnType", newDataType);
                            } else {
                                String newDataType = "Dict" + "[" + key + "," + " " + value + "]";
                                operation.vendorExtensions.put("x-returnType", newDataType);
                            }
                        } else if (Character.isUpperCase(operation.returnType.charAt(0))) {
                            String newDataType = "'" + operation.returnType + "'";
                            operation.vendorExtensions.put("x-returnType", newDataType);
                        } else {
                            operation.vendorExtensions.put("x-returnType", operation.returnType);
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
