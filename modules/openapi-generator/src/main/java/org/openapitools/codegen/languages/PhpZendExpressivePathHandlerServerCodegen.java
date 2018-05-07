/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.PathItem;
import org.openapitools.codegen.*;

import io.swagger.v3.oas.models.PathItem.HttpMethod;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.core.util.Yaml;
import io.swagger.v3.oas.models.parameters.*;
import org.openapitools.codegen.utils.ModelUtils;


import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class PhpZendExpressivePathHandlerServerCodegen extends AbstractPhpCodegen {

    public static final String VEN_FROM_QUERY = "internal.ze-ph.fromQuery";
    public static final String VEN_COLLECTION_FORMAT = "internal.ze-ph.collectionFormat";
    public static final String VEN_QUERY_DATA_TYPE = "internal.ze-ph.queryDataType";
    public static final String VEN_HAS_QUERY_DATA = "internal.ze-ph.hasQueryData";

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "php-ze-ph";
    }

    @Override
    public String getHelp() {
        return "Generates PHP server stub using Zend Expressive ( https://zendframework.github.io/zend-expressive ) and Path Handler ( https://github.com/Articus/PathHandler ).";
    }

    public PhpZendExpressivePathHandlerServerCodegen() {
        super();
        //no point to use double - http://php.net/manual/en/language.types.float.php , especially because of PHP 7+ float type declaration
        typeMapping.put("double", "float");

        embeddedTemplateDir = templateDir = "ze-ph";
        invokerPackage = "App";
        packagePath = "";
        srcBasePath = "src" + File.separator + "App";
        apiDirName = "Handler";
        modelDirName = "DTO";
        apiPackage = invokerPackage + "\\" + apiDirName;
        modelPackage = invokerPackage + "\\" + modelDirName;

        apiTestTemplateFiles.clear();
        modelTestTemplateFiles.clear();
        apiDocTemplateFiles.clear();
        modelDocTemplateFiles.clear();

        supportingFiles.add(new SupportingFile("README.md.mustache", packagePath, "README.md"));
        supportingFiles.add(new SupportingFile("composer.json.mustache", packagePath, "composer.json"));
        supportingFiles.add(new SupportingFile("index.php", packagePath + File.separator + "public", "index.php"));
        supportingFiles.add(new SupportingFile("container.php", packagePath + File.separator + "application", "container.php"));
        supportingFiles.add(new SupportingFile("config.yml", packagePath + File.separator + "application", "config.yml"));
        supportingFiles.add(new SupportingFile("app.yml.mustache", packagePath + File.separator + "application" + File.separator + "config", "app.yml"));
        supportingFiles.add(new SupportingFile("path_handler.yml.mustache", packagePath + File.separator + "application" + File.separator + "config", "path_handler.yml"));
        supportingFiles.add(new SupportingFile("data_transfer.yml.mustache", packagePath + File.separator + "application" + File.separator + "config", "data_transfer.yml"));
        supportingFiles.add(new SupportingFile("ErrorMiddleware.php.mustache", packagePath + File.separator + srcBasePath, "ErrorMiddleware.php"));
        supportingFiles.add(new SupportingFile("Date.php.mustache", packagePath + File.separator + srcBasePath + File.separator + "Strategy", "Date.php"));
        supportingFiles.add(new SupportingFile("DateTime.php.mustache", packagePath + File.separator + srcBasePath + File.separator + "Strategy", "DateTime.php"));
        supportingFiles.add(new SupportingFile("QueryParameter.php.mustache", packagePath + File.separator + srcBasePath + File.separator + "Strategy", "QueryParameter.php"));
        supportingFiles.add(new SupportingFile("QueryParameterArray.php.mustache", packagePath + File.separator + srcBasePath + File.separator + "Strategy", "QueryParameterArray.php"));
        supportingFiles.add(new SupportingFile("Type.php.mustache", packagePath + File.separator + srcBasePath + File.separator + "Validator", "Type.php"));
        supportingFiles.add(new SupportingFile("QueryParameterType.php.mustache", packagePath + File.separator + srcBasePath + File.separator + "Validator", "QueryParameterType.php"));
        supportingFiles.add(new SupportingFile("QueryParameterArrayType.php.mustache", packagePath + File.separator + srcBasePath + File.separator + "Validator", "QueryParameterArrayType.php"));

        additionalProperties.put(CodegenConstants.ARTIFACT_VERSION, "1.0.0");
    }

    /**
     * Add operation to group
     * Override of default grouping - group by resource path, not tag
     *
     * @param tag          name of the tag
     * @param resourcePath path of the resource
     * @param operation    Swagger Operation object
     * @param co           Codegen Operation object
     * @param operations   map of Codegen operations
     */
    @Override
    public void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation co, Map<String, List<CodegenOperation>> operations) {
        List<CodegenOperation> opList = operations.get(resourcePath);
        if (opList == null) {
            opList = new ArrayList<CodegenOperation>();
            operations.put(resourcePath, opList);
        }
        //ignore duplicate operation ids - that means that operation has several tags
        int counter = 0;
        for (CodegenOperation op : opList) {
            if (co.operationId.equals(op.operationId)) {
                counter++;
            }
        }
        if (counter == 0) {
            co.operationIdLowerCase = co.operationId.toLowerCase();
            opList.add(co);
            co.baseName = tag;
        }
    }

    /**
     * Return the file name of the Api Test
     *
     * @param name the file name of the Api
     * @return the file name of the Api
     */
    @Override
    public String toApiFilename(String name) {
        return toApiName(name);
    }

    /**
     * Output the API (class) name (capitalized) ending with "Api"
     * Return DefaultApi if name is empty
     *
     * @param name the name of the Api
     * @return capitalized Api name ending with "Api"
     */
    @Override
    public String toApiName(String name) {
        //Remove }
        name = name.replaceAll("[\\}]", "");
        return super.toModelName(name);
    }

    /**
     * Generate additional model definitions from query parameters
     *
     * @param openAPI OpenAPI object
     */
    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        super.preprocessOpenAPI(openAPI);

        Map<String, PathItem> paths = openAPI.getPaths();
        if (paths != null) {
            for (String pathname : paths.keySet()) {
                PathItem path = paths.get(pathname);
                Map<HttpMethod, Operation> operationMap = path.readOperationsMap();
                if (operationMap != null) {
                    for (HttpMethod method : operationMap.keySet()) {
                        Operation operation = operationMap.get(method);
                        Map<String, Schema> schemas = new HashMap<>();
                        if (operation == null || operation.getParameters() == null) {
                            continue;
                        }

                        List<String> requiredProperties = new ArrayList<>();
                        for (Parameter parameter : operation.getParameters()) {
                            Schema schema = convertParameterToSchema(parameter);
                            if (schema != null) {
                                schemas.put(schema.getName(), schema);
                                if (Boolean.TRUE.equals(parameter.getRequired())) {
                                    requiredProperties.add(schema.getName());
                                }
                            }
                        }

                        if (!schemas.isEmpty()) {
                            ObjectSchema model = new ObjectSchema();
                            String operationId = getOrGenerateOperationId(operation, pathname, method.name());
                            model.setDescription("Query parameters for " + operationId);
                            model.setProperties(schemas);
                            model.setRequired(requiredProperties);
                            //Add internal extension directly, because addExtension filters extension names
                            addInternalExtensionToSchema(model, VEN_FROM_QUERY, Boolean.TRUE);
                            String definitionName = generateUniqueDefinitionName(operationId + "QueryData", openAPI);
                            openAPI.getComponents().addSchemas(definitionName, model);
                            String definitionModel = "\\" + modelPackage + "\\" + toModelName(definitionName);
                            addInternalExtensionToOperation(operation, VEN_QUERY_DATA_TYPE, definitionModel);
                            addInternalExtensionToOperation(operation, VEN_HAS_QUERY_DATA, Boolean.TRUE);
                        }
                    }
                }
            }
        }
    }

    protected Schema convertParameterToSchema(Parameter parameter) {
        Schema property = null;
        if (parameter instanceof QueryParameter) {
            QueryParameter queryParameter = (QueryParameter) parameter;
            // array
            if (ModelUtils.isArraySchema(queryParameter.getSchema())) {
                Schema inner = ((ArraySchema) queryParameter.getSchema()).getItems();
                ArraySchema arraySchema = new ArraySchema();
                arraySchema.setMinItems(queryParameter.getSchema().getMinItems());
                arraySchema.setMaxItems(queryParameter.getSchema().getMaxItems());
                arraySchema.setItems(inner);
                String collectionFormat = getCollectionFormat(queryParameter);
                if (collectionFormat == null) {
                    collectionFormat = "csv";
                }
                addInternalExtensionToSchema(arraySchema, VEN_COLLECTION_FORMAT, collectionFormat);
                property = arraySchema;
            } else { // non-array e.g. string, integer
                switch (queryParameter.getSchema().getType()) {
                    case "string":
                        StringSchema stringSchema = new StringSchema();
                        stringSchema.setMinLength(queryParameter.getSchema().getMinLength());
                        stringSchema.setMaxLength(queryParameter.getSchema().getMaxLength());
                        stringSchema.setPattern(queryParameter.getSchema().getPattern());
                        stringSchema.setEnum(queryParameter.getSchema().getEnum());
                        property = stringSchema;
                        break;
                    case "integer":
                        IntegerSchema integerSchema = new IntegerSchema();
                        integerSchema.setMinimum(queryParameter.getSchema().getMinimum());
                        integerSchema.setMaximum(queryParameter.getSchema().getMaximum());
                        property = integerSchema;
                        break;
                    case "number":
                        NumberSchema floatSchema = new NumberSchema();
                        floatSchema.setMinimum(queryParameter.getSchema().getMinimum());
                        floatSchema.setMaximum(queryParameter.getSchema().getMaximum());
                        property = floatSchema;
                        break;
                    case "boolean":
                        property = new BooleanSchema();
                        break;
                    case "date":
                        property = new DateSchema();
                        break;
                    case "date-time":
                        property = new DateTimeSchema();
                        break;
                }
            }
            if (property != null) {
                property.setName(queryParameter.getName());
                property.setDescription(queryParameter.getDescription());
                addInternalExtensionToSchema(property, VEN_FROM_QUERY, Boolean.TRUE);
            }
        }
        return property;
    }

    protected void addInternalExtensionToSchema(Schema schema, String name, Object value) {
        //Add internal extension directly, because addExtension filters extension names
        if (schema.getExtensions() == null) {
            schema.setExtensions(new HashMap<>());
        }
        schema.getExtensions().put(name, value);
    }

    protected void addInternalExtensionToOperation(Operation operation, String name, Object value) {
        //Add internal extension directly, because addExtension filters extension names
        if (operation.getExtensions() == null) {
            operation.setExtensions(new HashMap<>());
        }
        operation.getExtensions().put(name, value);
    }

    protected String generateUniqueDefinitionName(String name, OpenAPI openAPI) {
        String result = name;
        if (openAPI.getComponents().getSchemas() != null) {
            int count = 1;
            while (openAPI.getComponents().getSchemas().containsKey(result)) {
                result = name + "_" + count;
                count += 1;
            }
        }
        return result;
    }

    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        objs = super.postProcessOperations(objs);
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");
        String interfaceToImplement;
        StringBuilder interfacesToImplement = new StringBuilder();
        String classMethod;
        String pathPattern = null;
        for (CodegenOperation op : operationList) {
            switch (op.httpMethod) {
                case "GET":
                    interfaceToImplement = "Operation\\GetInterface";
                    classMethod = "handleGet";
                    break;
                case "POST":
                    interfaceToImplement = "Operation\\PostInterface";
                    classMethod = "handlePost";
                    break;
                case "PATCH":
                    interfaceToImplement = "Operation\\PatchInterface";
                    classMethod = "handlePatch";
                    break;
                case "PUT":
                    interfaceToImplement = "Operation\\PutInterface";
                    classMethod = "handlePut";
                    break;
                case "DELETE":
                    interfaceToImplement = "Operation\\DeleteInterface";
                    classMethod = "handleDelete";
                    break;
                default:
                    throw new RuntimeException("Unknown HTTP Method " + op.httpMethod + " not allowed");
            }
            if (interfacesToImplement.length() > 0) {
                interfacesToImplement.append(", ");
            }
            interfacesToImplement.append(interfaceToImplement);
            op.httpMethod = classMethod;
            //All operations have same path because of custom operation grouping, so path pattern can be calculated only once
            if (pathPattern == null) {
                pathPattern = generatePathPattern(op);
            }
        }
        operations.put("interfacesToImplement", interfacesToImplement.toString());
        operations.put("pathPattern", pathPattern);

        return objs;
    }

    protected String generatePathPattern(CodegenOperation op) {
        String result = op.path;
        for (CodegenParameter pp : op.pathParams) {
            StringBuilder replacement = new StringBuilder("{" + pp.paramName);
            if (pp.isEnum) {
                StringBuilder enumRegExp = new StringBuilder();
                for (String enumValue : pp._enum) {
                    if (enumRegExp.length() > 0) {
                        enumRegExp.append("|");
                    }
                    enumRegExp.append(enumValue.replaceAll("[\\Q<>()[]{}|^$-=!?*+.\\\\E]", "\\\\$0"));
                }
                replacement.append(":");
                replacement.append(enumRegExp);
            } else if (pp.isInteger) {
                replacement.append(":0|(?:-?[1-9][0-9]*)");
            } else if (pp.isString && (pp.pattern != null) && (!pp.pattern.isEmpty())) {
                replacement.append(":");
                replacement.append(pp.pattern);
            }
            //TODO add regular expressions for other types if they are actually used for path parameters
            replacement.append("}");
            result = result.replace("{" + pp.paramName + "}", replacement);
        }
        return result;
    }
}
