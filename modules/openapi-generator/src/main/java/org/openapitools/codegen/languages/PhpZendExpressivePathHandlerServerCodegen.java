/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.PathItem.HttpMethod;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.parameters.QueryParameter;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

public class PhpZendExpressivePathHandlerServerCodegen extends AbstractPhpCodegen {
    private static final Logger LOGGER = LoggerFactory.getLogger(PhpZendExpressivePathHandlerServerCodegen.class);

    // TODO: Rename to x- prefixed vendor extensions, per specification.
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

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML))
                .securityFeatures(EnumSet.noneOf(SecurityFeature.class))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling
                )
                .excludeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
        );

        //no point to use double - http://php.net/manual/en/language.types.float.php , especially because of PHP 7+ float type declaration
        typeMapping.put("double", "float");

        embeddedTemplateDir = templateDir = "php-ze-ph";
        invokerPackage = "App";
        srcBasePath = "src" + File.separator + "App";
        apiDirName = "Handler";
        modelDirName = "DTO";
        apiPackage = invokerPackage + "\\" + apiDirName;
        modelPackage = invokerPackage + "\\" + modelDirName;

        apiTestTemplateFiles.clear();
        modelTestTemplateFiles.clear();
        apiDocTemplateFiles.clear();
        modelDocTemplateFiles.clear();

        supportingFiles.add(new SupportingFile("README.md.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("composer.json.mustache", "", "composer.json"));
        supportingFiles.add(new SupportingFile("index.php", "public", "index.php"));
        supportingFiles.add(new SupportingFile("container.php", "application", "container.php"));
        supportingFiles.add(new SupportingFile("config.yml", "application", "config.yml"));
        supportingFiles.add(new SupportingFile("app.yml.mustache", "application" + File.separator + "config", "app.yml"));
        supportingFiles.add(new SupportingFile("path_handler.yml.mustache", "application" + File.separator + "config", "path_handler.yml"));
        supportingFiles.add(new SupportingFile("data_transfer.yml.mustache", "application" + File.separator + "config", "data_transfer.yml"));
        supportingFiles.add(new SupportingFile("Factory.php.mustache", srcBasePath, "Factory.php"));
        supportingFiles.add(new SupportingFile("InternalServerError.php.mustache", srcBasePath + File.separator + "Middleware", "InternalServerError.php"));
        supportingFiles.add(new SupportingFile("Date.php.mustache", srcBasePath + File.separator + "Strategy", "Date.php"));
        supportingFiles.add(new SupportingFile("DateTime.php.mustache", srcBasePath + File.separator + "Strategy", "DateTime.php"));
        supportingFiles.add(new SupportingFile("QueryParameter.php.mustache", srcBasePath + File.separator + "Strategy", "QueryParameter.php"));
        supportingFiles.add(new SupportingFile("QueryParameterArray.php.mustache", srcBasePath + File.separator + "Strategy", "QueryParameterArray.php"));
        supportingFiles.add(new SupportingFile("Type.php.mustache", srcBasePath + File.separator + "Validator", "Type.php"));
        supportingFiles.add(new SupportingFile("QueryParameterType.php.mustache", srcBasePath + File.separator + "Validator", "QueryParameterType.php"));
        supportingFiles.add(new SupportingFile("QueryParameterArrayType.php.mustache", srcBasePath + File.separator + "Validator", "QueryParameterArrayType.php"));

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
            co.operationIdLowerCase = co.operationId.toLowerCase(Locale.ROOT);
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
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        objs = super.postProcessOperationsWithModels(objs, allModels);
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");
        String httpMethodDeclaration;
        String pathPattern = null;
        for (CodegenOperation op : operationList) {
            switch (op.httpMethod) {
                case "GET":
                    httpMethodDeclaration = "Get()";
                    break;
                case "POST":
                    httpMethodDeclaration = "Post()";
                    break;
                case "PATCH":
                    httpMethodDeclaration = "Patch()";
                    break;
                case "PUT":
                    httpMethodDeclaration = "Put()";
                    break;
                case "DELETE":
                    httpMethodDeclaration = "Delete()";
                    break;
                default:
                    httpMethodDeclaration = "HttpMethod(\"" + op.httpMethod + "\")";
            }
            op.httpMethod = httpMethodDeclaration;
            //Producing content with media type "*/*" is not supported
            if (op.produces != null) {
                for (Map<String, String> p : op.produces) {
                    if (p.replace("mediaType", "*/*", "n/a")) {
                        LOGGER.warn("Media type range '*/*' is not supported, using 'n/a' for code generation instead");
                    }
                }
            }
            //All operations have same path because of custom operation grouping, so path pattern can be calculated only once
            if (pathPattern == null) {
                pathPattern = generatePathPattern(op);
            }
        }
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
