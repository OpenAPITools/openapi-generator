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
import io.swagger.v3.oas.models.Paths;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.parameters.QueryParameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.responses.ApiResponses;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

public class PhpMezzioPathHandlerServerCodegen extends AbstractPhpCodegen {
    private final Logger LOGGER = LoggerFactory.getLogger(PhpMezzioPathHandlerServerCodegen.class);
    // Custom generator option names
    public static final String OPT_MODERN = "modern";
    // Internal vendor extension names for extra template data that should not be set in specification
    public static final String VEN_FROM_QUERY = "internal.ze-ph.fromQuery";
    public static final String VEN_COLLECTION_FORMAT = "internal.ze-ph.collectionFormat";
    public static final String VEN_QUERY_DATA_TYPE = "internal.ze-ph.queryDataType";
    public static final String VEN_HAS_QUERY_DATA = "internal.ze-ph.hasQueryData";
    public static final String VEN_FROM_CONTAINER = "internal.ze-ph.fromContainer";
    public static final String VEN_CONTAINER_DATA_TYPE = "internal.ze-ph.containerDataType";

    private boolean useModernSyntax = false;

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "php-mezzio-ph";
    }

    @Override
    public String getHelp() {
        return "Generates PHP server stub using Mezzio ( https://docs.mezzio.dev/mezzio/ ) and Path Handler ( https://github.com/Articus/PathHandler ).";
    }

    public PhpMezzioPathHandlerServerCodegen() {
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

        // remove these from primitive types to make the output works
        languageSpecificPrimitives.remove("\\DateTime");
        languageSpecificPrimitives.remove("\\SplFileObject");

        embeddedTemplateDir = templateDir = "php-mezzio-ph";
        invokerPackage = "App";
        srcBasePath = "src" + File.separator + "App";
        apiDirName = "Handler";
        modelDirName = "DTO";
        apiPackage = invokerPackage + "\\" + apiDirName;
        modelPackage = invokerPackage + "\\" + modelDirName;
        //"Api" classes have dedicated namespace so there is no need to add non-empty suffix by default
        apiNameSuffix = "";

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

        additionalProperties.put(CodegenConstants.ARTIFACT_VERSION, "1.0.0");
        //Register custom CLI options
        addSwitch(OPT_MODERN, "use modern language features (generated code will require PHP 8.0)", useModernSyntax);
    }

    @Override
    public void processOpts() {
        super.processOpts();
        if (additionalProperties.containsKey(OPT_MODERN)) {
            embeddedTemplateDir = templateDir = "php-mezzio-ph-modern";
            useModernSyntax = true;
        }
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

    @Override
    public String toApiName(String name) {
        return super.toApiName(toModelName(name));
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        String result;
        Map<String, Object> extensions = p.getExtensions();
        if ((extensions != null) && extensions.containsKey(VEN_CONTAINER_DATA_TYPE)) {
            result = (String) extensions.get(VEN_CONTAINER_DATA_TYPE);
        } else if (useModernSyntax && (ModelUtils.isArraySchema(p) || ModelUtils.isMapSchema(p))) {
            result = "array";
        } else {
            result = super.getTypeDeclaration(p);
        }
        return result;
    }

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        super.preprocessOpenAPI(openAPI);

        generateParameterSchemas(openAPI);
        generateContainerSchemas(openAPI);
    }

    /**
     * Generate additional model definitions from query parameters
     *
     * @param openAPI OpenAPI object
     */
    protected void generateParameterSchemas(OpenAPI openAPI) {
        Map<String, PathItem> paths = openAPI.getPaths();
        if (paths != null) {
            for (Map.Entry<String, PathItem> pathsEntry : paths.entrySet()) {
                String pathname = pathsEntry.getKey();
                PathItem path = pathsEntry.getValue();
                Map<HttpMethod, Operation> operationMap = path.readOperationsMap();
                if (operationMap != null) {
                    for (Map.Entry<HttpMethod, Operation> operationMapEntry : operationMap.entrySet()) {
                        HttpMethod method = operationMapEntry.getKey();
                        Operation operation = operationMapEntry.getValue();
                        Map<String, Schema> propertySchemas = new HashMap<>();
                        if (operation == null || operation.getParameters() == null) {
                            continue;
                        }

                        List<String> requiredProperties = new ArrayList<>();
                        for (Parameter parameter : operation.getParameters()) {
                            Parameter referencedParameter = ModelUtils.getReferencedParameter(openAPI, parameter);
                            Schema propertySchema = convertParameterToSchema(openAPI, referencedParameter);
                            if (propertySchema != null) {
                                propertySchemas.put(propertySchema.getName(), propertySchema);
                                if (Boolean.TRUE.equals(referencedParameter.getRequired())) {
                                    requiredProperties.add(propertySchema.getName());
                                }
                            }
                        }

                        if (!propertySchemas.isEmpty()) {
                            ObjectSchema schema = new ObjectSchema();
                            String operationId = getOrGenerateOperationId(operation, pathname, method.name());
                            schema.setDescription("Query parameters for " + operationId);
                            schema.setProperties(propertySchemas);
                            schema.setRequired(requiredProperties);
                            addInternalExtensionToSchema(schema, VEN_FROM_QUERY, Boolean.TRUE);
                            String schemaName = generateUniqueSchemaName(openAPI, operationId + "QueryData");
                            openAPI.getComponents().addSchemas(schemaName, schema);
                            String schemaDataType = getTypeDeclaration(toModelName(schemaName));
                            addInternalExtensionToOperation(operation, VEN_QUERY_DATA_TYPE, schemaDataType);
                            addInternalExtensionToOperation(operation, VEN_HAS_QUERY_DATA, Boolean.TRUE);
                        }
                    }
                }
            }
        }
    }

    protected Schema convertParameterToSchema(OpenAPI openAPI, Parameter parameter) {
        Schema property = null;
        if (parameter instanceof QueryParameter) {
            QueryParameter queryParameter = (QueryParameter) parameter;
            Schema parameterSchema = ModelUtils.getReferencedSchema(openAPI, queryParameter.getSchema());
            // array
            if (ModelUtils.isArraySchema(parameterSchema)) {
                Schema itemSchema = ((ArraySchema) parameterSchema).getItems();
                ArraySchema arraySchema = new ArraySchema();
                arraySchema.setMinItems(parameterSchema.getMinItems());
                arraySchema.setMaxItems(parameterSchema.getMaxItems());
                arraySchema.setItems(itemSchema);
                String collectionFormat = getCollectionFormat(queryParameter);
                if (collectionFormat == null) {
                    collectionFormat = "csv";
                }
                addInternalExtensionToSchema(arraySchema, VEN_COLLECTION_FORMAT, collectionFormat);
                property = arraySchema;
            } else { // non-array e.g. string, integer
                switch (parameterSchema.getType()) {
                    case "string":
                        StringSchema stringSchema = new StringSchema();
                        stringSchema.setMinLength(parameterSchema.getMinLength());
                        stringSchema.setMaxLength(parameterSchema.getMaxLength());
                        stringSchema.setPattern(parameterSchema.getPattern());
                        stringSchema.setEnum(parameterSchema.getEnum());
                        property = stringSchema;
                        break;
                    case "integer":
                        IntegerSchema integerSchema = new IntegerSchema();
                        integerSchema.setMinimum(parameterSchema.getMinimum());
                        integerSchema.setMaximum(parameterSchema.getMaximum());
                        property = integerSchema;
                        break;
                    case "number":
                        NumberSchema floatSchema = new NumberSchema();
                        floatSchema.setMinimum(parameterSchema.getMinimum());
                        floatSchema.setMaximum(parameterSchema.getMaximum());
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

    protected String generateUniqueSchemaName(OpenAPI openAPI, String name) {
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

    /**
     * Generate additional model definitions for containers in whole specification
     *
     * @param openAPI OpenAPI object
     */
    protected void generateContainerSchemas(OpenAPI openAPI) {
        Paths paths = openAPI.getPaths();
        for (String pathName : paths.keySet()) {
            for (Operation operation : paths.get(pathName).readOperations()) {
                List<Parameter> parameters = operation.getParameters();
                if (parameters != null) {
                    for (Parameter parameter : parameters) {
                        generateContainerSchemas(openAPI, ModelUtils.getReferencedParameter(openAPI, parameter).getSchema());
                    }
                }
                RequestBody requestBody = ModelUtils.getReferencedRequestBody(openAPI, operation.getRequestBody());
                if (requestBody != null) {
                    Content requestBodyContent = requestBody.getContent();
                    if (requestBodyContent != null) {
                        for (String mediaTypeName : requestBodyContent.keySet()) {
                            generateContainerSchemas(openAPI, requestBodyContent.get(mediaTypeName).getSchema());
                        }
                    }
                }
                ApiResponses responses = operation.getResponses();
                for (String responseCode : responses.keySet()) {
                    ApiResponse response = ModelUtils.getReferencedApiResponse(openAPI, responses.get(responseCode));
                    Content responseContent = response.getContent();
                    if (responseContent != null) {
                        for (String mediaTypeName : responseContent.keySet()) {
                            generateContainerSchemas(openAPI, responseContent.get(mediaTypeName).getSchema());
                        }
                    }
                }
            }
        }
    }

    /**
     * Generate additional model definitions for containers in specified schema
     *
     * @param openAPI OpenAPI object
     * @param schema OAS schema to process
     */
    protected void generateContainerSchemas(OpenAPI openAPI, Schema schema) {
        if (schema != null) {
            //Dereference schema
            schema = ModelUtils.getReferencedSchema(openAPI, schema);
            Boolean isContainer = Boolean.FALSE;

            if (ModelUtils.isObjectSchema(schema)) {
                //Recursively process all schemas of object properties
                Map<String, Schema> properties = schema.getProperties();
                if (properties != null) {
                    for (String propertyName: properties.keySet()) {
                        generateContainerSchemas(openAPI, properties.get(propertyName));
                    }
                }
            } else if (ModelUtils.isArraySchema(schema)) {
                //Recursively process schema of array items
                generateContainerSchemas(openAPI, ((ArraySchema) schema).getItems());
                isContainer = Boolean.TRUE;
            } else if (ModelUtils.isMapSchema(schema)) {
                //Recursively process schema of map items
                Object itemSchema = schema.getAdditionalProperties();
                if (itemSchema instanceof Schema) {
                    generateContainerSchemas(openAPI, (Schema) itemSchema);
                }
                isContainer = Boolean.TRUE;
            }

            if (isContainer) {
                //Generate special component schema for container
                String containerSchemaName = generateUniqueSchemaName(openAPI, "Collection");
                Schema containerSchema = new ObjectSchema();
                containerSchema.addProperties("inner", schema);
                addInternalExtensionToSchema(containerSchema, VEN_FROM_CONTAINER, Boolean.TRUE);
                openAPI.getComponents().addSchemas(containerSchemaName, containerSchema);
                String containerDataType = getTypeDeclaration(toModelName(containerSchemaName));
                addInternalExtensionToSchema(schema, VEN_CONTAINER_DATA_TYPE, containerDataType);
            }
        }
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
