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
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.responses.ApiResponses;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

public class PhpDataTransferClientCodegen extends AbstractPhpCodegen {
    private final Logger LOGGER = LoggerFactory.getLogger(PhpDataTransferClientCodegen.class);
    // Custom generator option names
    public static final String OPT_MODERN = "modern";
    // Internal vendor extension names for extra template data that should not be set in specification
    public static final String VEN_PARAMETER_LOCATION = "internal.parameterLocation";
    public static final String VEN_FROM_PARAMETERS = "internal.fromParameters";
    public static final String VEN_COLLECTION_FORMAT = "internal.collectionFormat";
    public static final String VEN_PARAMETER_DATA_TYPE = "internal.parameterDataType";
    public static final String VEN_HAS_PARAMETER_DATA = "internal.hasParameterData";
    public static final String VEN_FROM_CONTAINER = "internal.fromContainer";
    public static final String VEN_CONTAINER_DATA_TYPE = "internal.containerDataType";

    private boolean useModernSyntax = false;

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "php-dt";
    }

    @Override
    public String getHelp() {
        return "Generates a PHP client relying on Data Transfer ( https://github.com/Articus/DataTransfer ) and compliant with PSR-7, PSR-11, PSR-17 and PSR-18.";
    }

    public PhpDataTransferClientCodegen() {
        super();
        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON))
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

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();

        //no point to use double - http://php.net/manual/en/language.types.float.php , especially because of PHP 7+ float type declaration
        typeMapping.put("double", "float");

        // remove these from primitive types to make the output works
        languageSpecificPrimitives.remove("\\DateTime");
        languageSpecificPrimitives.remove("\\SplFileObject");

        apiTemplateFiles.clear();
        apiTestTemplateFiles.clear();
        apiDocTemplateFiles.clear();
        modelTestTemplateFiles.clear();
        modelDocTemplateFiles.clear();

        additionalProperties.put(CodegenConstants.ARTIFACT_VERSION, "1.0.0");
        //Register custom CLI options
        addSwitch(OPT_MODERN, "use modern language features (generated code will require PHP 8.0)", useModernSyntax);
    }

    @Override
    public void processOpts() {
        setSrcBasePath("src");
        //Preserve and process options mangled in parent class
        String rootNamespace = "App";
        if (additionalProperties.containsKey(CodegenConstants.INVOKER_PACKAGE)) {
            rootNamespace = (String) additionalProperties.get(CodegenConstants.INVOKER_PACKAGE);
        }
        String modelNamespace = rootNamespace + "\\DTO";
        if (additionalProperties.containsKey(CodegenConstants.MODEL_PACKAGE)) {
            modelNamespace = (String) additionalProperties.get(CodegenConstants.MODEL_PACKAGE);
        }
        super.processOpts();
        //Restore mangled options
        setInvokerPackage(rootNamespace);
        additionalProperties.put(CodegenConstants.INVOKER_PACKAGE, rootNamespace);
        setModelPackage(modelNamespace);
        additionalProperties.put(CodegenConstants.MODEL_PACKAGE, modelNamespace);
        //Process custom options
        if (additionalProperties.containsKey(OPT_MODERN)) {
            embeddedTemplateDir = templateDir = "php-dt-modern";
            useModernSyntax = true;
        } else {
            embeddedTemplateDir = templateDir = "php-dt";
        }

        supportingFiles.add(new SupportingFile("composer.json.mustache", "", "composer.json"));
        supportingFiles.add(new SupportingFile("ApiClient.php.mustache", toSrcPath(invokerPackage, srcBasePath), "ApiClient.php"));
        supportingFiles.add(new SupportingFile("ApiClientFactory.php.mustache", toSrcPath(invokerPackage, srcBasePath), "ApiClientFactory.php"));
        supportingFiles.add(new SupportingFile("README.md.mustache", "", "README.md"));
    }

    @Override
    public String toSrcPath(String packageName, String basePath) {
        return basePath + File.separator + packageName.replace("\\", File.separator);
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
    public void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation co, Map<String, List<CodegenOperation>> operations) {
        //Do not use tags for operation grouping
        super.addOperationToGroup("", resourcePath, operation, co, operations);
    }

    @Override
    protected String getContentType(RequestBody requestBody) {
        //Awfully nasty workaround to skip formParams generation
        return null;
    }

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        super.preprocessOpenAPI(openAPI);

        generateParameterSchemas(openAPI);
        generateContainerSchemas(openAPI);
    }

    @Override
    public void processOpenAPI(OpenAPI openAPI) {
        super.processOpenAPI(openAPI);

        quoteMediaTypes(openAPI);
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
                            schema.setDescription("Parameters for " + operationId);
                            schema.setProperties(propertySchemas);
                            schema.setRequired(requiredProperties);
                            addInternalExtensionToSchema(schema, VEN_FROM_PARAMETERS, Boolean.TRUE);
                            String schemaName = generateUniqueSchemaName(openAPI, operationId + "ParameterData");
                            openAPI.getComponents().addSchemas(schemaName, schema);
                            String schemaDataType = getTypeDeclaration(toModelName(schemaName));
                            addInternalExtensionToOperation(operation, VEN_PARAMETER_DATA_TYPE, schemaDataType);
                            addInternalExtensionToOperation(operation, VEN_HAS_PARAMETER_DATA, Boolean.TRUE);
                        }
                    }
                }
            }
        }
    }

    protected Schema convertParameterToSchema(OpenAPI openAPI, Parameter parameter) {
        Schema property = null;

        Schema parameterSchema = ModelUtils.getReferencedSchema(openAPI, parameter.getSchema());
        // array
        if (ModelUtils.isArraySchema(parameterSchema)) {
            Schema itemSchema = ((ArraySchema) parameterSchema).getItems();
            ArraySchema arraySchema = new ArraySchema();
            arraySchema.setMinItems(parameterSchema.getMinItems());
            arraySchema.setMaxItems(parameterSchema.getMaxItems());
            arraySchema.setItems(itemSchema);
            String collectionFormat = getCollectionFormat(parameter);
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
            property.setName(parameter.getName());
            property.setDescription(parameter.getDescription());
            addInternalExtensionToSchema(property, VEN_PARAMETER_LOCATION, parameter.getIn());
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
     * @param schema  OAS schema to process
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
                    for (String propertyName : properties.keySet()) {
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

    /**
     * Awfully nasty workaround - add quotation marks for all media types to prevent special treatment of form media types
     * in org/openapitools/codegen/DefaultGenerator.java:873
     * TODO find a better way to prevent special form media type treatment
     *
     * @param openAPI OpenAPI object
     */
    protected void quoteMediaTypes(OpenAPI openAPI) {
        Map<String, PathItem> paths = openAPI.getPaths();
        if (paths != null) {
            for (Map.Entry<String, PathItem> pathsEntry : paths.entrySet()) {
                String pathname = pathsEntry.getKey();
                PathItem path = pathsEntry.getValue();
                List<Operation> operations = path.readOperations();
                if (operations != null) {
                    for (Operation operation : operations) {
                        RequestBody requestBody = ModelUtils.getReferencedRequestBody(openAPI, operation.getRequestBody());
                        if (requestBody != null) {
                            requestBody.setContent(copyWithQuotedMediaTypes(requestBody.getContent()));
                        }
                        ApiResponses responses = operation.getResponses();
                        for (String responseCode : responses.keySet()) {
                            ApiResponse response = ModelUtils.getReferencedApiResponse(openAPI, responses.get(responseCode));
                            response.setContent(copyWithQuotedMediaTypes(response.getContent()));
                        }
                    }
                }
            }
        }
    }

    protected Content copyWithQuotedMediaTypes(Content content) {
        Content result = null;
        if (content != null) {
            result = new Content();
            for (String mediaType : content.keySet()) {
                result.addMediaType("'" + mediaType + "'", content.get(mediaType));
            }
        }
        return result;
    }
}
