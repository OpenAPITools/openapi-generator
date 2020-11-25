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

import com.google.common.collect.ImmutableMap;
import com.samskivert.mustache.Mustache;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.parser.util.SchemaTypeUtil;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.DocumentationFeature;
import org.openapitools.codegen.templating.mustache.IndentedLambda;
import org.openapitools.codegen.utils.ModelUtils;

import java.io.File;
import java.util.*;

public class TypeScriptFetchClientCodegen extends AbstractTypeScriptClientCodegen {
    private static final String X_IS_UNIQUE_ID = "x-isUniqueId";
    private static final String X_ENTITY_ID = "x-entityId";
    private static final String X_IS_META_DATA_RESPONSE = "x-isMetaDataResponse";

    public static final String NPM_REPOSITORY = "npmRepository";
    public static final String WITH_INTERFACES = "withInterfaces";
    public static final String USE_SINGLE_REQUEST_PARAMETER = "useSingleRequestParameter";
    public static final String PREFIX_PARAMETER_INTERFACES = "prefixParameterInterfaces";
    public static final String TYPESCRIPT_THREE_PLUS = "typescriptThreePlus";
    public static final String SAGAS_AND_RECORDS = "sagasAndRecords";

    protected String npmRepository = null;
    private boolean useSingleRequestParameter = true;
    private boolean prefixParameterInterfaces = false;
    protected boolean addedApiIndex = false;
    protected boolean addedModelIndex = false;
    protected boolean typescriptThreePlus = false;
    protected boolean sagasAndRecords = false;


    public TypeScriptFetchClientCodegen() {
        super();

        modifyFeatureSet(features -> features.includeDocumentationFeatures(DocumentationFeature.Readme));

        // clear import mapping (from default generator) as TS does not use it
        // at the moment
        importMapping.clear();

        outputFolder = "generated-code/typescript-fetch";
        embeddedTemplateDir = templateDir = "typescript-fetch";

        this.apiTemplateFiles.put("apis.mustache", ".ts");
        this.modelTemplateFiles.put("models.mustache", ".ts");
        this.addExtraReservedWords();

        typeMapping.put("date", "Date");
        typeMapping.put("DateTime", "Date");

        supportModelPropertyNaming(CodegenConstants.MODEL_PROPERTY_NAMING_TYPE.camelCase);
        this.cliOptions.add(new CliOption(NPM_REPOSITORY, "Use this property to set an url your private npmRepo in the package.json"));
        this.cliOptions.add(new CliOption(WITH_INTERFACES, "Setting this property to true will generate interfaces next to the default class implementations.", SchemaTypeUtil.BOOLEAN_TYPE).defaultValue(Boolean.FALSE.toString()));
        this.cliOptions.add(new CliOption(CodegenConstants.USE_SINGLE_REQUEST_PARAMETER, CodegenConstants.USE_SINGLE_REQUEST_PARAMETER_DESC, SchemaTypeUtil.BOOLEAN_TYPE).defaultValue(Boolean.TRUE.toString()));
        this.cliOptions.add(new CliOption(PREFIX_PARAMETER_INTERFACES, "Setting this property to true will generate parameter interface declarations prefixed with API class name to avoid name conflicts.", SchemaTypeUtil.BOOLEAN_TYPE).defaultValue(Boolean.FALSE.toString()));
        this.cliOptions.add(new CliOption(TYPESCRIPT_THREE_PLUS, "Setting this property to true will generate TypeScript 3.6+ compatible code.", SchemaTypeUtil.BOOLEAN_TYPE).defaultValue(Boolean.FALSE.toString()));
        this.cliOptions.add(new CliOption(SAGAS_AND_RECORDS, "Setting this property to true will generate additional files for use with redux-saga and immutablejs.", SchemaTypeUtil.BOOLEAN_TYPE).defaultValue(Boolean.FALSE.toString()));
    }

    @Override
    public String getName() {
        return "typescript-fetch";
    }

    @Override
    public String getHelp() {
        return "Generates a TypeScript client library using Fetch API (beta).";
    }

    public String getNpmRepository() {
        return npmRepository;
    }

    public void setNpmRepository(String npmRepository) {
        this.npmRepository = npmRepository;
    }

    public Boolean getTypescriptThreePlus() {
        return typescriptThreePlus;
    }

    public void setTypescriptThreePlus(Boolean typescriptThreePlus) {
        this.typescriptThreePlus = typescriptThreePlus;
    }

    public Boolean getSagasAndRecords() {
        return sagasAndRecords;
    }

    public void setSagasAndRecords(Boolean sagasAndRecords) {
        this.sagasAndRecords = sagasAndRecords;
    }

    @Override
    public void processOpts() {
        super.processOpts();
        additionalProperties.put("isOriginalModelPropertyNaming", getModelPropertyNaming() == CodegenConstants.MODEL_PROPERTY_NAMING_TYPE.original);
        additionalProperties.put("modelPropertyNaming", getModelPropertyNaming().name());

        String sourceDir = "";
        if (additionalProperties.containsKey(NPM_NAME)) {
            sourceDir = "src" + File.separator;
        }

        this.apiPackage = sourceDir + "apis";
        this.modelPackage = sourceDir + "models";

        supportingFiles.add(new SupportingFile("index.mustache", sourceDir, "index.ts"));
        supportingFiles.add(new SupportingFile("runtime.mustache", sourceDir, "runtime.ts"));

        if (additionalProperties.containsKey(CodegenConstants.USE_SINGLE_REQUEST_PARAMETER)) {
            this.setUseSingleRequestParameter(convertPropertyToBoolean(CodegenConstants.USE_SINGLE_REQUEST_PARAMETER));
        }
        writePropertyBack(CodegenConstants.USE_SINGLE_REQUEST_PARAMETER, getUseSingleRequestParameter());

        if (additionalProperties.containsKey(PREFIX_PARAMETER_INTERFACES)) {
            this.setPrefixParameterInterfaces(convertPropertyToBoolean(PREFIX_PARAMETER_INTERFACES));
        }
        writePropertyBack(PREFIX_PARAMETER_INTERFACES, getPrefixParameterInterfaces());

        if (additionalProperties.containsKey(NPM_NAME)) {
            addNpmPackageGeneration();
        }

        if (additionalProperties.containsKey(TYPESCRIPT_THREE_PLUS)) {
            this.setTypescriptThreePlus(convertPropertyToBoolean(TYPESCRIPT_THREE_PLUS));
        }

        if (additionalProperties.containsKey(SAGAS_AND_RECORDS)) {
            this.setSagasAndRecords(convertPropertyToBoolean(SAGAS_AND_RECORDS));
            if (this.getSagasAndRecords()) {
                apiTemplateFiles.put("sagas.mustache", "Sagas.ts");
                modelTemplateFiles.put("records.mustache", "Record.ts");
                supportingFiles.add(new SupportingFile("runtimeSagasAndRecords.mustache", sourceDir, "runtimeSagasAndRecords.ts"));
            }
        }
    }

    @Override
    protected ImmutableMap.Builder<String, Mustache.Lambda> addMustacheLambdas() {
        ImmutableMap.Builder<String, Mustache.Lambda> lambdas = super.addMustacheLambdas();
        lambdas.put("indented_star_1", new IndentedLambda(1, " ", "* "));
        lambdas.put("indented_star_4", new IndentedLambda(5, " ", "* "));
        return lambdas;
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isFileSchema(p)) {
            return "Blob";
        } else if (ModelUtils.isBinarySchema(p)) {
            return "Blob";
        }
        return super.getTypeDeclaration(p);
    }

    @Override
    protected void addAdditionPropertiesToCodeGenModel(CodegenModel codegenModel, Schema schema) {
        codegenModel.additionalPropertiesType = getTypeDeclaration(getAdditionalProperties(schema));
        addImport(codegenModel, codegenModel.additionalPropertiesType);
    }

    @Override
    protected void handleMethodResponse(Operation operation,
                                        Map<String, Schema> schemas,
                                        CodegenOperation op,
                                        ApiResponse methodResponse,
                                        Map<String, String> importMappings) {
        super.handleMethodResponse(operation, schemas, op, methodResponse, importMappings);

        if (this.getSagasAndRecords()) {
            op.returnTypeAlternate = "void";

            Schema schema = null;
            if (schemas != null) {
                schema = schemas.get(op.returnBaseType);
            }

            CodegenModel cm = null;
            if (schema != null) {
                cm = fromModel(op.returnBaseType, schema);

                if (Boolean.TRUE.equals(cm.vendorExtensions.get(X_IS_META_DATA_RESPONSE))) {
                    if (cm.vars.size() == 1 && "meta".equals(cm.vars.get(0).name)) {
                        op.returnTypeIsMetaOnlyResponse = true;
                    }
                    if (cm.vars.size() == 2 && "data".equals(cm.vars.get(1).name)) {
                        op.returnTypeIsMetaDataResponse = true;
                    }
                }
            }

            if (!op.returnTypeIsMetaOnlyResponse) {
                Schema responseSchema = unaliasSchema(ModelUtils.getSchemaFromResponse(methodResponse), importMapping);
                CodegenProperty cp = null;
                if (op.returnTypeIsMetaDataResponse && cm != null) {
                    cp = this.processCodeGenModel(cm).vars.get(1);
                } else if (responseSchema != null) {
                    cp = fromProperty("response", responseSchema);
                    this.processCodegenProperty(cp, "", null);
                }

                if (cp != null) {
                    op.returnTypeAlternate = cp.dataTypeAlternate;
                }
            }
        }
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        List<Object> models = (List<Object>) postProcessModelsEnum(objs).get("models");

        // process enum and custom properties in models
        for (Object _mo : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            CodegenModel cm = (CodegenModel) mo.get("model");
            cm.imports = new TreeSet(cm.imports);
            this.processCodeGenModel(cm);
        }

        return objs;
    }

    @Override
    public Map<String, Object> postProcessAllModels(Map<String, Object> objs) {
        List<CodegenModel> allModels = new ArrayList<CodegenModel>();
        List<String> entityModelClassnames = new ArrayList<String>();

        Map<String, Object> result = super.postProcessAllModels(objs);
        for (Map.Entry<String, Object> entry : result.entrySet()) {
            Map<String, Object> inner = (Map<String, Object>) entry.getValue();
            List<Map<String, Object>> models = (List<Map<String, Object>>) inner.get("models");
            for (Map<String, Object> model : models) {
                CodegenModel codegenModel = (CodegenModel) model.get("model");
                model.put("hasImports", codegenModel.imports.size() > 0);

                allModels.add(codegenModel);
                if (codegenModel.isEntity) {
                    entityModelClassnames.add(codegenModel.classname);
                }
            }
        }

        for (CodegenModel rootModel : allModels) {
            for (String curImport : rootModel.imports) {
                boolean isModelImport = false;
                for (CodegenModel model : allModels) {
                    if (model.classname.equals(curImport) && !model.isEnum) {
                        isModelImport = true;
                        break;
                    }
                }
                if (isModelImport) {
                    rootModel.modelImports.add(curImport);
                }
            }

            for (CodegenProperty var : rootModel.vars) {
                if (var.isModel && entityModelClassnames.indexOf(var.dataType) != -1) {
                    var.isEntity = true;
                } else if (var.itemsAreModels && entityModelClassnames.indexOf(var.items.dataType) != -1) {
                    var.itemsAreEntities = true;
                }
            }
        }
        return result;
    }

    private void autoSetDefaultValueForProperty(CodegenProperty var) {
        if (var.isListContainer || var.isModel) {
            var.defaultValue = var.dataTypeAlternate + "()";
        } else if (var.isUniqueId) {
            var.defaultValue = "\"-1\"";
        } else if (var.isEnum) {
            var.defaultValue = "'" + var._enum.get(0) + "'";
            updateCodegenPropertyEnum(var);
        } else if (var.dataType.equalsIgnoreCase("string")) {
            var.defaultValue = "\"\"";
        } else if (var.dataType.equalsIgnoreCase("integer")) {
            var.defaultValue = "0";
        }
    }

    private void addNpmPackageGeneration() {

        if (additionalProperties.containsKey(NPM_REPOSITORY)) {
            this.setNpmRepository(additionalProperties.get(NPM_REPOSITORY).toString());
        }

        //Files for building our lib
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("package.mustache", "", "package.json"));
        supportingFiles.add(new SupportingFile("tsconfig.mustache", "", "tsconfig.json"));
        supportingFiles.add(new SupportingFile("npmignore.mustache", "", ".npmignore"));
        supportingFiles.add(new SupportingFile("gitignore", "", ".gitignore"));
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> operations, List<Object> allModels) {
        // Add supporting file only if we plan to generate files in /apis
        if (operations.size() > 0 && !addedApiIndex) {
            addedApiIndex = true;
            supportingFiles.add(new SupportingFile("apis.index.mustache", apiPackage().replace('.', File.separatorChar), "index.ts"));
        }

        // Add supporting file only if we plan to generate files in /models
        if (allModels.size() > 0 && !addedModelIndex) {
            addedModelIndex = true;
            supportingFiles.add(new SupportingFile("models.index.mustache", modelPackage().replace('.', File.separatorChar), "index.ts"));
        }

        this.addOperationModelImportInfomation(operations);
        this.updateOperationParameterForEnum(operations);
        if (this.getSagasAndRecords()) {
            this.updateOperationParameterForSagaAndRecords(operations);
        }
        this.addOperationObjectResponseInformation(operations);
        this.addOperationPrefixParameterInterfacesInformation(operations);
        this.escapeOperationIds(operations);
        return operations;
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        Map<String, Object> parentObjs = super.postProcessSupportingFileData(objs);

        parentObjs.put("useSagaAndRecords", this.getSagasAndRecords());

        return parentObjs;
    }

    private CodegenModel processCodeGenModel(CodegenModel cm) {
        Object xEntityId = cm.vendorExtensions.get(X_ENTITY_ID);
        for (CodegenProperty var : cm.vars) {
            boolean parentIsEntity = this.processCodegenProperty(var, cm.classname, xEntityId);
            if (parentIsEntity) {
                cm.isEntity = true;
            };
        }

        if (Boolean.TRUE.equals(cm.vendorExtensions.get(X_IS_META_DATA_RESPONSE))) {
            if (cm.vars.size() == 1 && "meta".equals(cm.vars.get(0).name)) {
                cm.isMetaOnlyResponse = true;
            }
            if (cm.vars.size() == 2 && "data".equals(cm.vars.get(1).name)) {
                cm.isMetaDataResponse = true;
            }
        }

        if (cm.parent != null) {
            for (CodegenProperty var : cm.allVars) {
                if (Boolean.TRUE.equals(var.isEnum)) {
                    var.datatypeWithEnum = var.datatypeWithEnum
                            .replace(var.enumName, cm.classname + var.enumName);
                }
            }
        }
        if (!cm.oneOf.isEmpty()) {
            // For oneOfs only import $refs within the oneOf
            TreeSet<String> oneOfRefs = new TreeSet<>();
            for (String im : cm.imports) {
                if (cm.oneOf.contains(im)) {
                    oneOfRefs.add(im);
                }
            }
            cm.imports = oneOfRefs;
        }
        return cm;
    }

    private boolean processCodegenProperty(CodegenProperty var, String parentClassName, Object xEntityId) {
        boolean parentIsEntity = false;
        // name enum with model name, e.g. StatusEnum => PetStatusEnum
        if (Boolean.TRUE.equals(var.isEnum)) {
            // behaviour for enum names is specific for Typescript Fetch, not using namespaces
            var.datatypeWithEnum = var.datatypeWithEnum.replace(var.enumName, parentClassName + var.enumName);

            // need to post-process defaultValue, was computed with previous var.datatypeWithEnum
            if (var.defaultValue != null && !var.defaultValue.equals("undefined")) {
                int dotPos = var.defaultValue.indexOf(".");
                if (dotPos != -1) {
                    var.defaultValue = var.datatypeWithEnum + var.defaultValue.substring(dotPos);
                }
            }
        }

        var.isUniqueId = Boolean.TRUE.equals(var.vendorExtensions.get(X_IS_UNIQUE_ID));
        if (var.isUniqueId && xEntityId != null && xEntityId.equals(var.name)) {
            parentIsEntity = true;
        }

        var.itemsDataType = var.isListContainer ? var.items.dataType : null;
        var.itemsAreModels = var.isListContainer && var.items.isModel;

        if (this.getSagasAndRecords()) {
            var.dataTypeAlternate = var.dataType;
            if (var.isListContainer) {
                var.dataTypeAlternate = var.dataType.replace("Array<", "List<");
                if (var.items.isModel) {
                    var.itemsDataType = var.items.dataType + "Record";
                    var.dataTypeAlternate = var.dataTypeAlternate.replace(var.items.dataType, var.itemsDataType);
                } else if (var.items.isEnum) {
                    var.itemsDataType = var.items.datatypeWithEnum;
                    var.dataTypeAlternate = var.dataTypeAlternate.replace(var.items.dataType, var.itemsDataType);
                }
            } else if (var.isEnum) {
                var.dataTypeAlternate = var.datatypeWithEnum;
            } else if (var.isModel) {
                var.dataTypeAlternate = var.dataType + "Record";
            } else if (var.isUniqueId) {
                var.dataTypeAlternate = "string";
            }
            if (var.defaultValue == null || var.defaultValue.equals("undefined")) {
                this.autoSetDefaultValueForProperty(var);
            }
        }
        return parentIsEntity;
    }

    private void escapeOperationIds(Map<String, Object> operations) {
        Map<String, Object> _operations = (Map<String, Object>) operations.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) _operations.get("operation");
        for (CodegenOperation op : operationList) {
            String param = op.operationIdCamelCase + "Request";
            if (op.imports.contains(param)) {
                // we import a model with the same name as the generated operation, escape it
                op.operationIdCamelCase += "Operation";
                op.operationIdLowerCase += "operation";
                op.operationIdSnakeCase += "_operation";
            }
        }
    }

    private void addOperationModelImportInfomation(Map<String, Object> operations) {
        // This method will add extra infomation to the operations.imports array.
        // The api template uses this infomation to import all the required
        // models for a given operation.
        List<Map<String, Object>> imports = (List<Map<String, Object>>) operations.get("imports");
        for (Map<String, Object> im : imports) {
            im.put("className", im.get("import").toString().replace(modelPackage() + ".", ""));
        }
    }

    private void updateOperationParameterForEnum(Map<String, Object> operations) {
        // This method will add extra infomation as to whether or not we have enums and
        // update their names with the operation.id prefixed.
        // It will also set the uniqueId status if provided.
        Map<String, Object> _operations = (Map<String, Object>) operations.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) _operations.get("operation");
        boolean hasEnum = false;
        for (CodegenOperation op : operationList) {
            for (CodegenParameter param : op.allParams) {
                if (Boolean.TRUE.equals(param.isEnum)) {
                    hasEnum = true;
                    param.datatypeWithEnum = param.datatypeWithEnum
                            .replace(param.enumName, op.operationIdCamelCase + param.enumName);
                }
            }
        }

        operations.put("hasEnums", hasEnum);
    }

    private void updateOperationParameterForSagaAndRecords(Map<String, Object> operations) {
        // This method will add extra infomation as to whether or not we have enums and
        // update their names with the operation.id prefixed.
        // It will also set the uniqueId status if provided.
        Map<String, Object> _operations = (Map<String, Object>) operations.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) _operations.get("operation");
        for (CodegenOperation op : operationList) {
            for (CodegenParameter param : op.allParams) {
                if (Boolean.TRUE.equals(param.vendorExtensions.get(X_IS_UNIQUE_ID))) {
                    param.isUniqueId = true;
                }

                param.dataTypeAlternate = param.dataType;
                if (param.isListContainer) {
                    if (param.items.isModel) {
                        param.dataTypeAlternate = param.dataType.replace("Array<", "List<");
                        param.itemsDataType = param.items.dataType + "Record";
                        param.dataTypeAlternate = param.dataTypeAlternate.replace(param.items.dataType, param.itemsDataType);
                    } else if (param.items.isEnum) {
                        param.itemsDataType = param.datatypeWithEnum.substring(param.datatypeWithEnum.indexOf("<") + 1, param.datatypeWithEnum.lastIndexOf(">"));
                        param.dataTypeAlternate = param.datatypeWithEnum.replace("Array<", "List<");
                    }
                } else if (param.isEnum) {
                    param.dataTypeAlternate = param.datatypeWithEnum;
                } else if (param.isModel) {
                    param.dataTypeAlternate = param.dataType + "Record";
                } else if (param.isUniqueId) {
                    param.dataTypeAlternate = "string";
                }
            }
        }
    }

    private void addOperationObjectResponseInformation(Map<String, Object> operations) {
        // This method will modify the infomation on the operations' return type.
        // The api template uses this infomation to know when to return a text
        // response for a given simple response operation.
        Map<String, Object> _operations = (Map<String, Object>) operations.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) _operations.get("operation");
        for (CodegenOperation op : operationList) {
            if ("object".equals(op.returnType)) {
                op.isMapContainer = true;
                op.returnSimpleType = false;
            }
        }
    }

    private void addOperationPrefixParameterInterfacesInformation(Map<String, Object> operations) {
        Map<String, Object> _operations = (Map<String, Object>) operations.get("operations");
        operations.put("prefixParameterInterfaces", getPrefixParameterInterfaces());
    }

    private void addExtraReservedWords() {
        this.reservedWords.add("BASE_PATH");
        this.reservedWords.add("BaseAPI");
        this.reservedWords.add("RequiredError");
        this.reservedWords.add("COLLECTION_FORMATS");
        this.reservedWords.add("FetchAPI");
        this.reservedWords.add("ConfigurationParameters");
        this.reservedWords.add("Configuration");
        this.reservedWords.add("configuration");
        this.reservedWords.add("HTTPMethod");
        this.reservedWords.add("HTTPHeaders");
        this.reservedWords.add("HTTPQuery");
        this.reservedWords.add("HTTPBody");
        this.reservedWords.add("ModelPropertyNaming");
        this.reservedWords.add("FetchParams");
        this.reservedWords.add("RequestOpts");
        this.reservedWords.add("exists");
        this.reservedWords.add("RequestContext");
        this.reservedWords.add("ResponseContext");
        this.reservedWords.add("Middleware");
        this.reservedWords.add("ApiResponse");
        this.reservedWords.add("ResponseTransformer");
        this.reservedWords.add("JSONApiResponse");
        this.reservedWords.add("VoidApiResponse");
        this.reservedWords.add("BlobApiResponse");
        this.reservedWords.add("TextApiResponse");
        // "Index" would create a file "Index.ts" which on case insensitive filesystems
        // would override our "index.js" file
        this.reservedWords.add("Index");
    }

    private boolean getUseSingleRequestParameter() {
        return useSingleRequestParameter;
    }

    private void setUseSingleRequestParameter(boolean useSingleRequestParameter) {
        this.useSingleRequestParameter = useSingleRequestParameter;
    }

    private boolean getPrefixParameterInterfaces() {
        return prefixParameterInterfaces;
    }

    private void setPrefixParameterInterfaces(boolean prefixParameterInterfaces) {
        this.prefixParameterInterfaces = prefixParameterInterfaces;
    }

}
