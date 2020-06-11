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
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.parser.util.SchemaTypeUtil;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.DocumentationFeature;
import org.openapitools.codegen.templating.mustache.IndentedLambda;
import org.openapitools.codegen.utils.ModelUtils;

import java.io.File;
import java.util.TreeSet;
import java.util.List;
import java.util.Map;

public class TypeScriptFetchClientCodegen extends AbstractTypeScriptClientCodegen {

    public static final String NPM_REPOSITORY = "npmRepository";
    public static final String WITH_INTERFACES = "withInterfaces";
    public static final String USE_SINGLE_REQUEST_PARAMETER = "useSingleRequestParameter";
    public static final String PREFIX_PARAMETER_INTERFACES = "prefixParameterInterfaces";
    public static final String TYPESCRIPT_THREE_PLUS = "typescriptThreePlus";

    protected String npmRepository = null;
    private boolean useSingleRequestParameter = true;
    private boolean prefixParameterInterfaces = false;
    protected boolean addedApiIndex = false;
    protected boolean addedModelIndex = false;
    protected boolean typescriptThreePlus = false;


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
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        List<Object> models = (List<Object>) postProcessModelsEnum(objs).get("models");

        // process enum in models
        for (Object _mo : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            CodegenModel cm = (CodegenModel) mo.get("model");
            cm.imports = new TreeSet(cm.imports);
            // name enum with model name, e.g. StatusEnum => Pet.StatusEnum
            for (CodegenProperty var : cm.vars) {
                if (Boolean.TRUE.equals(var.isEnum)) {
                    // behaviour for enum names is specific for Typescript Fetch, not using namespaces
                    var.datatypeWithEnum = var.datatypeWithEnum.replace(var.enumName, cm.classname + var.enumName);
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
        }

        return objs;
    }

    @Override
    public Map<String, Object> postProcessAllModels(Map<String, Object> objs) {
        Map<String, Object> result = super.postProcessAllModels(objs);
        for (Map.Entry<String, Object> entry : result.entrySet()) {
            Map<String, Object> inner = (Map<String, Object>) entry.getValue();
            List<Map<String, Object>> models = (List<Map<String, Object>>) inner.get("models");
            for (Map<String, Object> model : models) {
                CodegenModel codegenModel = (CodegenModel) model.get("model");
                model.put("hasImports", codegenModel.imports.size() > 0);
            }
        }
        return result;
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
        this.updateOperationParameterEnumInformation(operations);
        this.addOperationObjectResponseInformation(operations);
        this.addOperationPrefixParameterInterfacesInformation(operations);
        this.escapeOperationIds(operations);
        this.addDeepObjectVendorExtension(operations);
        return operations;
    }

    private void addDeepObjectVendorExtension(Map<String, Object> operations) {
        Map<String, Object> _operations = (Map<String, Object>) operations.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) _operations.get("operation");

        for (CodegenOperation op : operationList) {
            for (CodegenParameter param : op.queryParams) {
                if (param.style != null && param.style.equals("deepObject")) {
                    param.vendorExtensions.put("x-codegen-isDeepObject", true);
                }
            }
        }
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

    private void updateOperationParameterEnumInformation(Map<String, Object> operations) {
        // This method will add extra infomation as to whether or not we have enums and
        // update their names with the operation.id prefixed.
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

    private void addOperationObjectResponseInformation(Map<String, Object> operations) {
        // This method will modify the infomation on the operations' return type.
        // The api template uses this infomation to know when to return a text
        // response for a given simple response operation.
        Map<String, Object> _operations = (Map<String, Object>) operations.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) _operations.get("operation");
        for (CodegenOperation op : operationList) {
            if("object".equals(op.returnType)) {
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
