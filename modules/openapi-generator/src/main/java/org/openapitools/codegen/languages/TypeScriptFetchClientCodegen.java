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

import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.parser.util.SchemaTypeUtil;
import org.openapitools.codegen.*;
import org.openapitools.codegen.utils.ModelUtils;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.TreeSet;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Map;

public class TypeScriptFetchClientCodegen extends AbstractTypeScriptClientCodegen {

    public static final String NPM_NAME = "npmName";
    public static final String NPM_VERSION = "npmVersion";
    public static final String NPM_REPOSITORY = "npmRepository";
    public static final String SNAPSHOT = "snapshot";
    public static final String WITH_INTERFACES = "withInterfaces";

    protected String npmName = null;
    protected String npmVersion = "1.0.0";
    protected String npmRepository = null;

    public TypeScriptFetchClientCodegen() {
        super();

        // clear import mapping (from default generator) as TS does not use it
        // at the moment
        importMapping.clear();

        outputFolder = "generated-code/typescript-fetch";
        embeddedTemplateDir = templateDir = "typescript-fetch";

        this.apiPackage = "apis";
        this.apiTemplateFiles.put("apis.mustache", ".ts");
        this.modelPackage = "models";
        this.modelTemplateFiles.put("models.mustache", ".ts");
        this.addExtraReservedWords();

        this.cliOptions.add(new CliOption(NPM_NAME, "The name under which you want to publish generated npm package"));
        this.cliOptions.add(new CliOption(NPM_VERSION, "The version of your npm package"));
        this.cliOptions.add(new CliOption(NPM_REPOSITORY, "Use this property to set an url your private npmRepo in the package.json"));
        this.cliOptions.add(new CliOption(SNAPSHOT, "When setting this property to true the version will be suffixed with -SNAPSHOT.yyyyMMddHHmm", SchemaTypeUtil.BOOLEAN_TYPE).defaultValue(Boolean.FALSE.toString()));
        this.cliOptions.add(new CliOption(WITH_INTERFACES, "Setting this property to true will generate interfaces next to the default class implementations.", SchemaTypeUtil.BOOLEAN_TYPE).defaultValue(Boolean.FALSE.toString()));
    }

    @Override
    public String getName() {
        return "typescript-fetch";
    }

    @Override
    public String getHelp() {
        return "Generates a TypeScript client library using Fetch API (beta).";
    }

    public String getNpmName() {
        return npmName;
    }

    public void setNpmName(String npmName) {
        this.npmName = npmName;
    }

    public String getNpmVersion() {
        return npmVersion;
    }

    public void setNpmVersion(String npmVersion) {
        this.npmVersion = npmVersion;
    }

    public String getNpmRepository() {
        return npmRepository;
    }

    public void setNpmRepository(String npmRepository) {
        this.npmRepository = npmRepository;
    }

    @Override
    public void processOpts() {
        super.processOpts();
        additionalProperties.put("isOriginalModelPropertyNaming", getModelPropertyNaming().equals("original"));
        additionalProperties.put("modelPropertyNaming", getModelPropertyNaming());
        supportingFiles.add(new SupportingFile("index.mustache", "", "index.ts"));
        supportingFiles.add(new SupportingFile("runtime.mustache", "", "runtime.ts"));
        supportingFiles.add(new SupportingFile("apis.index.mustache", apiPackage().replace('.', File.separatorChar), "index.ts"));
        supportingFiles.add(new SupportingFile("models.index.mustache", modelPackage().replace('.', File.separatorChar), "index.ts"));
        supportingFiles.add(new SupportingFile("tsconfig.mustache", "", "tsconfig.json"));
        if (additionalProperties.containsKey(NPM_NAME)) {
            addNpmPackageGeneration();
        }
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        Schema inner;
        if (ModelUtils.isArraySchema(p)) {
            inner = ((ArraySchema) p).getItems();
            return this.getSchemaType(p) + "<" + this.getTypeDeclaration(inner) + ">";
        } else if (ModelUtils.isMapSchema(p)) {
            inner = ModelUtils.getAdditionalProperties(p);
            return "{ [key: string]: " + this.getTypeDeclaration(inner) + "; }";
        } else if (ModelUtils.isFileSchema(p)) {
            return "Blob";
        } else if (ModelUtils.isBinarySchema(p)) {
            return "Blob";
        } else if (ModelUtils.isDateSchema(p)) {
            return "Date";
        } else if (ModelUtils.isDateTimeSchema(p)) {
            return "Date";
        }
        return super.getTypeDeclaration(p);
    }

    @Override
    protected void addAdditionPropertiesToCodeGenModel(CodegenModel codegenModel, Schema schema) {
        codegenModel.additionalPropertiesType = getTypeDeclaration(ModelUtils.getAdditionalProperties(schema));
        addImport(codegenModel, codegenModel.additionalPropertiesType);
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        // process enum in models
        List<Object> models = (List<Object>) postProcessModelsEnum(objs).get("models");
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
        if (additionalProperties.containsKey(NPM_NAME)) {
            this.setNpmName(additionalProperties.get(NPM_NAME).toString());
        }

        if (additionalProperties.containsKey(NPM_VERSION)) {
            this.setNpmVersion(additionalProperties.get(NPM_VERSION).toString());
        }

        if (additionalProperties.containsKey(SNAPSHOT) && Boolean.valueOf(additionalProperties.get(SNAPSHOT).toString())) {
            if (npmVersion.toUpperCase(Locale.ROOT).matches("^.*-SNAPSHOT$")) {
                this.setNpmVersion(npmVersion + "." + SNAPSHOT_SUFFIX_FORMAT.format(new Date()));
            }
            else {
                this.setNpmVersion(npmVersion + "-SNAPSHOT." + SNAPSHOT_SUFFIX_FORMAT.format(new Date()));
            }
        }
        additionalProperties.put(NPM_VERSION, npmVersion);

        if (additionalProperties.containsKey(NPM_REPOSITORY)) {
            this.setNpmRepository(additionalProperties.get(NPM_REPOSITORY).toString());
        }

        //Files for building our lib
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("package.mustache", "", "package.json"));
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> operations, List<Object> allModels) {
        this.addOperationModelImportInfomation(operations);
        this.updateOperationParameterEnumInformation(operations);
        return operations;
    }

    private void addOperationModelImportInfomation(Map<String, Object> operations) {
        // This method will add extra infomation to the operations.imports array.
        // The api template uses this infomation to import all the required
        // models for a given operation.
        List<Map<String, Object>> imports = (List<Map<String, Object>>) operations.get("imports");
        for (Map<String, Object> im : imports) {
            im.put("className", im.get("import").toString().replace("models.", ""));
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

    private void addExtraReservedWords() {
        this.reservedWords.add("BASE_PATH");
        this.reservedWords.add("BaseAPI");
        this.reservedWords.add("RequiredError");
        this.reservedWords.add("COLLECTION_FORMATS");
        this.reservedWords.add("FetchAPI");
        this.reservedWords.add("ConfigurationParameters");
        this.reservedWords.add("Configuration");
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
    }
}
