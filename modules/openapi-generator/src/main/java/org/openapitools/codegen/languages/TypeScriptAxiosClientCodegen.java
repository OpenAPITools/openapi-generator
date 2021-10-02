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

import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.parser.util.SchemaTypeUtil;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.DocumentationFeature;
import org.openapitools.codegen.utils.ModelUtils;

import java.util.*;

public class TypeScriptAxiosClientCodegen extends AbstractTypeScriptClientCodegen {

    public static final String NPM_REPOSITORY = "npmRepository";
    public static final String WITH_INTERFACES = "withInterfaces";
    public static final String SEPARATE_MODELS_AND_API = "withSeparateModelsAndApi";
    public static final String WITHOUT_PREFIX_ENUMS = "withoutPrefixEnums";
    public static final String USE_SINGLE_REQUEST_PARAMETER = "useSingleRequestParameter";

    protected String npmRepository = null;

    private String tsModelPackage = "";

    public TypeScriptAxiosClientCodegen() {
        super();

        modifyFeatureSet(features -> features.includeDocumentationFeatures(DocumentationFeature.Readme));

        // clear import mapping (from default generator) as TS does not use it
        // at the moment
        importMapping.clear();

        outputFolder = "generated-code/typescript-axios";
        embeddedTemplateDir = templateDir = "typescript-axios";

        this.cliOptions.add(new CliOption(NPM_REPOSITORY, "Use this property to set an url of your private npmRepo in the package.json"));
        this.cliOptions.add(new CliOption(WITH_INTERFACES, "Setting this property to true will generate interfaces next to the default class implementations.", SchemaTypeUtil.BOOLEAN_TYPE).defaultValue(Boolean.FALSE.toString()));
        this.cliOptions.add(new CliOption(SEPARATE_MODELS_AND_API, "Put the model and api in separate folders and in separate classes", SchemaTypeUtil.BOOLEAN_TYPE).defaultValue(Boolean.FALSE.toString()));
        this.cliOptions.add(new CliOption(WITHOUT_PREFIX_ENUMS, "Don't prefix enum names with class names", SchemaTypeUtil.BOOLEAN_TYPE).defaultValue(Boolean.FALSE.toString()));
        this.cliOptions.add(new CliOption(USE_SINGLE_REQUEST_PARAMETER, "Setting this property to true will generate functions with a single argument containing all API endpoint parameters instead of one argument per parameter.", SchemaTypeUtil.BOOLEAN_TYPE).defaultValue(Boolean.FALSE.toString()));
        // Templates have no mapping between formatted property names and original base names so use only "original" and remove this option
        removeOption(CodegenConstants.MODEL_PROPERTY_NAMING);
    }

    @Override
    public String getName() {
        return "typescript-axios";
    }

    @Override
    public String getHelp() {
        return "Generates a TypeScript client library using axios.";
    }

    public String getNpmRepository() {
        return npmRepository;
    }

    public void setNpmRepository(String npmRepository) {
        this.npmRepository = npmRepository;
    }

    private static String getRelativeToRoot(String path) {
        StringBuilder sb = new StringBuilder();
        int slashCount = path.split("/").length;
        if (slashCount == 0) {
            sb.append("./");
        } else {
            for (int i = 0; i < slashCount; ++i) {
                sb.append("../");
            }
        }
        return sb.toString();
    }

    @Override
    public void processOpts() {
        super.processOpts();
        tsModelPackage = modelPackage.replaceAll("\\.", "/");
        String tsApiPackage = apiPackage.replaceAll("\\.", "/");

        String modelRelativeToRoot = getRelativeToRoot(tsModelPackage);
        String apiRelativeToRoot = getRelativeToRoot(tsApiPackage);

        additionalProperties.put("tsModelPackage", tsModelPackage);
        additionalProperties.put("tsApiPackage", tsApiPackage);
        additionalProperties.put("apiRelativeToRoot", apiRelativeToRoot);
        additionalProperties.put("modelRelativeToRoot", modelRelativeToRoot);

        supportingFiles.add(new SupportingFile("index.mustache", "", "index.ts"));
        supportingFiles.add(new SupportingFile("baseApi.mustache", "", "base.ts"));
        supportingFiles.add(new SupportingFile("common.mustache", "", "common.ts"));
        supportingFiles.add(new SupportingFile("api.mustache", "", "api.ts"));
        supportingFiles.add(new SupportingFile("configuration.mustache", "", "configuration.ts"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("gitignore", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("npmignore", "", ".npmignore"));

        if (additionalProperties.containsKey(SEPARATE_MODELS_AND_API)) {
            boolean separateModelsAndApi = Boolean.parseBoolean(additionalProperties.get(SEPARATE_MODELS_AND_API).toString());
            if (separateModelsAndApi) {
                if (StringUtils.isAnyBlank(modelPackage, apiPackage)) {
                    throw new RuntimeException("apiPackage and modelPackage must be defined");
                }
                modelTemplateFiles.put("model.mustache", ".ts");
                apiTemplateFiles.put("apiInner.mustache", ".ts");
                supportingFiles.add(new SupportingFile("modelIndex.mustache", tsModelPackage, "index.ts"));
            }
        }

        if (additionalProperties.containsKey(NPM_NAME)) {
            addNpmPackageGeneration();
        }

    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        objs = super.postProcessOperationsWithModels(objs, allModels);
        Map<String, Object> vals = (Map<String, Object>) objs.getOrDefault("operations", new HashMap<>());
        List<CodegenOperation> operations = (List<CodegenOperation>) vals.getOrDefault("operation", new ArrayList<>());
        /*
            Filter all the operations that are multipart/form-data operations and set the vendor extension flag
            'multipartFormData' for the template to work with.
         */
        operations.stream()
                .filter(op -> op.hasConsumes)
                .filter(op -> op.consumes.stream().anyMatch(opc -> opc.values().stream().anyMatch("multipart/form-data"::equals)))
                .forEach(op -> {
                    op.vendorExtensions.putIfAbsent("multipartFormData", true);
                });
        return objs;
    }

    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        super.postProcessParameter(parameter);
        if (parameter.isFormParam && parameter.isArray && "binary".equals(parameter.dataFormat)) {
            parameter.isCollectionFormatMulti = true;
        }
    }

    @Override
    public Map<String, Object> postProcessAllModels(Map<String, Object> objs) {
        Map<String, Object> result = super.postProcessAllModels(objs);
        for (Map.Entry<String, Object> entry : result.entrySet()) {
            Map<String, Object> inner = (Map<String, Object>) entry.getValue();
            List<Map<String, Object>> models = (List<Map<String, Object>>) inner.get("models");
            for (Map<String, Object> model : models) {
                CodegenModel codegenModel = (CodegenModel) model.get("model");
                model.put("hasAllOf", codegenModel.allOf.size() > 0);
                model.put("hasOneOf", codegenModel.oneOf.size() > 0);
            }
        }
        return result;
    }


    @Override
    protected void addAdditionPropertiesToCodeGenModel(CodegenModel codegenModel, Schema schema) {
        codegenModel.additionalPropertiesType = getTypeDeclaration(getAdditionalProperties(schema));
        addImport(codegenModel, codegenModel.additionalPropertiesType);
    }

    @Override
    @SuppressWarnings("unchecked")
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        List<Object> models = (List<Object>) postProcessModelsEnum(objs).get("models");

        boolean withoutPrefixEnums = false;
        if (additionalProperties.containsKey(WITHOUT_PREFIX_ENUMS)) {
            withoutPrefixEnums = Boolean.parseBoolean(additionalProperties.get(WITHOUT_PREFIX_ENUMS).toString());
        }

        for (Object _mo  : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            CodegenModel cm = (CodegenModel) mo.get("model");

            // Deduce the model file name in kebab case
            cm.classFilename = cm.classname.replaceAll("([a-z0-9])([A-Z])", "$1-$2").toLowerCase(Locale.ROOT);

            //processed enum names
            if(!withoutPrefixEnums) {
                cm.imports = new TreeSet(cm.imports);
                // name enum with model name, e.g. StatusEnum => PetStatusEnum
                for (CodegenProperty var : cm.vars) {
                    if (Boolean.TRUE.equals(var.isEnum)) {
                        var.datatypeWithEnum = var.datatypeWithEnum.replace(var.enumName, cm.classname + var.enumName);
                        var.enumName = var.enumName.replace(var.enumName, cm.classname + var.enumName);
                    }
                }
                if (cm.parent != null) {
                    for (CodegenProperty var : cm.allVars) {
                        if (Boolean.TRUE.equals(var.isEnum)) {
                            var.datatypeWithEnum = var.datatypeWithEnum.replace(var.enumName, cm.classname + var.enumName);
                            var.enumName = var.enumName.replace(var.enumName, cm.classname + var.enumName);
                        }
                    }
                }
            }
        }

        // Apply the model file name to the imports as well
        for (Map<String, String> m : (List<Map<String, String>>) objs.get("imports")) {
            String javaImport = m.get("import").substring(modelPackage.length() + 1);
            String tsImport = tsModelPackage + "/" + javaImport;
            m.put("tsImport", tsImport);
            m.put("class", javaImport);
            m.put("filename", javaImport.replaceAll("([a-z0-9])([A-Z])", "$1-$2").toLowerCase(Locale.ROOT));
        }
        return objs;
    }

    /**
     * Overriding toRegularExpression() to avoid escapeText() being called,
     * as it would return a broken regular expression if any escaped character / metacharacter were present.
     */
    @Override
    public String toRegularExpression(String pattern) {
        return addRegularExpressionDelimiter(pattern);
    }

    @Override
    public String toModelFilename(String name) {
        return super.toModelFilename(name).replaceAll("([a-z0-9])([A-Z])", "$1-$2").toLowerCase(Locale.ROOT);
    }

    @Override
    public String toApiFilename(String name) {
        return super.toApiFilename(name).replaceAll("([a-z0-9])([A-Z])", "$1-$2").toLowerCase(Locale.ROOT);
    }

    private void addNpmPackageGeneration() {

        if (additionalProperties.containsKey(NPM_REPOSITORY)) {
            this.setNpmRepository(additionalProperties.get(NPM_REPOSITORY).toString());
        }

        //Files for building our lib
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("package.mustache", "", "package.json"));
        supportingFiles.add(new SupportingFile("tsconfig.mustache", "", "tsconfig.json"));
    }

    @Override
    protected void updatePropertyForAnyType(CodegenProperty property, Schema p) {
        // The 'null' value is allowed when the OAS schema is 'any type'.
        // See https://github.com/OAI/OpenAPI-Specification/issues/1389
        // custom line here, do not set property.isNullable = true
        if (languageSpecificPrimitives.contains(property.dataType)) {
            property.isPrimitiveType = true;
        }
        if (ModelUtils.isMapSchema(p)) {
            // an object or anyType composed schema that has additionalProperties set
            // some of our code assumes that any type schema with properties defined will be a map
            // even though it should allow in any type and have map constraints for properties
            updatePropertyForMap(property, p);
        }
    }
}
