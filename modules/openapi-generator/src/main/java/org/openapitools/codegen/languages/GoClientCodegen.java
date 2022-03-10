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

import com.google.common.collect.Iterables;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.security.SecurityScheme;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.ProcessUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public class GoClientCodegen extends AbstractGoCodegen {

    private final Logger LOGGER = LoggerFactory.getLogger(GoClientCodegen.class);
    protected String packageVersion = "1.0.0";
    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";
    public static final String WITH_XML = "withXml";
    public static final String STRUCT_PREFIX = "structPrefix";
    public static final String WITH_AWSV4_SIGNATURE = "withAWSV4Signature";
    public static final String GENERATE_INTERFACES = "generateInterfaces";
    protected String goImportAlias = "openapiclient";
    protected boolean isGoSubmodule = false;
    protected boolean useOneOfDiscriminatorLookup = false; // use oneOf discriminator's mapping for model lookup

    // A cache to efficiently lookup schema `toModelName()` based on the schema Key
    private Map<String, String> schemaKeyToModelNameCache = new HashMap<>();

    public GoClientCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML))
                .securityFeatures(EnumSet.of(
                        SecurityFeature.BasicAuth,
                        SecurityFeature.ApiKey,
                        SecurityFeature.OAuth2_Implicit
                ))
                .includeGlobalFeatures(
                        GlobalFeature.ParameterizedServer
                )
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling
                )
                .excludeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
                .includeParameterFeatures(
                        ParameterFeature.Cookie
                )
                .includeClientModificationFeatures(
                        ClientModificationFeature.BasePath,
                        ClientModificationFeature.UserAgent
                )
        );

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata).stability(Stability.STABLE).build();

        outputFolder = "generated-code/go";
        embeddedTemplateDir = templateDir = "go";
        usesOptionals = false;

        apiTemplateFiles.put("api.mustache", ".go");
        modelTemplateFiles.put("model.mustache", ".go");
        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");

        // default HIDE_GENERATION_TIMESTAMP to true
        hideGenerationTimestamp = Boolean.TRUE;

        cliOptions.add(CliOption.newBoolean(CodegenConstants.IS_GO_SUBMODULE, CodegenConstants.IS_GO_SUBMODULE_DESC));
        cliOptions.add(CliOption.newBoolean(WITH_XML, "whether to include support for application/xml content type and include XML annotations in the model (works with libraries that provide support for JSON and XML)"));
        cliOptions.add(CliOption.newBoolean(CodegenConstants.ENUM_CLASS_PREFIX, CodegenConstants.ENUM_CLASS_PREFIX_DESC));
        cliOptions.add(CliOption.newBoolean(STRUCT_PREFIX, "whether to prefix struct with the class name. e.g. DeletePetOpts => PetApiDeletePetOpts"));
        cliOptions.add(CliOption.newBoolean(WITH_AWSV4_SIGNATURE, "whether to include AWS v4 signature support"));
        cliOptions.add(CliOption.newBoolean(GENERATE_INTERFACES, "Generate interfaces for api classes"));

        // option to change the order of form/body parameter
        cliOptions.add(CliOption.newBoolean(
                        CodegenConstants.PREPEND_FORM_OR_BODY_PARAMETERS,
                        CodegenConstants.PREPEND_FORM_OR_BODY_PARAMETERS_DESC)
                .defaultValue(Boolean.FALSE.toString()));

        cliOptions.add(new CliOption(CodegenConstants.USE_ONEOF_DISCRIMINATOR_LOOKUP, CodegenConstants.USE_ONEOF_DISCRIMINATOR_LOOKUP_DESC).defaultValue("false"));
        // option to change how we process + set the data in the 'additionalProperties' keyword.
        CliOption disallowAdditionalPropertiesIfNotPresentOpt = CliOption.newBoolean(
                CodegenConstants.DISALLOW_ADDITIONAL_PROPERTIES_IF_NOT_PRESENT,
                CodegenConstants.DISALLOW_ADDITIONAL_PROPERTIES_IF_NOT_PRESENT_DESC).defaultValue(Boolean.TRUE.toString());
        Map<String, String> disallowAdditionalPropertiesIfNotPresentOpts = new HashMap<>();
        disallowAdditionalPropertiesIfNotPresentOpts.put("false",
                "The 'additionalProperties' implementation is compliant with the OAS and JSON schema specifications.");
        disallowAdditionalPropertiesIfNotPresentOpts.put("true",
                "Keep the old (incorrect) behaviour that 'additionalProperties' is set to false by default.");
        disallowAdditionalPropertiesIfNotPresentOpt.setEnum(disallowAdditionalPropertiesIfNotPresentOpts);
        cliOptions.add(disallowAdditionalPropertiesIfNotPresentOpt);
        this.setDisallowAdditionalPropertiesIfNotPresent(true);
    }

    /**
     * Configures a friendly name for the generator. This will be used by the
     * generator to select the library with the -g flag.
     *
     * @return the friendly name for the generator
     */
    @Override
    public String getName() {
        return "go";
    }

    /**
     * Configures the type of generator.
     *
     * @return the CodegenType for this generator
     * @see org.openapitools.codegen.CodegenType
     */
    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String toGetter(String name) {
        return "Get" + getterAndSetterCapitalize(name);
    }

    /**
     * Returns human-friendly help for the generator. Provide the consumer with help
     * tips, parameters here
     *
     * @return A string value for the help message
     */
    @Override
    public String getHelp() {
        return "Generates a Go client library.";
    }

    @Override
    public void processOpts() {
        this.setLegacyDiscriminatorBehavior(false);
        super.processOpts();

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
        } else {
            setPackageName("openapi");
        }

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_VERSION)) {
            setPackageVersion((String) additionalProperties.get(CodegenConstants.PACKAGE_VERSION));
        } else {
            setPackageVersion("1.0.0");
        }

        additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        additionalProperties.put(CodegenConstants.PACKAGE_VERSION, packageVersion);

        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);

        modelPackage = packageName;
        apiPackage = packageName;

        if (additionalProperties.containsKey(WITH_AWSV4_SIGNATURE)) {
            setWithAWSV4Signature(Boolean.parseBoolean(additionalProperties.get(WITH_AWSV4_SIGNATURE).toString()));
            additionalProperties.put(WITH_AWSV4_SIGNATURE, withAWSV4Signature);
        }

        if (additionalProperties.containsKey(WITH_XML)) {
            setWithXml(Boolean.parseBoolean(additionalProperties.get(WITH_XML).toString()));
            additionalProperties.put(WITH_XML, withXml);
        }

        if (additionalProperties.containsKey(CodegenConstants.ENUM_CLASS_PREFIX)) {
            setEnumClassPrefix(Boolean.parseBoolean(additionalProperties.get(CodegenConstants.ENUM_CLASS_PREFIX).toString()));
            additionalProperties.put(CodegenConstants.ENUM_CLASS_PREFIX, enumClassPrefix);
        }

        if (additionalProperties.containsKey(CodegenConstants.IS_GO_SUBMODULE)) {
            setIsGoSubmodule(Boolean.parseBoolean(additionalProperties.get(CodegenConstants.IS_GO_SUBMODULE).toString()));
            additionalProperties.put(CodegenConstants.IS_GO_SUBMODULE, isGoSubmodule);
        }

        if (additionalProperties.containsKey(STRUCT_PREFIX)) {
            setStructPrefix(Boolean.parseBoolean(additionalProperties.get(STRUCT_PREFIX).toString()));
            additionalProperties.put(STRUCT_PREFIX, structPrefix);
        }

        if (additionalProperties.containsKey(GENERATE_INTERFACES)) {
            setGenerateInterfaces(Boolean.parseBoolean(additionalProperties.get(GENERATE_INTERFACES).toString()));
            additionalProperties.put(GENERATE_INTERFACES, generateInterfaces);
        }

        // Generate the 'signing.py' module, but only if the 'HTTP signature' security scheme is specified in the OAS.
        Map<String, SecurityScheme> securitySchemeMap = openAPI != null ?
                (openAPI.getComponents() != null ? openAPI.getComponents().getSecuritySchemes() : null) : null;
        List<CodegenSecurity> authMethods = fromSecurity(securitySchemeMap);
        if (ProcessUtils.hasHttpSignatureMethods(authMethods)) {
            supportingFiles.add(new SupportingFile("signing.mustache", "", "signing.go"));
        }

        if (additionalProperties.containsKey("goImportAlias")) {
            setGoImportAlias(additionalProperties.get("goImportAlias").toString());
        } else {
            additionalProperties.put("goImportAlias", goImportAlias);
        }

        if (additionalProperties.containsKey(CodegenConstants.USE_ONEOF_DISCRIMINATOR_LOOKUP)) {
            setUseOneOfDiscriminatorLookup(convertPropertyToBooleanAndWriteBack(CodegenConstants.USE_ONEOF_DISCRIMINATOR_LOOKUP));
        } else {
            additionalProperties.put(CodegenConstants.USE_ONEOF_DISCRIMINATOR_LOOKUP, getUseOneOfDiscriminatorLookup());
        }

        if (additionalProperties.containsKey(CodegenConstants.DISALLOW_ADDITIONAL_PROPERTIES_IF_NOT_PRESENT)) {
            this.setDisallowAdditionalPropertiesIfNotPresent(Boolean.parseBoolean(additionalProperties
                    .get(CodegenConstants.DISALLOW_ADDITIONAL_PROPERTIES_IF_NOT_PRESENT).toString()));
        }

        supportingFiles.add(new SupportingFile("openapi.mustache", "api", "openapi.yaml"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("configuration.mustache", "", "configuration.go"));
        supportingFiles.add(new SupportingFile("client.mustache", "", "client.go"));
        supportingFiles.add(new SupportingFile("response.mustache", "", "response.go"));
        supportingFiles.add(new SupportingFile("go.mod.mustache", "", "go.mod"));
        supportingFiles.add(new SupportingFile("go.sum", "", "go.sum"));
        supportingFiles.add(new SupportingFile(".travis.yml", "", ".travis.yml"));
        supportingFiles.add(new SupportingFile("utils.mustache", "", "utils.go"));
    }

    public void setUseOneOfDiscriminatorLookup(boolean useOneOfDiscriminatorLookup) {
        this.useOneOfDiscriminatorLookup = useOneOfDiscriminatorLookup;
    }

    public boolean getUseOneOfDiscriminatorLookup() {
        return this.useOneOfDiscriminatorLookup;
    }

    public void setGoImportAlias(String goImportAlias) {
        this.goImportAlias = goImportAlias;
    }

    public void setPackageVersion(String packageVersion) {
        this.packageVersion = packageVersion;
    }

    public void setIsGoSubmodule(boolean isGoSubmodule) {
        this.isGoSubmodule = isGoSubmodule;
    }

    /**
     * Location to write api files.  You can use the apiPackage() as defined when the class is
     * instantiated
     */
    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator;
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator;
    }

    @Override
    public String apiDocFileFolder() {
        return (outputFolder + "/" + apiDocPath).replace('/', File.separatorChar);
    }

    @Override
    public String modelDocFileFolder() {
        return (outputFolder + "/" + modelDocPath).replace('/', File.separatorChar);
    }

    @Override
    public String toModelDocFilename(String name) {
        return toModelName(name);
    }

    @Override
    public String toApiDocFilename(String name) {
        return toApiName(name);
    }


    @Override
    public String toModelName(String name) {
        if (schemaKeyToModelNameCache.containsKey(name)) {
            return schemaKeyToModelNameCache.get(name);
        }

        // underscoring would also lowercase the whole name, thus losing acronyms which are in capitals
        String camelizedName = camelize(toModel(name, false));
        schemaKeyToModelNameCache.put(name, camelizedName);
        return camelizedName;
    }

    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return name + '_';
    }

    @Override
    public String toEnumDefaultValue(String value, String datatype) {
        String prefix = "";
        if (enumClassPrefix) {
            prefix = datatype.toUpperCase(Locale.ROOT) + "_";
        }
        return prefix + value;
    }

    @Override
    public void updateCodegenPropertyEnum(CodegenProperty var) {
        // make sure the inline enums have plain defaults (e.g. string, int, float)
        String enumDefault = null;
        if (var.isEnum && var.defaultValue != null) {
            enumDefault = var.defaultValue;
        }
        super.updateCodegenPropertyEnum(var);
        if (var.isEnum && enumDefault != null) {
            var.defaultValue = enumDefault;
        }
    }

    @Override
    public String toDefaultValue(Schema p) {
        p = ModelUtils.getReferencedSchema(this.openAPI, p);
        if (ModelUtils.isStringSchema(p)) {
            Object defaultObj = p.getDefault();
            if (defaultObj != null) {
                if (defaultObj instanceof java.lang.String) {
                    return "\"" + escapeText((String) defaultObj) + "\"";
                }
                return "\"" + escapeText(defaultObj.toString()) + "\"";
            }
            return null;
        }

        return super.toDefaultValue(p);
    }

    @Override
    public CodegenProperty fromProperty(String name, Schema p) {
        CodegenProperty prop = super.fromProperty(name, p);
        String cc = camelize(prop.name, true);
        if (isReservedWord(cc)) {
            cc = escapeReservedWord(cc);
        }
        prop.nameInCamelCase = cc;
        return prop;
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        // The superclass determines the list of required golang imports. The actual list of imports
        // depends on which types are used, some of which are changed in the code below (but then preserved
        // and used through x-go-base-type in templates). So super.postProcessModels
        // must be invoked at the beginning of this method.
        objs = super.postProcessModels(objs);

        List<Map<String, String>> imports = (List<Map<String, String>>) objs.get("imports");

        List<Map<String, Object>> models = (List<Map<String, Object>>) objs.get("models");
        for (Map<String, Object> m : models) {
            Object v = m.get("model");
            if (v instanceof CodegenModel) {
                CodegenModel model = (CodegenModel) v;
                if (model.isEnum) {
                    continue;
                }

                for (CodegenProperty param : Iterables.concat(model.vars, model.allVars, model.requiredVars, model.optionalVars)) {
                    param.vendorExtensions.put("x-go-base-type", param.dataType);
                    if (!param.isNullable || param.isContainer || param.isFreeFormObject
                            || (param.isAnyType && !param.isModel)) {
                        continue;
                    }
                    if (param.isDateTime) {
                        // Note this could have been done by adding the following line in processOpts(),
                        // however, we only want to represent the DateTime object as NullableTime if
                        // it's marked as nullable in the spec.
                        //    typeMapping.put("DateTime", "NullableTime");
                        param.dataType = "NullableTime";
                    } else {
                        param.dataType = "Nullable" + Character.toUpperCase(param.dataType.charAt(0))
                                + param.dataType.substring(1);
                    }
                }

                // additional import for different cases
                // oneOf
                if (model.oneOf != null && !model.oneOf.isEmpty()) {
                    imports.add(createMapping("import", "fmt"));
                }

                // anyOf
                if (model.anyOf != null && !model.anyOf.isEmpty()) {
                    imports.add(createMapping("import", "fmt"));
                }

                // additionalProperties: true and parent
                if (model.isAdditionalPropertiesTrue && model.parent != null && Boolean.FALSE.equals(model.isMap)) {
                    imports.add(createMapping("import", "reflect"));
                    imports.add(createMapping("import", "strings"));
                }

            }
        }
        return objs;
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        objs = super.postProcessOperationsWithModels(objs, allModels);
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        HashMap<String, CodegenModel> modelMaps = new HashMap<String, CodegenModel>();
        HashMap<String, Integer> processedModelMaps = new HashMap<String, Integer>();

        for (Object o : allModels) {
            HashMap<String, Object> h = (HashMap<String, Object>) o;
            CodegenModel m = (CodegenModel) h.get("model");
            modelMaps.put(m.classname, m);
        }

        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");
        for (CodegenOperation op : operationList) {
            for (CodegenParameter p : op.allParams) {
                p.vendorExtensions.put("x-go-example", constructExampleCode(p, modelMaps, processedModelMaps));
            }
            processedModelMaps.clear();
        }

        for (CodegenOperation operation : operationList) {
            for (CodegenParameter cp : operation.allParams) {
                cp.vendorExtensions.put("x-go-example", constructExampleCode(cp, modelMaps, processedModelMaps));
            }
            if (processedModelMaps.containsKey("time.Time")) {
                operation.vendorExtensions.put("x-go-import", "    \"time\"");
            }
            processedModelMaps.clear();
        }

        return objs;
    }

    private String constructExampleCode(CodegenParameter codegenParameter, HashMap<String, CodegenModel> modelMaps, HashMap<String, Integer> processedModelMap) {
        if (codegenParameter.isArray) { // array
            String prefix = codegenParameter.dataType;
            String dataType = StringUtils.removeStart(codegenParameter.dataType, "[]");
            if (modelMaps.containsKey(dataType)) {
                prefix = "[]" + goImportAlias + "." + dataType;
            }
            return prefix + "{" + constructExampleCode(codegenParameter.items, modelMaps, processedModelMap) + "}";
        } else if (codegenParameter.isMap) {
            String prefix = codegenParameter.dataType;
            String dataType = StringUtils.removeStart(codegenParameter.dataType, "map[string][]");
            if (modelMaps.containsKey(dataType)) {
                prefix = "map[string][]" + goImportAlias + "." + dataType;
            }
            if (codegenParameter.items == null) {
                return prefix + "{ ... }";
            }
            return prefix + "{\"key\": " + constructExampleCode(codegenParameter.items, modelMaps, processedModelMap) + "}";
        } else if (codegenParameter.isPrimitiveType) { // primitive type
            if (codegenParameter.isString) {
                if (!StringUtils.isEmpty(codegenParameter.example) && !"null".equals(codegenParameter.example)) {
                    return "\"" + codegenParameter.example + "\"";
                } else {
                    return "\"" + codegenParameter.paramName + "_example\"";
                }
            } else if (codegenParameter.isBoolean) { // boolean
                if (Boolean.parseBoolean(codegenParameter.example)) {
                    return "true";
                } else {
                    return "false";
                }
            } else if (codegenParameter.isUri) { // URL
                return "\"https://example.com\"";
            } else if (codegenParameter.isDateTime || codegenParameter.isDate) { // datetime or date
                processedModelMap.put("time.Time", 1);
                return "time.Now()";
            } else if (codegenParameter.isFile) {
                return "os.NewFile(1234, \"some_file\")";
            } else { // numeric
                if (!StringUtils.isEmpty(codegenParameter.example) && !"null".equals(codegenParameter.example)) {
                    return codegenParameter.dataType + "(" + codegenParameter.example + ")";
                } else {
                    return codegenParameter.dataType + "(987)";
                }
            }
        } else { // model
            // look up the model
            if (modelMaps.containsKey(codegenParameter.dataType)) {
                return constructExampleCode(modelMaps.get(codegenParameter.dataType), modelMaps, processedModelMap);
            } else if (codegenParameter.isEmail) { // email
                if (!StringUtils.isEmpty(codegenParameter.example) && !"null".equals(codegenParameter.example)) {
                    return "\"" + codegenParameter.example + "\"";
                } else {
                    return "\"" + codegenParameter.paramName + "@example.com\"";
                }
            } else if (codegenParameter.isDateTime || codegenParameter.isDate) { // datetime or date
                processedModelMap.put("time.Time", 1);
                return "time.Now()";
            } else {
                //LOGGER.error("Error in constructing examples. Failed to look up the model " + codegenParameter.dataType);
                return "TODO";
            }
        }
    }

    private String constructExampleCode(CodegenProperty codegenProperty, HashMap<String, CodegenModel> modelMaps, HashMap<String, Integer> processedModelMap) {
        if (codegenProperty.isArray) { // array
            String prefix = codegenProperty.dataType;
            String dataType = StringUtils.removeStart(codegenProperty.dataType, "[]");
            if (modelMaps.containsKey(dataType)) {
                prefix = "[]" + goImportAlias + "." + dataType;
            }
            if (codegenProperty.items.isNullable) {
                // We can't easily generate a pointer inline, so just use nil in that case
                return prefix + "{nil}";
            }
            return prefix + "{" + constructExampleCode(codegenProperty.items, modelMaps, processedModelMap) + "}";
        } else if (codegenProperty.isMap) { // map
            String prefix = codegenProperty.dataType;
            String dataType = StringUtils.removeStart(codegenProperty.dataType, "map[string][]");
            if (modelMaps.containsKey(dataType)) {
                prefix = "map[string][]" + goImportAlias + "." + dataType;
            }
            if (codegenProperty.items == null) {
                return prefix + "{ ... }";
            }
            return prefix + "{\"key\": " + constructExampleCode(codegenProperty.items, modelMaps, processedModelMap) + "}";
        } else if (codegenProperty.isPrimitiveType) { // primitive type
            if (codegenProperty.isString) {
                if (!StringUtils.isEmpty(codegenProperty.example) && !"null".equals(codegenProperty.example)) {
                    return "\"" + codegenProperty.example + "\"";
                } else {
                    return "\"" + codegenProperty.name + "_example\"";
                }
            } else if (codegenProperty.isBoolean) { // boolean
                if (Boolean.parseBoolean(codegenProperty.example)) {
                    return "true";
                } else {
                    return "false";
                }
            } else if (codegenProperty.isUri) { // URL
                return "\"https://example.com\"";
            } else if (codegenProperty.isDateTime || codegenProperty.isDate) { // datetime or date
                processedModelMap.put("time.Time", 1);
                return "time.Now()";
            } else { // numeric
                String example;
                if (!StringUtils.isEmpty(codegenProperty.example) && !"null".equals(codegenProperty.example)) {
                    example = codegenProperty.example;
                } else {
                    example = "123";
                }

                return codegenProperty.dataType + "(" + example + ")";
            }
        } else {
            // look up the model
            if (modelMaps.containsKey(codegenProperty.dataType)) {
                return constructExampleCode(modelMaps.get(codegenProperty.dataType), modelMaps, processedModelMap);
            } else if (codegenProperty.isEmail) { // email
                if (!StringUtils.isEmpty(codegenProperty.example) && !"null".equals(codegenProperty.example)) {
                    return "\"" + codegenProperty.example + "\"";
                } else {
                    return "\"" + codegenProperty.name + "@example.com\"";
                }
            } else if (codegenProperty.isDateTime || codegenProperty.isDate) { // datetime or date
                processedModelMap.put("time.Time", 1);
                return "time.Now()";
            } else {
                //LOGGER.error("Error in constructing examples. Failed to look up the model " + codegenProperty.dataType);
                return "\"TODO\"";
            }
        }
    }

    private String constructExampleCode(CodegenModel codegenModel, HashMap<String, CodegenModel> modelMaps, HashMap<String, Integer> processedModelMap) {
        // break infinite recursion. Return, in case a model is already processed in the current context.
        String model = codegenModel.name;
        if (processedModelMap.containsKey(model)) {
            int count = processedModelMap.get(model);
            if (count == 1) {
                processedModelMap.put(model, 2);
            } else if (count == 2) {
                return "";
            } else {
                throw new RuntimeException("Invalid count when constructing example: " + count);
            }
        } else if (codegenModel.isEnum) {
            Map<String, Object> allowableValues = codegenModel.allowableValues;
            List<Object> values = (List<Object>) allowableValues.get("values");
            String example = String.valueOf(values.get(0));
            if (codegenModel.isString) {
                example = "\"" + example + "\"";
            }
            return goImportAlias + "." + model + "(" + example + ")";
        } else if (codegenModel.oneOf != null && !codegenModel.oneOf.isEmpty()) {
            String subModel = (String) codegenModel.oneOf.toArray()[0];
            String oneOf = constructExampleCode(modelMaps.get(subModel), modelMaps, processedModelMap).substring(1);
            return goImportAlias + "." + model + "{" + subModel + ": " + oneOf + "}";
        } else {
            processedModelMap.put(model, 1);
        }

        List<String> propertyExamples = new ArrayList<>();
        for (CodegenProperty codegenProperty : codegenModel.requiredVars) {
            propertyExamples.add(constructExampleCode(codegenProperty, modelMaps, processedModelMap));
        }
        return "*" + goImportAlias + ".New" + toModelName(model) + "(" + StringUtils.join(propertyExamples, ", ") + ")";
    }
}
