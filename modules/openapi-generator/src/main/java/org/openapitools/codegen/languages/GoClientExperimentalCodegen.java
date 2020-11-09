/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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
import io.swagger.v3.oas.models.security.SecurityScheme;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.ProcessUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public class GoClientExperimentalCodegen extends GoClientCodegen {

    private static final Logger LOGGER = LoggerFactory.getLogger(GoClientExperimentalCodegen.class);
    protected String goImportAlias = "openapiclient";
    protected boolean useOneOfDiscriminatorLookup = false; // use oneOf discriminator's mapping for model lookup

    public GoClientExperimentalCodegen() {
        super();
        outputFolder = "generated-code/go-experimental";
        embeddedTemplateDir = templateDir = "go-experimental";

        usesOptionals = false;

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata).stability(Stability.EXPERIMENTAL).build();

        cliOptions.add(new CliOption(CodegenConstants.USE_ONEOF_DISCRIMINATOR_LOOKUP, CodegenConstants.USE_ONEOF_DISCRIMINATOR_LOOKUP_DESC).defaultValue("false"));
    }

    /**
     * Configures a friendly name for the generator. This will be used by the
     * generator to select the library with the -g flag.
     *
     * @return the friendly name for the generator
     */
    @Override
    public String getName() {
        return "go-experimental";
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
        return "Generates a Go client library (experimental and may subject to breaking changes without further notice).";
    }

    @Override
    public void processOpts() {
        this.setLegacyDiscriminatorBehavior(false);
        super.processOpts();
        supportingFiles.add(new SupportingFile("utils.mustache", "", "utils.go"));

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
            additionalProperties.put(CodegenConstants.USE_ONEOF_DISCRIMINATOR_LOOKUP, useOneOfDiscriminatorLookup);
        }

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

    @Override
    public String toModelName(String name) {
        // underscoring would also lowercase the whole name, thus losing acronyms which are in capitals
        return camelize(toModel(name, false));
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
            if (p.getDefault() != null) {
                return "\"" + escapeText((String) p.getDefault()) + "\"";
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

                for (CodegenProperty param : model.vars) {
                    param.vendorExtensions.put("x-go-base-type", param.dataType);
                    if (!param.isNullable || param.isMapContainer || param.isListContainer ||
                            param.isFreeFormObject || param.isAnyType) {
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

                // add x-additional-properties
                if ("map[string]map[string]interface{}".equals(model.parent)) {
                    model.vendorExtensions.put("x-additional-properties", true);
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
        }

        processedModelMaps.clear();
        for (CodegenOperation operation : operationList) {
            for (CodegenParameter cp : operation.allParams) {
                cp.vendorExtensions.put("x-go-example", constructExampleCode(cp, modelMaps, processedModelMaps));
            }
        }

        return objs;
    }

    private String constructExampleCode(CodegenParameter codegenParameter, HashMap<String, CodegenModel> modelMaps, HashMap<String, Integer> processedModelMap) {
        if (codegenParameter.isListContainer) { // array
            return codegenParameter.dataType + "{" + constructExampleCode(codegenParameter.items, modelMaps, processedModelMap) + "}";
        } else if (codegenParameter.isMapContainer) {
            return "map[string]string{ \"Key\" = \"Value\" }";
        } else if (codegenParameter.isPrimitiveType) { // primitive type
            if (codegenParameter.isString) {
                if (StringUtils.isEmpty(codegenParameter.example)) {
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
                return "URL(string: \"https://example.com\")!";
            } else if (codegenParameter.isDateTime || codegenParameter.isDate) { // datetime or date
                return "Get-Date";
            } else { // numeric
                if (StringUtils.isEmpty(codegenParameter.example)) {
                    return codegenParameter.example;
                } else {
                    return "987";
                }
            }
        } else { // model
            // look up the model
            if (modelMaps.containsKey(codegenParameter.dataType)) {
                return constructExampleCode(modelMaps.get(codegenParameter.dataType), modelMaps, processedModelMap);
            } else {
                //LOGGER.error("Error in constructing examples. Failed to look up the model " + codegenParameter.dataType);
                return "TODO";
            }
        }
    }

    private String constructExampleCode(CodegenProperty codegenProperty, HashMap<String, CodegenModel> modelMaps, HashMap<String, Integer> processedModelMap) {
        if (codegenProperty.isListContainer) { // array
            return codegenProperty.dataType + "{" + constructExampleCode(codegenProperty.items, modelMaps, processedModelMap) + ")";
        } else if (codegenProperty.isMapContainer) { // map
            return "map[string]string{ \"Key\" = \"Value\" }";
        } else if (codegenProperty.isPrimitiveType) { // primitive type
            if (codegenProperty.isString) {
                if (StringUtils.isEmpty(codegenProperty.example)) {
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
                return "\"https://example.com\")!";
            } else if (codegenProperty.isDateTime || codegenProperty.isDate) { // datetime or date
                return "time.Now()";
            } else { // numeric
                String example;
                if (StringUtils.isEmpty(codegenProperty.example)) {
                    example = codegenProperty.example;
                } else {
                    example = "123";
                }

                if (codegenProperty.isLong) {
                    return "int64(" + example + ")";
                } else {
                    return example;
                }
            }
        } else {
            // look up the model
            if (modelMaps.containsKey(codegenProperty.dataType)) {
                return constructExampleCode(modelMaps.get(codegenProperty.dataType), modelMaps, processedModelMap);
            } else {
                //LOGGER.error("Error in constructing examples. Failed to look up the model " + codegenProperty.dataType);
                return "\"TODO\"";
            }
        }
    }

    private String constructExampleCode(CodegenModel codegenModel, HashMap<String, CodegenModel> modelMaps, HashMap<String, Integer> processedModelMap) {
        String example;

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
        } else {
            processedModelMap.put(model, 1);
        }

        example = "" + goImportAlias + "." + codegenModel.name + "{";
        List<String> propertyExamples = new ArrayList<>();
        for (CodegenProperty codegenProperty : codegenModel.allVars) {
            propertyExamples.add(codegenProperty.name + ": " + constructExampleCode(codegenProperty, modelMaps, processedModelMap));
        }
        example += StringUtils.join(propertyExamples, ", ");
        example += "}";
        return example;
    }


}
