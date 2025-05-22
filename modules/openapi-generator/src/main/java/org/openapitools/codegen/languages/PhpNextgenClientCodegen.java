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
import lombok.Getter;
import lombok.Setter;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

public class PhpNextgenClientCodegen extends AbstractPhpCodegen {
    @SuppressWarnings("hiding")
    private final Logger LOGGER = LoggerFactory.getLogger(PhpNextgenClientCodegen.class);

    public static final String SUPPORT_STREAMING = "supportStreaming";

    @Getter @Setter
    protected boolean supportStreaming = false;

    public PhpNextgenClientCodegen() {
        super();

        // override default src and test folders to comply PSD skeleton
        setSrcBasePath("src");
        setTestBasePath("tests");

        // mark as beta so far
        this.generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA).build();

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

        // clear import mapping (from default generator) as php does not use it
        // at the moment
        importMapping.clear();

        setInvokerPackage("OpenAPI\\Client");
        setApiPackage(getInvokerPackage() + "\\" + apiDirName);
        setModelPackage(getInvokerPackage() + "\\" + modelDirName);
        setPackageName("OpenAPIClient-php");
        supportsInheritance = true;
        setOutputDir("generated-code" + File.separator + "php");
        modelTestTemplateFiles.put("model_test.mustache", ".php");
        embeddedTemplateDir = templateDir = "php-nextgen";

        // default HIDE_GENERATION_TIMESTAMP to true
        hideGenerationTimestamp = Boolean.TRUE;

        // provide primitives to mustache template
        List sortedLanguageSpecificPrimitives = new ArrayList(languageSpecificPrimitives);
        Collections.sort(sortedLanguageSpecificPrimitives);
        String primitives = "'" + StringUtils.join(sortedLanguageSpecificPrimitives, "', '") + "'";
        additionalProperties.put("primitives", primitives);

        cliOptions.add(new CliOption(CodegenConstants.HIDE_GENERATION_TIMESTAMP, CodegenConstants.ALLOW_UNICODE_IDENTIFIERS_DESC)
                .defaultValue(Boolean.TRUE.toString()));
        cliOptions.add(CliOption.newBoolean(SUPPORT_STREAMING, "Support streaming endpoint", this.supportStreaming));
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "php-nextgen";
    }

    @Override
    public String getHelp() {
        return "Generates a PHP client library (beta).";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        convertPropertyToBooleanAndWriteBack(SUPPORT_STREAMING, this::setSupportStreaming);

        supportingFiles.add(new SupportingFile("ApiException.mustache", toSrcPath(invokerPackage, srcBasePath), "ApiException.php"));
        supportingFiles.add(new SupportingFile("Configuration.mustache", toSrcPath(invokerPackage, srcBasePath), "Configuration.php"));
        supportingFiles.add(new SupportingFile("FormDataProcessor.mustache", toSrcPath(invokerPackage, srcBasePath), "FormDataProcessor.php"));
        supportingFiles.add(new SupportingFile("ObjectSerializer.mustache", toSrcPath(invokerPackage, srcBasePath), "ObjectSerializer.php"));
        supportingFiles.add(new SupportingFile("ModelInterface.mustache", toSrcPath(modelPackage, srcBasePath), "ModelInterface.php"));
        supportingFiles.add(new SupportingFile("HeaderSelector.mustache", toSrcPath(invokerPackage, srcBasePath), "HeaderSelector.php"));
        supportingFiles.add(new SupportingFile("composer.mustache", "", "composer.json"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("phpunit.xml.mustache", "", "phpunit.xml.dist"));
        supportingFiles.add(new SupportingFile(".travis.yml", "", ".travis.yml"));
        supportingFiles.add(new SupportingFile(".php-cs-fixer.dist.php", "", ".php-cs-fixer.dist.php"));
        supportingFiles.add(new SupportingFile(".phplint.mustache", "", ".phplint.yml"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));

        if (this.supportStreaming) {
            typeMapping.put("file", "\\Psr\\\\Http\\\\Message\\\\StreamInterface");
        }
    }

    @Override
    public Map<String, ModelsMap> postProcessAllModels(Map<String, ModelsMap> objs) {
        final Map<String, ModelsMap> processed = super.postProcessAllModels(objs);

        for (Map.Entry<String, ModelsMap> entry : processed.entrySet()) {
            entry.setValue(postProcessModelsMap(entry.getValue()));
        }

        return processed;
    }

    private ModelsMap postProcessModelsMap(ModelsMap objs) {
        for (ModelMap m : objs.getModels()) {
            CodegenModel model = m.getModel();

            for (CodegenProperty prop : model.vars) {
                String propType;
                if (prop.isArray || prop.isMap) {
                    propType = "array";
                } else {
                    propType = prop.dataType;
                }

                if ((!prop.required || prop.isNullable) && !propType.equals("mixed")) { // optional or nullable but not mixed
                    propType = "?" + propType;
                }

                prop.vendorExtensions.putIfAbsent("x-php-prop-type", propType);
            }

            if (model.isEnum) {
                for (Map<String, Object> enumVars : (List<Map<String, Object>>) model.getAllowableValues().get("enumVars")) {
                    if ((Boolean) enumVars.get("isString")) {
                        model.vendorExtensions.putIfAbsent("x-php-enum-type", "string");
                    } else {
                        model.vendorExtensions.putIfAbsent("x-php-enum-type", "int");
                    }
                }
            }
        }
        return objs;
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        objs = super.postProcessOperationsWithModels(objs, allModels);
        OperationMap operations = objs.getOperations();
        for (CodegenOperation operation : operations.getOperation()) {
            Set<String> phpReturnTypeOptions = new LinkedHashSet<>();
            Set<String> docReturnTypeOptions = new LinkedHashSet<>();

            for (CodegenResponse response : operation.responses) {
                if (response.dataType != null) {
                    String returnType = response.dataType;
                    if (response.isArray || response.isMap) {
                        // PHP does not understand array type hinting so we strip it
                        // The phpdoc will still contain the array type hinting
                        returnType = "array";
                    }

                    phpReturnTypeOptions.add(returnType);
                    docReturnTypeOptions.add(response.dataType);
                }
            }

            if (phpReturnTypeOptions.isEmpty()) {
                operation.vendorExtensions.putIfAbsent("x-php-return-type-is-void", true);
                operation.vendorExtensions.putIfAbsent("x-php-return-type", "void");
                operation.vendorExtensions.putIfAbsent("x-php-doc-return-type", "void");
            } else {
                operation.vendorExtensions.putIfAbsent("x-php-return-type-is-void", false);
                operation.vendorExtensions.putIfAbsent("x-php-return-type", String.join("|", phpReturnTypeOptions));
                operation.vendorExtensions.putIfAbsent("x-php-doc-return-type", String.join("|", docReturnTypeOptions));
            }

            for (CodegenParameter param : operation.allParams) {
                String paramType;
                if (param.isArray || param.isMap) {
                    paramType = "array";
                } else {
                    paramType = param.dataType;
                }
                if ((!param.required || param.isNullable) && !paramType.equals("mixed")) { // optional or nullable but not mixed
                    paramType = "?" + paramType;
                }
                param.vendorExtensions.putIfAbsent("x-php-param-type", paramType);
            }
        }

        return objs;
    }

    @Override
    public String toDefaultValue(CodegenProperty codegenProperty, Schema schema) {

        if (codegenProperty.isArray) {
            schema = ModelUtils.getReferencedSchema(this.openAPI, schema);

            if (schema.getDefault() != null) { // array schema has default value
                return "[" + schema.getDefault().toString() + "]";
            } else if (schema.getItems().getDefault() != null) { // array item schema has default value
                return "[" + toDefaultValue(schema.getItems()) + "]";
            } else {
                return null;
            }
        }
        return super.toDefaultValue(codegenProperty, schema);
    }

    @Override
    public String toDefaultParameterValue(CodegenProperty codegenProperty, Schema<?> schema) {
        return toDefaultValue(codegenProperty, schema);
    }

    @Override
    public void setParameterExampleValue(CodegenParameter p) {
        if (p.isArray && p.items.defaultValue != null) {
            p.example = p.defaultValue;
        } else {
            super.setParameterExampleValue(p);
        }
    }
}
