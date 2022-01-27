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
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.templating.mustache.OnChangeLambda;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.samskivert.mustache.Mustache.Lambda;

import io.swagger.v3.oas.models.Operation;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;
import java.util.Map;

public class OpenAPIYamlGenerator extends DefaultCodegen implements CodegenConfig {
    public static final String OUTPUT_NAME = "outputFile";

    private final Logger LOGGER = LoggerFactory.getLogger(OpenAPIYamlGenerator.class);

    protected String outputFile = "openapi/openapi.yaml";

    public OpenAPIYamlGenerator() {
        super();

        modifyFeatureSet(features -> features
                .documentationFeatures(EnumSet.allOf(DocumentationFeature.class))
                .dataTypeFeatures(EnumSet.allOf(DataTypeFeature.class))
                .wireFormatFeatures(EnumSet.allOf(WireFormatFeature.class))
                .securityFeatures(EnumSet.allOf(SecurityFeature.class))
                .globalFeatures(EnumSet.allOf(GlobalFeature.class))
                .parameterFeatures(EnumSet.allOf(ParameterFeature.class))
                .schemaSupportFeatures(EnumSet.allOf(SchemaSupportFeature.class))
        );

        embeddedTemplateDir = templateDir = "openapi-yaml";
        outputFolder = "generated-code/openapi-yaml";
        cliOptions.add(CliOption.newString(OUTPUT_NAME, "Output filename").defaultValue(outputFile));
        supportingFiles.add(new SupportingFile("README.md", "", "README.md"));
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.DOCUMENTATION;
    }

    @Override
    public String getName() {
        return "openapi-yaml";
    }

    @Override
    public String getHelp() {
        return "Creates a static openapi.yaml file (OpenAPI spec v3).";
    }

    @Override
    public void processOpts() {
        super.processOpts();
        if (additionalProperties.containsKey(OUTPUT_NAME)) {
            outputFile = additionalProperties.get(OUTPUT_NAME).toString();
        }
        LOGGER.info("Output file [outputFile={}]", outputFile);
        supportingFiles.add(new SupportingFile("openapi.mustache", outputFile));
    }

    @Override
    protected ImmutableMap.Builder<String, Lambda> addMustacheLambdas() {
        return super.addMustacheLambdas()
                .put("onchange", new OnChangeLambda());
    }

    /**
     * Group operations by resourcePath so that operations with same path and
     * different http method can be rendered one after the other.
     */
    @Override
    public void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation
            co, Map<String, List<CodegenOperation>> operations) {
        List<CodegenOperation> opList = operations.computeIfAbsent(resourcePath,
                k -> new ArrayList<>());
        opList.add(co);
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        generateYAMLSpecFile(objs);
        return super.postProcessSupportingFileData(objs);
    }

    @Override
    public String escapeQuotationMark(String input) {
        // just return the original string
        return input;
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        // just return the original string
        return input;
    }

    @Override
    public GeneratorLanguage generatorLanguage() { return null; }
}
