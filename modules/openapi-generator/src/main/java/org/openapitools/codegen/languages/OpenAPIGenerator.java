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
import org.apache.commons.io.FileUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.serializer.SerializerUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.util.EnumSet;

public class OpenAPIGenerator extends DefaultCodegen implements CodegenConfig {
    public static final String OUTPUT_NAME = "outputFileName";

    private final Logger LOGGER = LoggerFactory.getLogger(OpenAPIGenerator.class);

    protected String outputFileName = "openapi.json";

    public OpenAPIGenerator() {
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

        embeddedTemplateDir = templateDir = "openapi";
        outputFolder = "generated-code/openapi";

        supportingFiles.add(new SupportingFile("README.md", "", "README.md"));

        cliOptions.add(CliOption.newString(OUTPUT_NAME, "Output file name").defaultValue(outputFileName));
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.DOCUMENTATION;
    }

    @Override
    public String getName() {
        return "openapi";
    }

    @Override
    public String getHelp() {
        return "Creates a static openapi.json file (OpenAPI spec v3.0).";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(OUTPUT_NAME)) {
            outputFileName = additionalProperties.get(OUTPUT_NAME).toString();
        }
        LOGGER.info("Output file name [outputFileName={}]", outputFileName);
    }

    @Override
    public void processOpenAPI(OpenAPI openAPI) {
        String jsonOpenAPI = SerializerUtils.toJsonString(openAPI);

        try {
            String outputFile = outputFolder + File.separator + outputFileName;
            FileUtils.writeStringToFile(new File(outputFile), jsonOpenAPI, StandardCharsets.UTF_8);
            LOGGER.info("wrote file to {}", outputFile);
        } catch (Exception e) {
            LOGGER.error(e.getMessage(), e);
        }
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
}
