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

import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;

import java.io.File;
import java.util.EnumSet;

public class StaticDocCodegen extends DefaultCodegen implements CodegenConfig {
    protected String invokerPackage = "org.openapitools.client";
    protected String groupId = "org.openapitools";
    protected String artifactId = "openapi-client";
    protected String artifactVersion = "1.0.0";
    protected String sourceFolder = "docs";

    public StaticDocCodegen() {
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

        // clear import mapping (from default generator) as this generator does not use it
        // at the moment
        importMapping.clear();

        outputFolder = "docs";
        modelTemplateFiles.put("model.mustache", ".html");
        apiTemplateFiles.put("operation.mustache", ".html");
        embeddedTemplateDir = templateDir = "openapi-static";

        cliOptions.add(new CliOption(CodegenConstants.INVOKER_PACKAGE, CodegenConstants.INVOKER_PACKAGE_DESC));
        cliOptions.add(new CliOption(CodegenConstants.GROUP_ID, CodegenConstants.GROUP_ID_DESC));
        cliOptions.add(new CliOption(CodegenConstants.ARTIFACT_ID, CodegenConstants.ARTIFACT_ID_DESC));
        cliOptions.add(new CliOption(CodegenConstants.ARTIFACT_VERSION, CodegenConstants.ARTIFACT_VERSION_DESC));

        additionalProperties.put(CodegenConstants.INVOKER_PACKAGE, invokerPackage);
        additionalProperties.put(CodegenConstants.GROUP_ID, groupId);
        additionalProperties.put(CodegenConstants.ARTIFACT_ID, artifactId);
        additionalProperties.put(CodegenConstants.ARTIFACT_VERSION, artifactVersion);

        supportingFiles.add(new SupportingFile("package.mustache", "", "package.json"));
        supportingFiles.add(new SupportingFile("main.mustache", "", "main.js"));
        supportingFiles.add(new SupportingFile("assets/css/bootstrap-responsive.css",
                outputFolder + "/assets/css", "bootstrap-responsive.css"));
        supportingFiles.add(new SupportingFile("assets/css/bootstrap.css",
                outputFolder + "/assets/css", "bootstrap.css"));
        supportingFiles.add(new SupportingFile("assets/css/style.css",
                outputFolder + "/assets/css", "style.css"));
        supportingFiles.add(new SupportingFile("assets/images/logo.png",
                outputFolder + "/assets/images", "logo.png"));
        supportingFiles.add(new SupportingFile("assets/js/bootstrap.js",
                outputFolder + "/assets/js", "bootstrap.js"));
        supportingFiles.add(new SupportingFile("assets/js/jquery-1.8.3.min.js",
                outputFolder + "/assets/js", "jquery-1.8.3.min.js"));
        supportingFiles.add(new SupportingFile("assets/js/main.js",
                outputFolder + "/assets/js", "main.js"));
        supportingFiles.add(new SupportingFile("index.mustache",
                outputFolder, "index.html"));

        instantiationTypes.put("array", "ArrayList");
        instantiationTypes.put("map", "HashMap");
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.DOCUMENTATION;
    }

    @Override
    public String getName() {
        return "dynamic-html";
    }

    @Override
    public String getHelp() {
        return "Generates a dynamic HTML site.";
    }

    @Override
    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "_" + name;
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + "operations";
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + "models";
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
