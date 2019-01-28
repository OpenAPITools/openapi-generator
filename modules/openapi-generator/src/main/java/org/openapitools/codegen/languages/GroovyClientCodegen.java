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

import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.SupportingFile;

import java.io.File;

import static org.openapitools.codegen.utils.StringUtils.camelize;


public class GroovyClientCodegen extends AbstractJavaCodegen {
    public static final String CONFIG_PACKAGE = "configPackage";
    protected String title = "Petstore Server";
    protected String configPackage = "";

    public GroovyClientCodegen() {
        super();

        // avoid importing the following as models
        languageSpecificPrimitives.add("Date");
        languageSpecificPrimitives.add("ArrayList");
        languageSpecificPrimitives.add("File");
        languageSpecificPrimitives.add("Map");

        sourceFolder = projectFolder + File.separator + "groovy";
        outputFolder = "generated-code/groovy";
        modelTemplateFiles.put("model.mustache", ".groovy");
        apiTemplateFiles.put("api.mustache", ".groovy");
        apiTestTemplateFiles.clear(); // TODO: add test template
        embeddedTemplateDir = templateDir = "Groovy";

        // clear model and api doc template as this codegen
        // does not support auto-generated markdown doc at the moment
        //TODO: add doc templates
        modelDocTemplateFiles.remove("model_doc.mustache");
        apiDocTemplateFiles.remove("api_doc.mustache");

        apiPackage = "org.openapitools.api";
        modelPackage = "org.openapitools.model";
        configPackage = "org.openapitools.configuration";
        invokerPackage = "org.openapitools.api";
        artifactId = "openapi-groovy";
        dateLibrary = "legacy"; //TODO: add joda support to groovy

        additionalProperties.put("title", title);
        additionalProperties.put(CONFIG_PACKAGE, configPackage);

        cliOptions.add(new CliOption(CONFIG_PACKAGE, "configuration package for generated code"));
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "groovy";
    }

    @Override
    public String getHelp() {
        return "Generates a Groovy API client (beta).";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(CONFIG_PACKAGE)) {
            this.setConfigPackage((String) additionalProperties.get(CONFIG_PACKAGE));
        }

        supportingFiles.add(new SupportingFile("build.gradle.mustache", "", "build.gradle"));
        // TODO readme to be added later
        //supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("ApiUtils.mustache",
                (sourceFolder + File.separator + apiPackage).replace(".", java.io.File.separator), "ApiUtils.groovy"));

    }

    @Override
    public String toApiName(String name) {
        if (name.length() == 0) {
            return "DefaultApi";
        }
        name = sanitizeName(name);
        return camelize(name) + "Api";
    }

    public void setConfigPackage(String configPackage) {
        this.configPackage = configPackage;
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove ' to avoid code injection
        return input.replace("'", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("*/", "*_/").replace("/*", "/_*");
    }
}
