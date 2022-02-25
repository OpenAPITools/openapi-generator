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

import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.meta.features.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.EnumSet;

public class PythonAiohttpConnexionServerCodegen extends AbstractPythonConnexionServerCodegen {
    private final Logger LOGGER = LoggerFactory.getLogger(PythonAiohttpConnexionServerCodegen.class);

    public PythonAiohttpConnexionServerCodegen() {
        super("python-aiohttp", true);

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML, WireFormatFeature.Custom))
                .securityFeatures(EnumSet.of(
                        SecurityFeature.BasicAuth,
                        SecurityFeature.BearerToken,
                        SecurityFeature.ApiKey,
                        SecurityFeature.OAuth2_Implicit
                ))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling
                )
                .excludeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
                .excludeParameterFeatures(
                        ParameterFeature.Cookie
                )
        );

        testPackage = "tests";
        embeddedTemplateDir = templateDir = "python-aiohttp";
    }

    /**
     * Configures a friendly name for the generator.  This will be used by the generator
     * to select the library with the -g flag.
     *
     * @return the friendly name for the generator
     */
    @Override
    public String getName() {
        return "python-aiohttp";
    }

    @Override
    protected void addSupportingFiles() {
        supportingFiles.add(new SupportingFile("conftest.mustache", testPackage, "conftest.py"));
        supportingFiles.add(new SupportingFile("__init__test.mustache", testPackage, "__init__.py"));
        supportingFiles.add(new SupportingFile("__init__main.mustache", packagePath(), "__init__.py"));
        supportingFiles.add(new SupportingFile("setup.mustache", "", "setup.py"));
        supportingFiles.add(new SupportingFile("tox.mustache", "", "tox.ini"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
    }

    @Override
    public String generatorLanguageVersion() { return "3.5.2+"; };
}
