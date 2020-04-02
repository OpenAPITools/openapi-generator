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

import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.EnumSet;

public class KotlinVertxServerCodegen extends AbstractKotlinCodegen {

    protected String rootPackage = "org.openapitools.server.api";
    protected String apiVersion = "1.0.0-SNAPSHOT";

    public static final String ROOT_PACKAGE = "rootPackage";

    public static final String PROJECT_NAME = "projectName";

    static Logger LOGGER = LoggerFactory.getLogger(KotlinVertxServerCodegen.class);

    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    public String getName() {
        return "kotlin-vertx";
    }

    public String getHelp() {
        return "Generates a kotlin-vertx server.";
    }

    public KotlinVertxServerCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML))
                .securityFeatures(EnumSet.noneOf(
                        SecurityFeature.class
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
                .includeParameterFeatures(
                        ParameterFeature.Cookie
                )
        );

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();

        outputFolder = "generated-code" + File.separator + "kotlin-vertx";
        modelTemplateFiles.put("model.mustache", ".kt");

        apiTestTemplateFiles.clear();
        modelDocTemplateFiles.clear();
        supportingFiles.clear();

        apiTemplateFiles.clear();
        apiTemplateFiles.put("api.mustache", ".kt");
        apiTemplateFiles.put("apiProxy.mustache", "VertxProxyHandler.kt");
        apiTemplateFiles.put("api_verticle.mustache", "Verticle.kt");

        embeddedTemplateDir = templateDir = "kotlin-vertx-server";
        apiPackage = rootPackage + ".verticle";
        modelPackage = rootPackage + ".model";
        artifactId = "openapi-kotlin-vertx-server";
        artifactVersion = apiVersion;

        updateOption(CodegenConstants.API_PACKAGE, apiPackage);
        updateOption(CodegenConstants.MODEL_PACKAGE, modelPackage);
        additionalProperties.put(ROOT_PACKAGE, rootPackage);

        supportingFiles.add(new SupportingFile("pom.mustache", "", "pom.xml"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));

    }

}
