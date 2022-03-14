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

import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.EnumSet;

public class GraphQLSchemaCodegen extends AbstractGraphQLCodegen implements CodegenConfig {

    private final Logger LOGGER = LoggerFactory.getLogger(GraphQLSchemaCodegen.class);

    @Override
    public CodegenType getTag() {
        return CodegenType.SCHEMA;
    }

    public String getName() {
        return "graphql-schema";
    }

    public String getHelp() {
        return "Generates GraphQL schema files (beta)";
    }

    public GraphQLSchemaCodegen() {
        super();

        modifyFeatureSet(features -> features
//                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON))
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
        );

        outputFolder = "generated-code/graphql-schema";
        modelTemplateFiles.put("model.mustache", ".graphql");
        apiTemplateFiles.put("api.mustache", ".graphql");
        embeddedTemplateDir = templateDir = "graphql-schema";
        hideGenerationTimestamp = Boolean.TRUE;


        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");


        cliOptions.clear();
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME, "GraphQL package name (convention: lowercase).")
                .defaultValue("openapi2graphql"));
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_VERSION, "GraphQL package version.")
                .defaultValue("1.0.0"));
        cliOptions.add(new CliOption(CodegenConstants.HIDE_GENERATION_TIMESTAMP, CodegenConstants.HIDE_GENERATION_TIMESTAMP_DESC)
                .defaultValue(Boolean.TRUE.toString()));

    }

    @Override
    public void processOpts() {
        super.processOpts();

        //apiTestTemplateFiles.put("api_test.mustache", ".graphql");
        //modelTestTemplateFiles.put("model_test.mustache", ".graphql");

        apiDocTemplateFiles.clear(); // TODO: add api doc template
        modelDocTemplateFiles.clear(); // TODO: add model doc template

        modelPackage = packageName;
        apiPackage = packageName;

        //supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        //supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        //supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"))
        //supportingFiles.add(new SupportingFile(".travis.yml", "", ".travis.yml"));
    }

    @Override
    public GeneratorLanguage generatorLanguage() { return GeneratorLanguage.GRAPH_QL; }
}
