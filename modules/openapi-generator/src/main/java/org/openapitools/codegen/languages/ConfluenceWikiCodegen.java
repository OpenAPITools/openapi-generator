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

import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.utils.ModelUtils;

import java.util.*;

public class ConfluenceWikiCodegen extends DefaultCodegen implements CodegenConfig {
    private static final String ALL_OPERATIONS = "";
    protected String invokerPackage = "org.openapitools.client";
    protected String groupId = "org.openapitools";
    protected String artifactId = "openapi-client";
    protected String artifactVersion = "1.0.0";

    public ConfluenceWikiCodegen() {
        super();

        // TODO: ConfluenceWiki maintainer review
        modifyFeatureSet(features -> features
                .documentationFeatures(EnumSet.noneOf(DocumentationFeature.class))
                .securityFeatures(EnumSet.noneOf(SecurityFeature.class))
                .excludeParameterFeatures(ParameterFeature.Cookie)
                .includeWireFormatFeatures(
                        WireFormatFeature.JSON,
                        WireFormatFeature.XML,
                        WireFormatFeature.Custom
                )
                .excludeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism,
                        SchemaSupportFeature.Union
                )
        );

        outputFolder = "docs";
        embeddedTemplateDir = templateDir = "confluenceWikiDocs";

        defaultIncludes = new HashSet<String>();

        cliOptions.add(new CliOption("appName", "short name of the application"));
        cliOptions.add(new CliOption("appDescription", "description of the application"));
        cliOptions.add(new CliOption("infoUrl", "a URL where users can get more information about the application"));
        cliOptions.add(new CliOption("infoEmail", "an email address to contact for inquiries about the application"));
        cliOptions.add(new CliOption("licenseInfo", "a short description of the license"));
        cliOptions.add(new CliOption("licenseUrl", "a URL pointing to the full license"));
        cliOptions.add(new CliOption(CodegenConstants.INVOKER_PACKAGE, CodegenConstants.INVOKER_PACKAGE_DESC));
        cliOptions.add(new CliOption(CodegenConstants.GROUP_ID, CodegenConstants.GROUP_ID_DESC));
        cliOptions.add(new CliOption(CodegenConstants.ARTIFACT_ID, CodegenConstants.ARTIFACT_ID_DESC));
        cliOptions.add(new CliOption(CodegenConstants.ARTIFACT_VERSION, CodegenConstants.ARTIFACT_VERSION_DESC));

        additionalProperties.put("appName", "OpenAPI Sample");
        additionalProperties.put("appDescription", "A sample OpenAPI server");
        additionalProperties.put("infoUrl", "https://openapi-generator.tech");
        additionalProperties.put("infoEmail", "team@openapitools.org");
        additionalProperties.put("licenseInfo", "All rights reserved");
        additionalProperties.put("licenseUrl", "http://apache.org/licenses/LICENSE-2.0.html");
        additionalProperties.put(CodegenConstants.INVOKER_PACKAGE, invokerPackage);
        additionalProperties.put(CodegenConstants.GROUP_ID, groupId);
        additionalProperties.put(CodegenConstants.ARTIFACT_ID, artifactId);
        additionalProperties.put(CodegenConstants.ARTIFACT_VERSION, artifactVersion);

        supportingFiles.add(new SupportingFile("index.mustache", "", "confluence-markup.txt"));
        reservedWords = new HashSet<String>();

        languageSpecificPrimitives = new HashSet<String>();
        importMapping = new HashMap<String, String>();
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.DOCUMENTATION;
    }

    @Override
    public String getName() {
        return "cwiki";
    }

    @Override
    public String getHelp() {
        return "Generates confluence wiki markup.";
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            return getSchemaType(p) + "[" + getTypeDeclaration(inner) + "]";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = getAdditionalProperties(p);
            return getSchemaType(p) + "[String, " + getTypeDeclaration(inner) + "]";
        }
        return super.getTypeDeclaration(p);
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");
        for (CodegenOperation op : operationList) {
            op.httpMethod = op.httpMethod.toLowerCase(Locale.ROOT);
        }
        return objs;
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
