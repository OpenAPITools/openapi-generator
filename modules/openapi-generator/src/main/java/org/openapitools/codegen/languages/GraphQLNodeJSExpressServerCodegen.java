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

import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.EnumSet;

public class GraphQLNodeJSExpressServerCodegen extends AbstractGraphQLCodegen implements CodegenConfig {

    private final Logger LOGGER = LoggerFactory.getLogger(GraphQLNodeJSExpressServerCodegen.class);

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "graphql-nodejs-express-server";
    }

    @Override
    public String getHelp() {
        return "Generates a GraphQL Node.js Express server (beta) including it's types, queries, mutations, (resolvers)";
    }

    public GraphQLNodeJSExpressServerCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
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
                .excludeParameterFeatures(
                        ParameterFeature.Cookie
                )
        );

        packageName = "openapi3graphql-server";
        packageVersion = "1.0.0";

        outputFolder = "generated-code/graphql-nodejs-express-server";
        embeddedTemplateDir = templateDir = "graphql-nodejs-express-server";

        hideGenerationTimestamp = Boolean.TRUE;

        apiTemplateFiles.put("api.mustache", ".graphql");
        apiTemplateFiles.put("resolvers.mustache", "_resolver.js");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");
        modelTemplateFiles.put("model.mustache", ".graphql");
        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        // TODO check why api doc is not written

        cliOptions.clear();
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME, "GraphQL Node.js Express server package name (convention: lowercase).")
                .defaultValue("openapi3graphql-server"));
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_VERSION, "GraphQL Node.js Express server package version.")
                .defaultValue("1.0.0"));
        cliOptions.add(new CliOption(CodegenConstants.HIDE_GENERATION_TIMESTAMP, CodegenConstants.HIDE_GENERATION_TIMESTAMP_DESC)
                .defaultValue(Boolean.TRUE.toString()));
    }

    @Override
    public void processOpts() {
        super.processOpts();

        // TODO: add api test template
        //apiTestTemplateFiles.put("api_test.mustache", ".graphql");

        // TODO: add model test template
        //modelTestTemplateFiles.put("model_test.mustache", ".graphql");
        modelTestTemplateFiles.clear();

        modelPackage = packageName;
        apiPackage = packageName;

        String supportFolder = apiPackage().replace('.', File.separatorChar);

        // Dynamic express/graphql related stuff
        supportingFiles.add(new SupportingFile("schema.graphql.mustache", supportFolder, "schema.graphql"));

        // General stuff
        supportingFiles.add(new SupportingFile("gitignore", supportFolder, ".gitignore"));
        supportingFiles.add(new SupportingFile("README.mustache", supportFolder, "README.md"));
        supportingFiles.add(new SupportingFile("package.json.mustache", supportFolder, "package.json"));
        supportingFiles.add(new SupportingFile("server.js", supportFolder, "server.js"));
        supportingFiles.add(new SupportingFile("start.js", supportFolder, "start.js"));
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();

            // IMPORTANT NOTE we add the braces within template because there we have the possibility to differentiate
            // between some specific types for GraphQL:
            // return "[" + getTypeDeclaration(inner) + "]";
            return getTypeDeclaration(inner);
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = (Schema) p.getAdditionalProperties();

            return getTypeDeclaration(inner);
        }

        // IMPORTANT NOTE Not using the supertype invocation, because we want to UpperCamelize the type:
        String schemaType = getSchemaType(p);
        String nullable = ModelUtils.isNullable(p) ? "" : "!";

        if (typeMapping.containsKey(schemaType)) {
            return typeMapping.get(schemaType) + nullable;
        }

        if (languageSpecificPrimitives.contains(schemaType)) {
            return schemaType + nullable;
        }

        return toModelName(schemaType);
    }

    @Override
    public String toEnumName(CodegenProperty property) {
        String enumName = toModelName(property.name);

        // Remove [] for array or map of ENUM
        enumName = enumName.replace("[]", "");

        // ENUM starts with a number
        if (enumName.matches("\\d.*")) {
            return StringUtils.capitalize("_" + enumName) + "Enum";
        } else {
            return StringUtils.capitalize(enumName) + "Enum";
        }
    }

    @Override
    public GeneratorLanguage generatorLanguage() { return GeneratorLanguage.JAVASCRIPT; }
}
