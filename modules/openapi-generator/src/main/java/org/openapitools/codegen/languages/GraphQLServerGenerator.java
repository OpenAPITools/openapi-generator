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

import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;

import java.io.File;

public class GraphQLServerGenerator extends AbstractGraphQLCodegen implements CodegenConfig {

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "graphql-server";
    }

    @Override
    public String getHelp() {
        return "Generates a GraphQL express server including it's types, queries, mutations, (resolvers)";
    }

    public GraphQLServerGenerator() {
        super();

        packageName = "openapi3graphql-server";
        packageVersion = "1.0.0";

        outputFolder = "generated-code/graphql-server";
        embeddedTemplateDir = templateDir = "graphql-server";

        hideGenerationTimestamp = Boolean.TRUE;

        apiTemplateFiles.put("api.mustache", ".graphql");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");
        modelTemplateFiles.put("model.mustache", ".graphql");
        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        // TODO check why api doc is not written

        cliOptions.clear();
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME, "GraphQL express server package name (convention: lowercase).")
                .defaultValue("openapi3graphql-server"));
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_VERSION, "GraphQL express server package version.")
                .defaultValue("1.0.0"));
        cliOptions.add(new CliOption(CodegenConstants.HIDE_GENERATION_TIMESTAMP, CodegenConstants.HIDE_GENERATION_TIMESTAMP_DESC)
                .defaultValue(Boolean.TRUE.toString()));
    }

    @Override
    public void processOpts() {
        super.processOpts();

        //apiTestTemplateFiles.put("api_test.mustache", ".graphql");

        //modelTestTemplateFiles.put("model_test.mustache", ".graphql");
        modelTestTemplateFiles.clear(); // TODO: add model test template

        modelPackage = packageName;
        apiPackage = packageName;

        String supportFolder = apiPackage().replace('.', File.separatorChar);

        // Dynamic express/graphql related stuff
        // supportingFiles.add(new SupportingFile("operations.mustache", supportFolder, "operations.js"));
        // TODO supportingFiles.add(new SupportingFile("type-defs.mustache", supportFolder, "type-defs.js"));
        // TODO supportingFiles.add(new SupportingFile("operations.mustache", supportFolder, "operations.js"));

        // General stuff
        supportingFiles.add(new SupportingFile(".gitignore", supportFolder, ".gitignore"));
        supportingFiles.add(new SupportingFile("README.mustache", supportFolder, "README.md"));
        supportingFiles.add(new SupportingFile("package.json.mustache", supportFolder, "package.json"));
        supportingFiles.add(new SupportingFile("server.js", supportFolder, "server.js"));
        supportingFiles.add(new SupportingFile("start.js", supportFolder, "start.js"));
    }

    @Override
    public String toEnumName(CodegenProperty property) {
        String enumName = toModelName(property.name);

        // remove [] for array or map of enum
        enumName = enumName.replace("[]", "");

        if (enumName.matches("\\d.*")) { // starts with number
            return StringUtils.capitalize("_" + enumName) + "Enum";
        } else {
            return StringUtils.capitalize(enumName) + "Enum";
        }
    }
}
