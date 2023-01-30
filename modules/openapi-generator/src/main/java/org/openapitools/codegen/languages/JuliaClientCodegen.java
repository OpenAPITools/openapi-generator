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
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;

import java.io.File;

public class JuliaClientCodegen extends AbstractJuliaCodegen {
    /**
     * Configures the type of generator.
     *
     * @return the CodegenType for this generator
     * @see org.openapitools.codegen.CodegenType
     */
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    /**
     * Configures the name of the generator.
     * This will be used to refer to the generator when configuration is read from config files.
     *
     * @return the name of the generator
     */
    public String getName() {
        return "julia-client";
    }

    /**
     * Configures a help message for the generator.
     * TODO: add parameters, tips here
     *
     * @return the help message for the generator
     */
    public String getHelp() {
        return "Generates a julia client.";
    }

    public JuliaClientCodegen() {
        super();

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();

        outputFolder = "generated-code" + File.separator + "julia-client";
        modelTemplateFiles.put("model.mustache", ".jl");
        apiTemplateFiles.put("api.mustache", ".jl");
        embeddedTemplateDir = templateDir = "julia-client";
        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");

        // apiPackage = "Apis";
        // modelPackage = "Models";
        supportingFiles.clear();
        supportingFiles.add(new SupportingFile("README.mustache", "README.md"));

        cliOptions.clear();
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME, "Julia client package name.").defaultValue("APIClient"));
        cliOptions.add(new CliOption("exportModels", "Whether to generate code to export model names.").defaultValue("false"));
        cliOptions.add(new CliOption("exportOperations", "Whether to generate code to export operation names.").defaultValue("false"));
    }

    @Override
    public void processOpts() {
        super.processOpts();
        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
        } else {
            setPackageName("APIClient");
            additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        }

        if (additionalProperties.containsKey("exportModels")) {
            setExportModels(Boolean.parseBoolean((String) additionalProperties.get("exportModels")));
        }
        additionalProperties.put("exportModels", exportModels);

        if (additionalProperties.containsKey("exportOperations")) {
            setExportModels(Boolean.parseBoolean((String) additionalProperties.get("exportOperations")));
        }
        additionalProperties.put("exportOperations", exportModels);

        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);
        supportingFiles.add(new SupportingFile("client.mustache", srcPath, packageName + ".jl"));
        supportingFiles.add(new SupportingFile("modelincludes.mustache", srcPath, "modelincludes.jl"));
    }
}
