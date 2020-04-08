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

import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.SupportingFile;

import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.DocumentationFeature;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;

public class CSharpDotNet2ClientCodegen extends AbstractCSharpCodegen {
    private static final Logger LOGGER = LoggerFactory.getLogger(CSharpDotNet2ClientCodegen.class);

    public static final String CLIENT_PACKAGE = "clientPackage";
    protected String clientPackage = "Org.OpenAPITools.Client";
    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";

    public CSharpDotNet2ClientCodegen() {
        super();

        modifyFeatureSet(features -> features.includeDocumentationFeatures(DocumentationFeature.Readme));

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.DEPRECATED)
                .build();

        // clear import mapping (from default generator) as C# (2.0) does not use it
        // at the moment
        importMapping.clear();

        modelTemplateFiles.put("model.mustache", ".cs");
        apiTemplateFiles.put("api.mustache", ".cs");

        setApiPackage(packageName + ".Api");
        setModelPackage(packageName + ".Model");
        setClientPackage(packageName + ".Client");
        setSourceFolder("src" + File.separator + "main" + File.separator + "CsharpDotNet2");

        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");

        cliOptions.clear();
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME,
                "C# package name (convention: Camel.Case).")
                .defaultValue(packageName));
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_VERSION,
                "C# package version.")
                .defaultValue(packageVersion));
        cliOptions.add(new CliOption(CLIENT_PACKAGE,
                "C# client package name (convention: Camel.Case).")
                .defaultValue(clientPackage));
    }

    @Override
    public void processOpts() {
        LOGGER.warn("Per Microsoft Product Lifecycle (https://support.microsoft.com/en-us/lifecycle/search?sort=PN&alpha=.NET%20Framework&Filter=FilterNO), support for .NET Framework 2.0 ended in 2011 so there may be security issues using the auto-generated C# 2.0 source code.");

        super.processOpts();

        if (additionalProperties.containsKey(CLIENT_PACKAGE)) {
            setClientPackage((String) additionalProperties.get(CLIENT_PACKAGE));
        } else {
            additionalProperties.put(CLIENT_PACKAGE, getClientPackage());
        }

        final String clientPackage = getClientPackage();
        final String clientPackagePath = clientPackage.replace(".", java.io.File.separator);

        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);

        supportingFiles.add(new SupportingFile("Configuration.mustache",
                sourceFolder + File.separator + clientPackagePath, "Configuration.cs"));
        supportingFiles.add(new SupportingFile("ApiClient.mustache",
                sourceFolder + File.separator + clientPackagePath, "ApiClient.cs"));
        supportingFiles.add(new SupportingFile("ApiException.mustache",
                sourceFolder + File.separator + clientPackagePath, "ApiException.cs"));
        supportingFiles.add(new SupportingFile("packages.config.mustache", "vendor", "packages.config"));
        supportingFiles.add(new SupportingFile("compile-mono.sh.mustache", "", "compile-mono.sh"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));

    }

    @Override
    public String apiPackage() {
        return packageName + ".Api";
    }

    @Override
    public String modelPackage() {
        return packageName + ".Model";
    }

    public String getClientPackage() {
        return clientPackage;
    }

    public void setClientPackage(String clientPackage) {
        this.clientPackage = clientPackage;
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "csharp-dotnet2";
    }

    @Override
    public String getHelp() {
        return "Generates a C# .Net 2.0 client library (beta).";
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + apiPackage().replace('.', File.separatorChar);
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + modelPackage().replace('.', File.separatorChar);
    }

    @Override
    public String apiDocFileFolder() {
        return outputFolder + File.separator + apiDocPath.replace('/', File.separatorChar);
    }

    @Override
    public String modelDocFileFolder() {
        return outputFolder + File.separator + modelDocPath.replace('/', File.separatorChar);
    }

}
