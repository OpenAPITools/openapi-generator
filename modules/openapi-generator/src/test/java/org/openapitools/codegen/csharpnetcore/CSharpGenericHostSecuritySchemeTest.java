/*
 * Copyright 2026 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 */
package org.openapitools.codegen.csharpnetcore;

import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.testng.annotations.Test;

import java.nio.file.Path;

import static org.openapitools.codegen.TestUtils.assertFileContains;
import static org.openapitools.codegen.TestUtils.assertFileNotContains;
import static org.openapitools.codegen.TestUtils.newTempFolder;

public class CSharpGenericHostSecuritySchemeTest {
    @Test
    public void testUnusedSecuritySchemeDoesNotRequireTokenProvider() throws Exception {
        Path output = newTempFolder();
        CodegenConfigurator config = new CodegenConfigurator().setGeneratorName("csharp").setLibrary("generichost")
                .setInputSpec("src/test/resources/3_0/csharp/unused-security-scheme.yaml")
                .setOutputDir(output.toString());
        new DefaultGenerator().opts(config.toClientOptInput()).generate();

        assertFileNotContains(output.resolve("src/Org.OpenAPITools/Api/ThingsApi.cs"),
                "public TokenProvider<ApiKeyToken> ApiKeyProvider { get; }",
                "TokenProvider<ApiKeyToken> apiKeyProvider");
    }

    @Test
    public void testTokenProviderOnlyAddedToApisUsingTheScheme() throws Exception {
        Path output = newTempFolder();
        CodegenConfigurator config = new CodegenConfigurator().setGeneratorName("csharp").setLibrary("generichost")
                .setInputSpec("src/test/resources/3_0/csharp/mixed-security-scheme.yaml")
                .setOutputDir(output.toString());
        new DefaultGenerator().opts(config.toClientOptInput()).generate();

        assertFileContains(output.resolve("src/Org.OpenAPITools/Api/SecuredApi.cs"),
                "public TokenProvider<ApiKeyToken> ApiKeyProvider { get; }",
                "TokenProvider<ApiKeyToken> apiKeyProvider",
                "await ApiKeyProvider.GetAsync(\"X-API-KEY\"");
        assertFileNotContains(output.resolve("src/Org.OpenAPITools/Api/OpenApi.cs"),
                "public TokenProvider<ApiKeyToken> ApiKeyProvider { get; }",
                "TokenProvider<ApiKeyToken> apiKeyProvider");
    }
}
