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
import static org.openapitools.codegen.TestUtils.newTempFolder;

public class CSharpGenericHostOperationServerTest {
    @Test
    public void testOperationServerRetainsPortAndAppendsPath() throws Exception {
        Path output = newTempFolder();
        CodegenConfigurator config = new CodegenConfigurator().setGeneratorName("csharp").setLibrary("generichost")
                .setInputSpec("src/test/resources/3_0/csharp/operation-server-path.yaml")
                .setOutputDir(output.toString()).addAdditionalProperty("packageName", "ServerRepro");
        new DefaultGenerator().opts(config.toClientOptInput()).generate();
        assertFileContains(output.resolve("src/ServerRepro/Api/DefaultApi.cs"),
                "uriBuilderLocalVar.Host = urlLocalVar.Host;",
                "uriBuilderLocalVar.Port = urlLocalVar.Port;",
                "string.Concat(urlLocalVar.AbsolutePath.TrimEnd('/'), \"/files/{id}\")",
                "uriBuilderLocalVar.Path.Replace(\"%7Bid%7D\"");
    }
}
