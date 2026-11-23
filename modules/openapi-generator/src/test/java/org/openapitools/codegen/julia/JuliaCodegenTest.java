/*
 * Copyright 2026 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

package org.openapitools.codegen.julia;

import org.apache.commons.io.FileUtils;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class JuliaCodegenTest {

    @DataProvider
    public Object[][] generators() {
        return new Object[][]{{"julia-client"}, {"julia-server"}};
    }

    @Test(dataProvider = "generators")
    public void testByteArrayModelUsesJuliaTypes(String generatorName) throws IOException {
        Path output = Files.createTempDirectory("julia-byte-array");
        try {
            CodegenConfigurator configurator = new CodegenConfigurator()
                    .setGeneratorName(generatorName)
                    .setInputSpec("src/test/resources/3_0/julia/byte-array.yaml")
                    .setOutputDir(output.toString());

            new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

            // Both the field declaration and the runtime type metadata must contain Julia syntax.
            TestUtils.assertFileContains(output.resolve("src/models/model_Payload.jl"),
                    "encoded::Union{Nothing, Vector{UInt8}} = nothing",
                    "Symbol(\"encoded\")=>\"Vector{UInt8}\"");
        } finally {
            FileUtils.deleteDirectory(output.toFile());
        }
    }
}
