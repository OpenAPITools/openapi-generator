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

package org.openapitools.codegen.powershell;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.parser.core.models.ParseOptions;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.languages.PowerShellClientCodegen;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

import static org.openapitools.codegen.TestUtils.assertFileContains;
import static org.openapitools.codegen.TestUtils.assertFileNotContains;

public class PowerShellClientCodegenTest {

    /**
     * Regression test for <a href="https://github.com/OpenAPITools/openapi-generator/issues/23535">#23535</a>.
     *
     * PowerShell treats {@code $} inside double-quoted strings as the sigil for
     * variable interpolation, so hash-table keys emitted as {@code "$foo"} get
     * rewritten at runtime to the value of {@code $foo} (usually empty). This
     * produces invalid DTO commandlets whenever an OpenAPI property name starts
     * with (or contains) {@code $}. The {@code model_simple.mustache} template
     * now emits single-quoted keys so the literal baseName is preserved.
     */
    @Test
    public void dollarSignPropertyNamesAreSingleQuoted() throws IOException {
        File output = Files.createTempDirectory("test-powershell-23535").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/dollar-in-names-pull14359.yaml", null, new ParseOptions())
                .getOpenAPI();

        PowerShellClientCodegen codegen = new PowerShellClientCodegen();
        codegen.setOutputDir(output.getAbsolutePath());

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");
        generator.opts(input).generate();

        // The PowerShell codegen sanitises "$DollarModel$" to "DollarModel",
        // so the emitted model lives at src/<package>/Model/DollarModel.ps1.
        java.nio.file.Path dollarModelPs1 = Paths.get(outputPath + "/src/PSOpenAPITools/Model/DollarModel.ps1");

        // Every site where model_simple.mustache emits a user-supplied baseName
        // must use single-quoted PowerShell strings so `$` is treated literally:
        //
        //   1. The `Initialize-…` hash literal: `'$dollarValue$' = ${DollarValue}`
        //   2. The JSON round-trip hash literal in `ConvertFrom-…JsonTo…`
        //   3. The property-allowlist array: `$AllProperties = ('$dollarValue$')`
        //   4. The property-indexer lookup: `$JsonParameters.PSobject.Properties['$dollarValue$'].value`
        //   5. The presence-check regex: `-match '$dollarValue$'`
        assertFileContains(dollarModelPs1,
                "'$dollarValue$' = ${DollarValue}",
                "$AllProperties = ('$dollarValue$')",
                "$JsonParameters.PSobject.Properties['$dollarValue$'].value",
                "-match '$dollarValue$'");

        // The previous double-quoted emissions must no longer appear anywhere in
        // the generated model file.
        assertFileNotContains(dollarModelPs1,
                "\"$dollarValue$\" = ",
                "$AllProperties = (\"$dollarValue$\")",
                "$JsonParameters.PSobject.Properties[\"$dollarValue$\"]",
                "-match \"$dollarValue$\"");
    }
}
