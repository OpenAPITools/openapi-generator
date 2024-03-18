/*
 * Copyright 2020 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

package org.openapitools.codegen.csharpnetcore;

import static org.junit.Assert.assertNotNull;
import static org.openapitools.codegen.TestUtils.assertFileContains;
import static org.openapitools.codegen.TestUtils.assertFileExists;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.Schema;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.CSharpClientCodegen;
import org.openapitools.codegen.languages.JavaClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

public class CSharpClientCodegenTest {

    @Test
    public void testToEnumVarName() throws Exception {
        final CSharpClientCodegen codegen = new CSharpClientCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.toEnumVarName("FooBar", "string"), "FooBar");
        Assert.assertEquals(codegen.toEnumVarName("fooBar", "string"), "FooBar");
        Assert.assertEquals(codegen.toEnumVarName("foo-bar", "string"), "FooBar");
        Assert.assertEquals(codegen.toEnumVarName("foo_bar", "string"), "FooBar");
        Assert.assertEquals(codegen.toEnumVarName("foo bar", "string"), "FooBar");

        // The below cases do not work currently, camelize doesn't support uppercase
        // Assert.assertEquals(codegen.toEnumVarName("FOO-BAR", "string"), "FooBar");
        // Assert.assertEquals(codegen.toEnumVarName("FOO_BAR", "string"), "FooBar");
    }

    @Test
    public void testUnsigned() {
        // test unsigned integer/long
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/unsigned-test.yaml");
        CSharpClientCodegen codegen = new CSharpClientCodegen();

        Schema test1 = openAPI.getComponents().getSchemas().get("format_test");
        codegen.setOpenAPI(openAPI);
        codegen.processOpts();
        CodegenModel cm1 = codegen.fromModel("format_test", test1);
        Assert.assertEquals(cm1.getClassname(), "FormatTest");

        final CodegenProperty property1 = cm1.allVars.get(2);
        Assert.assertEquals(property1.baseName, "unsigned_integer");
        Assert.assertEquals(property1.dataType, "uint");
        Assert.assertEquals(property1.vendorExtensions.get("x-unsigned"), Boolean.TRUE);
        Assert.assertTrue(property1.isPrimitiveType);
        Assert.assertTrue(property1.isInteger);
        Assert.assertFalse(property1.isContainer);
        Assert.assertFalse(property1.isFreeFormObject);
        Assert.assertFalse(property1.isAnyType);

        final CodegenProperty property2 = cm1.allVars.get(4);
        Assert.assertEquals(property2.baseName, "unsigned_long");
        Assert.assertEquals(property2.dataType, "ulong");
        Assert.assertEquals(property2.vendorExtensions.get("x-unsigned"), Boolean.TRUE);
        Assert.assertTrue(property2.isPrimitiveType);
        Assert.assertTrue(property2.isLong);
        Assert.assertFalse(property2.isContainer);
        Assert.assertFalse(property2.isFreeFormObject);
        Assert.assertFalse(property2.isAnyType);
    }

    @Test
    public void testHandleConstantParams() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/java/autoset_constant.yaml");
        final DefaultGenerator defaultGenerator = new DefaultGenerator();
        final ClientOptInput clientOptInput = new ClientOptInput();
        clientOptInput.openAPI(openAPI);
        CSharpClientCodegen cSharpClientCodegen = new CSharpClientCodegen();
        cSharpClientCodegen.setOutputDir(output.getAbsolutePath());
        cSharpClientCodegen.additionalProperties().put(CodegenConstants.AUTOSET_CONSTANTS, "true");
        cSharpClientCodegen.setAutosetConstants(true);
        clientOptInput.config(cSharpClientCodegen);
        defaultGenerator.opts(clientOptInput);

        Map<String, File> files = defaultGenerator.generate().stream()
                .collect(Collectors.toMap(File::getPath, Function.identity()));

        File apiFile = files
                .get(Paths.get(output.getAbsolutePath(), "src", "Org.OpenAPITools", "Api", "HelloExampleApi.cs").toString());
        assertNotNull(apiFile);
        assertFileContains(apiFile.toPath(),
                "localVarRequestOptions.HeaderParameters.Add(\"X-CUSTOM_CONSTANT_HEADER\", Org.OpenAPITools.Client.ClientUtils.ParameterToString(\"CONSTANT_VALUE\"));");
    }
}
