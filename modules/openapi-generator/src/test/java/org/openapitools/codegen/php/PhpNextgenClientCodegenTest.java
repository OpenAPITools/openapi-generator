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

package org.openapitools.codegen.php;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.parser.core.models.ParseOptions;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.languages.PhpNextgenClientCodegen;
import org.openapitools.codegen.testutils.ConfigAssert;
import org.testng.Assert;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.File;
import java.nio.file.Files;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

public class PhpNextgenClientCodegenTest {

    protected PhpNextgenClientCodegen codegen;

    @BeforeMethod
    public void before() {
        codegen = new PhpNextgenClientCodegen();
    }

    @Test
    public void testInitialConfigValues() throws Exception {
        final PhpNextgenClientCodegen codegen = new PhpNextgenClientCodegen();

        codegen.processOpts();

        Assert.assertEquals(codegen.isSupportStreaming(), false);
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final PhpNextgenClientCodegen codegen = new PhpNextgenClientCodegen();

        codegen.setSupportStreaming(true);

        codegen.processOpts();

        Assert.assertEquals(codegen.isSupportStreaming(), true);
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValuesWithFalseValue() throws Exception {
        final PhpNextgenClientCodegen codegen = new PhpNextgenClientCodegen();

        codegen.additionalProperties().put(PhpNextgenClientCodegen.SUPPORT_STREAMING, false);

        codegen.processOpts();
        ConfigAssert configAssert = new ConfigAssert(codegen.additionalProperties());

        configAssert.assertValue(PhpNextgenClientCodegen.SUPPORT_STREAMING, codegen::isSupportStreaming, Boolean.FALSE);
        Assert.assertEquals(codegen.isSupportStreaming(), false);
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValuesWithTrueValue() throws Exception {
        final PhpNextgenClientCodegen codegen = new PhpNextgenClientCodegen();

        codegen.additionalProperties().put(PhpNextgenClientCodegen.SUPPORT_STREAMING, true);

        codegen.processOpts();
        ConfigAssert configAssert = new ConfigAssert(codegen.additionalProperties());

        configAssert.assertValue(PhpNextgenClientCodegen.SUPPORT_STREAMING, codegen::isSupportStreaming, Boolean.TRUE);
        Assert.assertEquals(codegen.isSupportStreaming(), true);
    }

    @Test
    public void testEnumUnknownDefaultCaseDeserializationEnabled() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/bugs/issue_20593.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CodegenConstants.ENUM_UNKNOWN_DEFAULT_CASE, "true");

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        List<String> modelContent = Files
                .readAllLines(files.get("Pet.php").toPath())
                .stream()
                .map(String::trim)
                .collect(Collectors.toList());

        Assert.assertListContains(modelContent, a -> a.equals("$color = self::COLOR_UNKNOWN_DEFAULT_OPEN_API;"), "");
        Assert.assertListNotContains(modelContent, a -> a.equals("\"Invalid value '%s' for 'color', must be one of '%s'\","), "");
    }

    @Test
    public void testEnumUnknownDefaultCaseDeserializationDisabled() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/bugs/issue_20593.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        List<String> modelContent = Files
                .readAllLines(files.get("Pet.php").toPath())
                .stream()
                .map(String::trim)
                .collect(Collectors.toList());

        Assert.assertListNotContains(modelContent, a -> a.equals("$color = self::COLOR_UNKNOWN_DEFAULT_OPEN_API;"), "");
        Assert.assertListContains(modelContent, a -> a.equalsIgnoreCase("\"Invalid value '%s' for 'color', must be one of '%s'\","), "");
    }
}
