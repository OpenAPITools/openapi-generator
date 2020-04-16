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

package org.openapitools.codegen.python;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.StringSchema;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.PythonClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.ArrayList;
import java.util.Arrays;

public class PythonClientCodegenTest {

    @Test
    public void testInitialConfigValues() throws Exception {
        final PythonClientCodegen codegen = new PythonClientCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final PythonClientCodegen codegen = new PythonClientCodegen();
        codegen.setHideGenerationTimestamp(false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final PythonClientCodegen codegen = new PythonClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
    }

    @Test(description = "test enum null/nullable patterns")
    public void testEnumNull() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/issue_1997.yaml");

        StringSchema prop = (StringSchema) openAPI.getComponents().getSchemas().get("Type").getProperties().get("prop");
        ArrayList<Object> expected = new ArrayList<>(Arrays.asList("A", "B", "C"));
        assert prop.getNullable();
        assert prop.getEnum().equals(expected);
    }

    @Test(description = "test regex patterns")
    public void testRegularExpressionOpenAPISchemaVersion3() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/issue_1517.yaml");
        final PythonClientCodegen codegen = new PythonClientCodegen();
        codegen.setOpenAPI(openAPI);
        final String path = "/ping";
        final Operation p = openAPI.getPaths().get(path).getGet();
        final CodegenOperation op = codegen.fromOperation(path, "get", p, null);
        // pattern_no_forward_slashes '^pattern$'
        Assert.assertEquals(op.allParams.get(0).pattern, "/^pattern$/");
        // pattern_two_slashes '/^pattern$/'
        Assert.assertEquals(op.allParams.get(1).pattern, "/^pattern$/");
        // pattern_dont_escape_backslash '/^pattern\d{3}$/'
        Assert.assertEquals(op.allParams.get(2).pattern, "/^pattern\\d{3}$/");
        // pattern_dont_escape_escaped_forward_slash '/^pattern\/\d{3}$/'
        Assert.assertEquals(op.allParams.get(3).pattern, "/^pattern\\/\\d{3}$/");
        // pattern_escape_unescaped_forward_slash '^pattern/\d{3}$'
        Assert.assertEquals(op.allParams.get(4).pattern, "/^pattern\\/\\d{3}$/");
        // pattern_with_modifiers '/^pattern\d{3}$/i
        Assert.assertEquals(op.allParams.get(5).pattern, "/^pattern\\d{3}$/i");
    }

    @Test(description = "test single quotes escape")
    public void testSingleQuotes() {
        final PythonClientCodegen codegen = new PythonClientCodegen();
        StringSchema schema = new StringSchema();
        schema.setDefault("Text containing 'single' quote");
        String defaultValue = codegen.toDefaultValue(schema);
        Assert.assertEquals("'Text containing \'single\' quote'", defaultValue);
    }
}
