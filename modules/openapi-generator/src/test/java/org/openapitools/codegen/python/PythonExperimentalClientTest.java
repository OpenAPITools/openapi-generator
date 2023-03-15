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

package org.openapitools.codegen.python;

import com.google.common.io.Resources;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.*;
import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.PythonExperimentalClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.nio.charset.StandardCharsets;

@SuppressWarnings("static-method")
public class PythonExperimentalClientTest {

    @Test(description = "tests RecursiveExampleValueWithCycle")
    public void testRecursiveExampleValueWithCycle() throws Exception {

        final OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/issue_7532.yaml");
        final PythonExperimentalClientCodegen codegen = new PythonExperimentalClientCodegen();
        codegen.setOpenAPI(openAPI);
        Schema schemaWithCycleInTreesProperty = openAPI.getComponents().getSchemas().get("Forest");
        String exampleValue = codegen.toExampleValue(schemaWithCycleInTreesProperty, null);

        String expectedValue = Resources.toString(
                Resources.getResource("3_0/issue_7532_tree_example_value_expected.txt"),
                StandardCharsets.UTF_8);
        expectedValue = expectedValue.replaceAll("\\r\\n", "\n");
        Assert.assertEquals(exampleValue.trim(), expectedValue.trim());
    }

    @Test
    public void testSpecWithTooLowVersionThrowsException() throws RuntimeException {
        final OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/2_0/petstore.yaml");
        final PythonExperimentalClientCodegen codegen = new PythonExperimentalClientCodegen();
        codegen.preprocessOpenAPI(openAPI);
    }

    @Test
    public void testSpecWithAcceptableVersion() {
        final OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/petstore.yaml");
        final PythonExperimentalClientCodegen codegen = new PythonExperimentalClientCodegen();
        codegen.preprocessOpenAPI(openAPI);
        Assert.assertEquals(openAPI.getOpenapi() , "3.0.0");
        Assert.assertTrue(openAPI.getExtensions() == null);
    }

    @Test
    public void testSpecWithAcceptableVersionAndExtension() {
        final OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/issue_12196.yaml");
        final PythonExperimentalClientCodegen codegen = new PythonExperimentalClientCodegen();
        codegen.preprocessOpenAPI(openAPI);
        Assert.assertEquals(openAPI.getOpenapi() , "3.0.0");
        Assert.assertFalse(openAPI.getExtensions().isEmpty());
        Assert.assertFalse(openAPI.getExtensions().containsValue("x-original-swagger-version"));
    }

}
