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
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.*;
import java.io.PrintWriter;
import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.PythonExperimentalClientCodegen;
import org.openapitools.codegen.utils.ModelUtils;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import org.testng.reporters.jq.Model;

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

    @Test(description = "tests GeoJson Example for GeoJsonGeometry")
    public void testRecursiveGeoJsonExampleWhenTypeIsGeoJsonGeometry() throws IOException {

        testEndpointExampleValue("/geojson",
                                 "src/test/resources/3_0/issue_13043_recursive_model.yaml",
                                 "3_0/issue_13043_recursive_model_expected_value.txt");


    }

    @Test(description = "tests GeoJson Example for GeometryCollection")
    public void testRecursiveGeoJsonExampleWhenTypeIsGeometryCollection() throws IOException {

        testEndpointExampleValue("/geojson_geometry_collection",
                                 "src/test/resources/3_0/issue_13043_recursive_model.yaml",
                                 "3_0/issue_13043_geometry_collection_expected_value.txt");

    }

    private void testEndpointExampleValue(String endpoint, String specFilePath, String expectedAnswerPath) throws IOException {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec(specFilePath);
        final PythonExperimentalClientCodegen codegen = new PythonExperimentalClientCodegen();
        codegen.setOpenAPI(openAPI);

        final Operation operation = openAPI.getPaths().get(endpoint).getPost();
        Schema schema = ModelUtils.getSchemaFromRequestBody(operation.getRequestBody());
        String exampleValue = codegen.toExampleValue(schema, null);

        // uncomment if you need to regenerate the expected value
        //        PrintWriter printWriter = new PrintWriter("src/test/resources/" + expectedAnswerPath);
        //        printWriter.write(exampleValue);
        //        printWriter.close();
        //        org.junit.Assert.assertTrue(false);

        String expectedValue = Resources.toString(
                Resources.getResource(expectedAnswerPath),
                StandardCharsets.UTF_8);
        expectedValue = expectedValue.replaceAll("\\r\\n", "\n");
        Assert.assertEquals(exampleValue.trim(), expectedValue.trim());

    }

}
