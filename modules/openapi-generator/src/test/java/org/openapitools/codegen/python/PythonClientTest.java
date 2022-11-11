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
import org.openapitools.codegen.*;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.PythonClientCodegen;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.utils.ModelUtils;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;

@SuppressWarnings("static-method")
public class PythonClientTest {

    @Test(description = "tests RecursiveExampleValueWithCycle")
    public void testRecursiveExampleValueWithCycle() throws Exception {

        final OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/issue_7532.yaml");
        final PythonClientCodegen codegen = new PythonClientCodegen();
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
        final PythonClientCodegen codegen = new PythonClientCodegen();
        codegen.preprocessOpenAPI(openAPI);
    }

    @Test
    public void testSpecWithAcceptableVersion() {
        final OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/petstore.yaml");
        final PythonClientCodegen codegen = new PythonClientCodegen();
        codegen.preprocessOpenAPI(openAPI);
        Assert.assertEquals(openAPI.getOpenapi() , "3.0.0");
        Assert.assertTrue(openAPI.getExtensions() == null);
    }

    @Test
    public void testSpecWithAcceptableVersionAndExtension() {
        final OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/issue_12196.yaml");
        final PythonClientCodegen codegen = new PythonClientCodegen();
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
        final PythonClientCodegen codegen = new PythonClientCodegen();
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

    @Test
    public void testApiTestsNotGenerated() throws Exception {
        File output = Files.createTempDirectory("test").toFile();

        Map<String, String> globalProperties = Collections.singletonMap("apiTests", "false");
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGlobalProperties(globalProperties)
                .setGeneratorName("python")
                .setInputSpec("src/test/resources/3_0/petstore.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();
        Assert.assertTrue(files.size() > 0);

        Path pathThatShouldNotExist = output.toPath().resolve("openapi_client/test/test_paths");
        Assert.assertFalse(Files.isDirectory(pathThatShouldNotExist));
        output.deleteOnExit();
    }

    @Test
    public void testApisNotGenerated() throws Exception {
        File output = Files.createTempDirectory("test").toFile();

        Map<String, String> globalProperties = Collections.singletonMap("models", "");
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGlobalProperties(globalProperties)
                .setGeneratorName("python")
                .setInputSpec("src/test/resources/3_0/petstore.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();
        Assert.assertTrue(files.size() > 0);

        Path pathThatShouldNotExist = output.toPath().resolve("openapi_client/paths");
        Assert.assertFalse(Files.isDirectory(pathThatShouldNotExist));
        output.deleteOnExit();
    }

    @Test
    public void testRegexWithoutTrailingSlashWorks() {
        OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/11_regex.yaml");
        PythonClientCodegen codegen = new PythonClientCodegen();
        codegen.setOpenAPI(openAPI);

        String modelName = "UUID";
        Schema schema = openAPI.getComponents().getSchemas().get(modelName);

        CodegenModel cm = codegen.fromModel(modelName, schema);
        String expectedRegexPattern = "[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{12}";
        Assert.assertEquals(cm.getPattern(), expectedRegexPattern);
        Assert.assertEquals(cm.vendorExtensions.get("x-regex"), expectedRegexPattern);
        Assert.assertFalse(cm.vendorExtensions.containsKey("x-modifiers"));
    }

    @Test
    public void testRegexWithMultipleFlagsWorks() {
        OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/11_regex.yaml");
        PythonClientCodegen codegen = new PythonClientCodegen();
        codegen.setOpenAPI(openAPI);

        String modelName = "StringWithRegexWithThreeFlags";
        Schema schema = openAPI.getComponents().getSchemas().get(modelName);

        CodegenModel cm = codegen.fromModel(modelName, schema);
        String expectedRegexPattern = "a.";
        Assert.assertEquals(cm.getPattern(), expectedRegexPattern);
        Assert.assertEquals(cm.vendorExtensions.get("x-regex"), expectedRegexPattern);
        Assert.assertEquals(cm.vendorExtensions.get("x-modifiers"), Arrays.asList("DOTALL", "IGNORECASE", "MULTILINE"));
    }

    @Test
    public void testEnumNames() {
        OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/13942_schema_enum_names.yaml");
        PythonClientCodegen codegen = new PythonClientCodegen();
        codegen.setOpenAPI(openAPI);

        String modelName = "StringEnum";
        Schema schema = openAPI.getComponents().getSchemas().get(modelName);

        CodegenModel cm = codegen.fromModel(modelName, schema);

        ModelMap modelMap = new ModelMap();
        modelMap.setModel(cm);

        ModelsMap modelsMap = new ModelsMap();
        modelsMap.setModels(Collections.singletonList(modelMap));
        codegen.postProcessModels(modelsMap);

        ArrayList<Map<String, Object>> enumVars = (ArrayList<Map<String, Object>>) cm.getAllowableValues().get("enumVars");
        Assert.assertEquals(enumVars.size(), 2);
        Assert.assertEquals(enumVars.get(0).get("name"), "DIGIT_THREE_67B9C");
        Assert.assertEquals(enumVars.get(1).get("name"), "FFA5A4");
    }
}