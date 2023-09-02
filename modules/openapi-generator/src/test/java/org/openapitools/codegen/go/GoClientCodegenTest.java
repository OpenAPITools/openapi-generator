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

package org.openapitools.codegen.go;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import org.openapitools.codegen.*;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.GoClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


public class GoClientCodegenTest {

    @Test
    public void testInitialConfigValues() throws Exception {
        final GoClientCodegen codegen = new GoClientCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertTrue(codegen.isHideGenerationTimestamp());
        Assert.assertNull(codegen.additionalProperties().get(GoClientCodegen.MODEL_FILE_FOLDER));
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final GoClientCodegen codegen = new GoClientCodegen();
        codegen.setHideGenerationTimestamp(false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertFalse(codegen.isHideGenerationTimestamp());
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final GoClientCodegen codegen = new GoClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertFalse(codegen.isHideGenerationTimestamp());
    }

    @Test(description = "test example value for body parameter")
    public void bodyParameterTest() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml");
        final GoClientCodegen codegen = new GoClientCodegen();
        codegen.setOpenAPI(openAPI);
        final String path = "/fake";
        final Operation p = openAPI.getPaths().get(path).getGet();
        final CodegenOperation op = codegen.fromOperation(path, "post", p, null);
        Assert.assertEquals(op.formParams.size(), 2);
        CodegenParameter bp = op.formParams.get(0);
        Assert.assertFalse(bp.isPrimitiveType);
    }

    @Test(description = "test to ensure the parameter names are unique")
    public void ensureParameterNameUniqueTest() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/conflictingParameter.yaml");
        final GoClientCodegen codegen = new GoClientCodegen();
        codegen.setOpenAPI(openAPI);
        final String path = "/pet/{id}";
        final Operation p = openAPI.getPaths().get(path).getPost();
        final CodegenOperation op = codegen.fromOperation(path, "post", p, null);
        Assert.assertEquals(op.allParams.size(), 9);
        CodegenParameter cp = op.allParams.get(0);
        Assert.assertEquals(cp.paramName, "id");
        CodegenParameter cp2 = op.allParams.get(1);
        Assert.assertEquals(cp2.paramName, "id2");
        CodegenParameter cp3 = op.allParams.get(2);
        Assert.assertEquals(cp3.paramName, "id3");
        CodegenParameter cp4 = op.allParams.get(3);
        Assert.assertEquals(cp4.paramName, "id4");
        CodegenParameter cp5 = op.allParams.get(4);
        Assert.assertEquals(cp5.paramName, "id5");
    }

    @Test
    public void testFilenames() throws Exception {
        final GoClientCodegen codegen = new GoClientCodegen();

        // Model names are generated from schema / definition names
        Assert.assertEquals(codegen.toModelFilename("Animal"), "model_animal");
        Assert.assertEquals(codegen.toModelFilename("AnimalTest"), "model_animal_test_");
        Assert.assertEquals(codegen.toModelFilename("AnimalFarm"), "model_animal_farm");
        Assert.assertEquals(codegen.toModelFilename("AnimalFarmTest"), "model_animal_farm_test_");

        // API names are generated from tag names
        Assert.assertEquals(codegen.toApiFilename("Animal"), "api_animal");
        Assert.assertEquals(codegen.toApiFilename("Animal Test"), "api_animal_test_");
        Assert.assertEquals(codegen.toApiFilename("Animal Farm"), "api_animal_farm");
        Assert.assertEquals(codegen.toApiFilename("Animal Farm Test"), "api_animal_farm_test_");
    }

    @Test
    public void testPrimitiveTypeInOneOf() throws IOException {
        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("go")
                .setInputSpec("src/test/resources/3_0/oneOf_primitive.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();
        System.out.println(files);
        files.forEach(File::deleteOnExit);

        Path modelFile = Paths.get(output + "/model_example.go");
        TestUtils.assertFileContains(modelFile, "Child *Child");
        TestUtils.assertFileContains(modelFile, "Int32 *int32");
        TestUtils.assertFileContains(modelFile, "dst.Int32");
        TestUtils.assertFileNotContains(modelFile, "int32 *int32");
        TestUtils.assertFileNotContains(modelFile, "dst.int32");
    }

    @Test
    public void testNullableComposition() throws IOException {
        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("go")
                .setInputSpec("src/test/resources/3_0/allOf_nullable.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        TestUtils.assertFileContains(Paths.get(output + "/model_example.go"), "Child NullableChild");
    }

    @Test
    public void testMultipleRequiredPropertiesHasSameOneOfObject() throws IOException {
        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("go")
                .setInputSpec("src/test/resources/3_0/petstore-multiple-required-properties-has-same-oneOf-object.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();
        System.out.println(files);
        files.forEach(File::deleteOnExit);

        Path docFile = Paths.get(output + "/docs/PetAPI.md");
        TestUtils.assertFileContains(docFile, "openapiclient.pet{Cat: openapiclient.NewCat(\"Attr_example\")}, openapiclient.pet{Cat: openapiclient.NewCat(\"Attr_example\")}, openapiclient.pet{Cat: openapiclient.NewCat(\"Attr_example\")}");
    }

    @Test
    public void testStructPrefix() throws IOException {
        Map<String, Object> properties = new HashMap<>();
        properties.put(GoClientCodegen.STRUCT_PREFIX, true);

        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("go")
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_0/petstore.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        TestUtils.assertFileContains(Paths.get(output + "/api_pet.go"), "type PetAPIAddPetRequest struct");
    }

    @Test
    public void testAdditionalPropertiesModelFileFolder() throws Exception {
        final GoClientCodegen codegen = new GoClientCodegen();
        codegen.additionalProperties().put(GoClientCodegen.MODEL_FILE_FOLDER, "model_dir");
        codegen.processOpts();

        Assert.assertEquals(codegen.modelFileFolder(), "generated-code/go/model_dir/".replace("/", File.separator));
    }
    
    @Test
    public void verifyTestFile() throws IOException {
        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("go")
                .setInputSpec("src/test/resources/3_0/petstore.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        TestUtils.assertFileExists(Paths.get(output + "/test/api_pet_test.go"));
        TestUtils.assertFileContains(Paths.get(output + "/test/api_pet_test.go"),
                "func Test_openapi_PetAPIService(t *testing.T) {");
    }

    @Test
    public void verifyTestImport() throws IOException {
        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("go")
                .setGitUserId("OpenAPITools")
                .setGitRepoId("openapi-generator")
                .setInputSpec("src/test/resources/3_0/petstore.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        TestUtils.assertFileExists(Paths.get(output + "/test/api_pet_test.go"));
        TestUtils.assertFileContains(Paths.get(output + "/test/api_pet_test.go"),
                "openapiclient \"github.com/OpenAPITools/openapi-generator\"");
    }

    @Test
    public void verifyFormatErrorMessageInUse() throws IOException {
        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("go")
                .setInputSpec("src/test/resources/3_0/go/petstore-with-problem-details.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        TestUtils.assertFileExists(Paths.get(output + "/api_pet.go"));
        TestUtils.assertFileContains(Paths.get(output + "/api_pet.go"),
                "newErr.error = formatErrorMessage(localVarHTTPResponse.Status, &v)");
    }

    @Test
    public void verifyApiTestWithNullResponse() throws IOException {
        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("go")
                .setGitUserId("OpenAPITools")
                .setGitRepoId("openapi-generator")
                .setInputSpec("src/test/resources/3_0/go/petstore-with-no-response-body.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        TestUtils.assertFileExists(Paths.get(output + "/test/api_pet_test.go"));
        TestUtils.assertFileNotContains(Paths.get(output + "/test/api_pet_test.go"),
                "require.NotNil(t, resp)");
        TestUtils.assertFileNotContains(Paths.get(output + "/test/api_pet_test.go"),
                "resp, httpRes, err := apiClient.PetAPI.PetDelete(context.Background()).Execute()");
        TestUtils.assertFileContains(Paths.get(output + "/test/api_pet_test.go"),
                "httpRes, err := apiClient.PetAPI.PetDelete(context.Background()).Execute()");
    }

}
