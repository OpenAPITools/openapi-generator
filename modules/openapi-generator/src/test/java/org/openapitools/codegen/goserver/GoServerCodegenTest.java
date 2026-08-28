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

package org.openapitools.codegen.goserver;

import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;

public class GoServerCodegenTest {

    @Test
    public void verifyGoMod() throws IOException {
        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = createDefaultCodegenConfigurator(output)
                .setInputSpec("src/test/resources/3_0/go-server/route-order.yaml");

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        TestUtils.assertFileExists(Paths.get(output + "/go.mod"));
        TestUtils.assertFileContains(Paths.get(output + "/go.mod"),
                "module github.com/my-user/my-repo");
        TestUtils.assertFileContains(Paths.get(output + "/go.mod"),
                "require github.com/gorilla/mux v1.8.0");
    }

    @Test
    public void verifyOrder() throws IOException {
        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = createDefaultCodegenConfigurator(output)
                .setInputSpec("src/test/resources/3_0/go-server/route-order.yaml");

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        TestUtils.assertFileExists(Paths.get(output + "/go/routers.go"));
        TestUtils.assertFileContains(Paths.get(output + "/go/routers.go"),
                "type Routes map[string]Route");

        TestUtils.assertFileExists(Paths.get(output + "/go/api_dev.go"));
        // verify /getPath/latest is first route
        Assert.assertEquals(Files.readAllLines(Paths.get(output + "/go/api_dev.go")).get(52), "\t\t\"GetLatest\": Route{");
        // verify /getPath/{id} is second route
        Assert.assertEquals(Files.readAllLines(Paths.get(output + "/go/api_dev.go")).get(58), "\t\t\"GetById\": Route{");

    }

    @Test
    public void verifyDuplicateOperationIdsAreConsistent() throws IOException {
        // Regression test for duplicate auto-generated operationIds (e.g. /foo and /foo/).
        // Previously nickname and operationId were deduplicated by two independent passes with
        // inconsistent counter conventions (_1 vs _0), so the route map / interfaces used
        // {{operationId}} (e.g. FooGet_0) while the controller handler / service stub used
        // {{nickname}} (e.g. FooGet_1), producing uncompilable code.
        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = createDefaultCodegenConfigurator(output)
                .setInputSpec("src/test/resources/3_0/go-server/duplicate-operation-ids.yaml");

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        // The deduplicated operationId pair is FooGet and FooGet_0; routes, handlers and
        // service stubs must all reference the same name (templates use {{operationId}}).
        TestUtils.assertFileContains(Paths.get(output + "/go/api_default.go"),
                "\"FooGet\": Route{");
        TestUtils.assertFileContains(Paths.get(output + "/go/api_default.go"),
                "\"FooGet_0\": Route{");

        // Route handler reference (uses {{operationId}}) and handler method definition
        // (uses {{nickname}}) must reference the same name.
        TestUtils.assertFileContains(Paths.get(output + "/go/api_default.go"),
                "c.FooGet_0,");
        TestUtils.assertFileContains(Paths.get(output + "/go/api_default.go"),
                "func (c *DefaultAPIController) FooGet_0(");
        // Service call inside the handler must also use the deduplicated operationId.
        TestUtils.assertFileContains(Paths.get(output + "/go/api_default.go"),
                "c.service.FooGet_0(");

        // The interface (api.go) and the service stub must declare FooGet_0 too.
        TestUtils.assertFileContains(Paths.get(output + "/go/api.go"),
                "FooGet_0(");
        TestUtils.assertFileContains(Paths.get(output + "/go/api_default_service.go"),
                "func (s *DefaultAPIService) FooGet_0(");

        // Negative assertions: the divergent _1 name must not appear anywhere.
        String apiDefault = new String(Files.readAllBytes(Paths.get(output + "/go/api_default.go")), StandardCharsets.UTF_8);
        String api = new String(Files.readAllBytes(Paths.get(output + "/go/api.go")), StandardCharsets.UTF_8);
        String apiService = new String(Files.readAllBytes(Paths.get(output + "/go/api_default_service.go")), StandardCharsets.UTF_8);
        Assert.assertFalse(apiDefault.contains("FooGet_1"),
                "nickname diverged from operationId (FooGet_1 present in api_default.go)");
        Assert.assertFalse(api.contains("FooGet_1"),
                "nickname diverged from operationId (FooGet_1 present in api.go)");
        Assert.assertFalse(apiService.contains("FooGet_1"),
                "nickname diverged from operationId (FooGet_1 present in api_default_service.go)");
    }

    @Test
    public void verifyRequiredZeroValueAllowed() throws IOException {
        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = createDefaultCodegenConfigurator(output)
                .setInputSpec("src/test/resources/3_0/go-server/required-zero-value.yaml");

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        TestUtils.assertFileExists(Paths.get(output + "/go/model_test_object.go"));
        TestUtils.assertFileContains(Paths.get(output + "/go/model_test_object.go"),
                "func (o *TestObject) UnmarshalJSON(data []byte) (err error) {");
        TestUtils.assertFileContains(Paths.get(output + "/go/model_test_object.go"),
                "var decoded TestObject");
        TestUtils.assertFileContains(Paths.get(output + "/go/model_test_object.go"),
                "requiredProperties := []string{");
        TestUtils.assertFileContains(Paths.get(output + "/go/model_test_object.go"),
                "allowedJsonKeys := map[string]struct{}{");
        TestUtils.assertFileContains(Paths.get(output + "/go/model_test_object.go"),
                "return fmt.Errorf(\"json: unknown field %q\", key)");
        TestUtils.assertFileContains(Paths.get(output + "/go/model_test_object.go"),
                "json.Unmarshal(value, &decoded.N)");
        TestUtils.assertFileContains(Paths.get(output + "/go/model_test_object.go"),
                "json.Unmarshal(value, &decoded.Name)");
        TestUtils.assertFileContains(Paths.get(output + "/go/model_test_object.go"),
                "return &RequiredError{Field: requiredProperty}");
        TestUtils.assertFileContains(Paths.get(output + "/go/model_test_object.go"),
                "if string(value) == \"null\" && !requiredNullableProperties[requiredProperty] {");
        TestUtils.assertFileNotContains(Paths.get(output + "/go/model_test_object.go"),
                "IsZeroValue");

        TestUtils.assertFileExists(Paths.get(output + "/go/api_default.go"));
        TestUtils.assertFileContains(Paths.get(output + "/go/api_default.go"),
                "var requiredErr *RequiredError");
    }

    @Test
    public void verifyOneOfInlineRequiredZeroValueAllowed() throws IOException {
        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = createDefaultCodegenConfigurator(output)
                .setInputSpec("src/test/resources/3_0/go-server/oneof-inline-required.yaml");

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        java.nio.file.Path modelPath = Paths.get(output + "/go/model_item_request.go");
        TestUtils.assertFileExists(modelPath);
        TestUtils.assertFileContains(modelPath,
                "func (o *ItemRequest) UnmarshalJSON(data []byte) (err error) {");
        TestUtils.assertFileContains(modelPath,
                "\"quantity\",");
        TestUtils.assertFileNotContains(modelPath,
                "\"quantity\": obj.Quantity,");
    }

    @Test
    public void verifyOrderEmbedsAllOfParent() throws IOException {
        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = createDefaultCodegenConfigurator(output)
                .setInputSpec("src/test/resources/3_0/go-server/petstore_with_test_endpoint.yaml");

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        java.nio.file.Path modelPath = Paths.get(output + "/go/model_order.go");
        TestUtils.assertFileExists(modelPath);
        TestUtils.assertFileContains(modelPath, "SpecialInfo");
        TestUtils.assertFileContains(modelPath, "\"requireTest\",");
        TestUtils.assertFileContains(modelPath, "json.Unmarshal(value, &decoded.PetId)");
        TestUtils.assertFileContains(modelPath, "json.Unmarshal(value, &decoded.RequireTest)");
    }

    @Test
    public void verifyOrderUnmarshalJSONHandlesInheritedAllOfRequiredFields() throws IOException {
        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = createDefaultCodegenConfigurator(output)
                .setInputSpec("src/test/resources/3_0/go-server/petstore_with_test_endpoint.yaml")
                .addAdditionalProperty("router", "chi");

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        java.nio.file.Path modelPath = Paths.get(output + "/go/model_order.go");
        TestUtils.assertFileExists(modelPath);
        TestUtils.assertFileContains(modelPath, "\"comment\",");
        TestUtils.assertFileContains(modelPath, "\"requireTest\",");
        TestUtils.assertFileContains(modelPath, "json.Unmarshal(value, &decoded.Comment)");
        TestUtils.assertFileContains(modelPath, "json.Unmarshal(value, &decoded.RequireTest)");
    }

    private static CodegenConfigurator createDefaultCodegenConfigurator(File output) {
        return new CodegenConfigurator()
                .setGeneratorName("go-server")
                .setGitUserId("my-user")
                .setGitRepoId("my-repo")
                .setPackageName("mypackage")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));
    }

    @Test
    public void verifyReadOnlyRequiredFieldsNotEnforced() throws IOException {
        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = createDefaultCodegenConfigurator(output)
                .setInputSpec("src/test/resources/3_0/go-server/readonly-required.yaml");

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        // AssertThingRequired must not reference the readOnly "id" field, so a
        // request body that legitimately omits it is accepted.
        // Primitive non-readOnly required fields are validated in UnmarshalJSON, not here.
        java.nio.file.Path modelPath = Paths.get(output + "/go/model_thing.go");
        TestUtils.assertFileExists(modelPath);
        TestUtils.assertFileContains(modelPath,
                "func AssertThingRequired(obj Thing) error {");
        TestUtils.assertFileContains(modelPath,
                "Primitive required fields are validated for JSON request bodies in UnmarshalJSON");
        // name is a primitive string, validated by UnmarshalJSON — excluded from AssertRequired
        TestUtils.assertFileNotContains(modelPath,
                "\"name\": obj.Name,");
        // readOnly "id" must be skipped
        TestUtils.assertFileNotContains(modelPath,
                "\"id\": obj.Id,");
        // readOnly nested model "meta" must not be recursed into, so a
        // request body that omits it (and thus omits Meta's own required
        // fields) is accepted.
        TestUtils.assertFileNotContains(modelPath,
                "if err := AssertMetaRequired(obj.Meta); err != nil {");
    }

}
