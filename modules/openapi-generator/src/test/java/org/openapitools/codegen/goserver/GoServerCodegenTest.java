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

    private static CodegenConfigurator createDefaultCodegenConfigurator(File output) {
        return new CodegenConfigurator()
                .setGeneratorName("go-server")
                .setGitUserId("my-user")
                .setGitRepoId("my-repo")
                .setPackageName("mypackage")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));
    }

}
