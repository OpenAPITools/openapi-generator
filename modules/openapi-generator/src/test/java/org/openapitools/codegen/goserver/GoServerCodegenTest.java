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
        Assert.assertEquals(Files.readAllLines(Paths.get(output + "/go/api_dev.go")).get(57), "\t\t\"GetById\": Route{");

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
