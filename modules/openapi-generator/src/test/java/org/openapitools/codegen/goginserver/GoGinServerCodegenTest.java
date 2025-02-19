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

package org.openapitools.codegen.goginserver;

import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;


public class GoGinServerCodegenTest {

    @Test
    public void verifyGoMod() throws IOException {
        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = createDefaultCodegenConfigurator(output)
                .setInputSpec("src/test/resources/3_0/petstore.yaml");

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        TestUtils.assertFileExists(Paths.get(output + "/go.mod"));
        TestUtils.assertFileContains(Paths.get(output + "/go.mod"),
                "module github.com/my-user/my-repo");
        TestUtils.assertFileContains(Paths.get(output + "/go.mod"),
                "require github.com/gin-gonic/gin v1.9.1");
    }

    @Test
    public void webhooks() throws IOException {
        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = createDefaultCodegenConfigurator(output)
                .setInputSpec("src/test/resources/3_1/webhooks.yaml");

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        TestUtils.assertFileContains(Paths.get(output + "/go/routers.go"),
                "NewPetPost");
        TestUtils.assertFileContains(Paths.get(output + "/go/api_default.go"),
                " c.JSON(200, gin.H{\"status\": \"OK\"})");
    }

    @Test
    public void verifyInterfaceOnly() throws IOException {
        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = createDefaultCodegenConfigurator(output)
                .setInputSpec("src/test/resources/3_0/petstore.yaml")
                .addAdditionalProperty("interfaceOnly", true);

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        TestUtils.assertFileContains(Paths.get(output + "/go/api_pet.go"),
                "type PetAPI interface");
    }

    private static CodegenConfigurator createDefaultCodegenConfigurator(File output) {
        return new CodegenConfigurator()
                .setGeneratorName("go-gin-server")
                .setGitUserId("my-user")
                .setGitRepoId("my-repo")
                .setPackageName("mypackage")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));
    }

}
