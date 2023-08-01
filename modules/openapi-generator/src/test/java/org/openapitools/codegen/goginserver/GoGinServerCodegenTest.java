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


public class GoGinServerCodegenTest {

    @Test
    public void verifyGoMod() throws IOException {
        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("go-gin-server")
                .setGitUserId("my-user")
                .setGitRepoId("my-repo")
                .setPackageName("my-package")
                .setInputSpec("src/test/resources/3_0/petstore.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        TestUtils.assertFileExists(Paths.get(output + "/go.mod"));
        TestUtils.assertFileContains(Paths.get(output + "/go.mod"),
                "module github.com/my-user/my-repo");
        TestUtils.assertFileContains(Paths.get(output + "/go.mod"),
                "require github.com/gin-gonic/gin v1.9.0");
    }

}
