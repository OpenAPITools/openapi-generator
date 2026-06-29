/*
 * Copyright 2026 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

package org.openapitools.codegen.graphql;

import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import static org.openapitools.codegen.TestUtils.assertFileExists;

public class GraphQLNodeJSExpressServerCodegenTest {

    @Test
    public void resolverExamplesAreEscapedForJavaScriptStrings() throws IOException {
        File output = Files.createTempDirectory("graphql-nodejs-express-server-test_").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("graphql-nodejs-express-server")
                .setInputSpec("src/test/resources/3_0/graphql-nodejs-express-server/problematic-example.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        files.forEach(File::deleteOnExit);

        Path path = Paths.get(output + "/openapi3graphql-server/api/test_api_resolver.js");
        assertFileExists(path);
        TestUtils.assertFileContains(path, "\"filter\": \"line \\\"quoted\\\" \\\\ path\\nnext\"");
    }
}
