/*
 * Copyright 2019 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.elm;

import static com.google.common.collect.Lists.newArrayList;
import static java.util.Collections.emptyList;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotNull;
import static org.testng.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.ClientOpts;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.MockDefaultGenerator;
import org.openapitools.codegen.languages.ElmClientCodegen;
import org.testng.annotations.Test;

import com.google.common.collect.ImmutableMap;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.parser.core.models.ParseOptions;

public class ElmClientCodegenTest {

    @Test
    public void testPostProcessRootEndpoint() {
        // given
        final ElmClientCodegen codegen = new ElmClientCodegen();

        CodegenOperation rootOp = new CodegenOperation() {{ path = "/"; }};
        List<CodegenOperation> ops = newArrayList(rootOp);

        Map<String, Object> operations = new HashMap<>();
        operations.put("operations", ImmutableMap
                .<String, Object>builder()
                .put("operation", ops)
                .build());

        // when
        Map<String, Object> result = codegen.postProcessOperationsWithModels(operations, emptyList());

        // then
        assertEquals(result.size(), 2);
        assertTrue(result.containsKey("operations"));
        assertTrue(result.containsKey("elmImports"));

        assertEquals(rootOp.path, "\"\"");
    }

    @Test
    public void testGenerateRootEndpoint() throws IOException {
        // given
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        final ElmClientCodegen codegen = new ElmClientCodegen();
        codegen.setOutputDir(output.getAbsolutePath());

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/rootOperation.yaml", null, new ParseOptions())
                .getOpenAPI();

        ClientOpts opts = new ClientOpts();

        ClientOptInput input = new ClientOptInput();
        input.setOpenAPI(openAPI);
        input.setConfig(codegen);
        input.setOpts(opts);

        // when
        MockDefaultGenerator generator = new MockDefaultGenerator();
        generator.opts(input).generate();

        // then
        assertFileContains(generator, outputPath + "/src/Request/Default.elm", "rootGet", "[\"\"]");
    }

    private static void assertFileContains(MockDefaultGenerator generator, String file, String... expected) {
        String content = generator.getFiles().get(file);
        assertNotNull(content, "The file \"" + file + "\" was not generated");
        for (String line : expected) {
            assertTrue(content.contains(line), "The file \"" + file + "\" does not contain \"" + line + "\"");
        }
    }
}
