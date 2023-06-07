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

package org.openapitools.codegen.java.apachehttpclient;

import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.JavaClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.openapitools.codegen.TestUtils.validateJavaSourceFiles;

public class ApacheHttpClientCodegenTest {

    @Test
    public void testApacheHttpClientExplodedQueryParamObject() throws IOException {
        Map<String, Object> properties = new HashMap<>();
        properties.put(CodegenConstants.API_PACKAGE, "xyz.abcdef.api");

        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.APACHE)
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_0/issue4808.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        Assert.assertEquals(files.size(), 41);
        validateJavaSourceFiles(files);

        TestUtils.assertFileContains(Paths.get(output + "/src/main/java/xyz/abcdef/api/DefaultApi.java"),
                "localVarQueryParams.addAll(apiClient.parameterToPair(\"since\", queryObject.getSince()));",
                "localVarQueryParams.addAll(apiClient.parameterToPair(\"sinceBuild\", queryObject.getSinceBuild()));",
                "localVarQueryParams.addAll(apiClient.parameterToPair(\"maxBuilds\", queryObject.getMaxBuilds()));",
                "localVarQueryParams.addAll(apiClient.parameterToPair(\"maxWaitSecs\", queryObject.getMaxWaitSecs()));"
        );
    }

    @Test
    public void testApacheHttpClientExplodedQueryParamWithArrayProperty() throws IOException {
        Map<String, Object> properties = new HashMap<>();
        properties.put(CodegenConstants.API_PACKAGE, "xyz.abcdef.api");

        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
            .setGeneratorName("java")
            .setLibrary(JavaClientCodegen.APACHE)
            .setAdditionalProperties(properties)
            .setInputSpec("src/test/resources/3_0/exploded-query-param-array.yaml")
            .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        generator.opts(clientOptInput).generate();

        TestUtils.assertFileContains(Paths.get(output + "/src/main/java/xyz/abcdef/api/DefaultApi.java"),
            "localVarQueryParams.addAll(apiClient.parameterToPairs(\"multi\", \"values\", queryObject.getValues()))"
        );
    }
}
