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

package org.openapitools.codegen.php;

import java.io.File;
import java.nio.file.Files;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.AbstractPhpCodegen;
import org.openapitools.codegen.languages.PhpSymfonyServerCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;


public class PhpSymfonyServerCodegenTest {

    @Test
    public void testInitialConfigValues() throws Exception {
        final PhpSymfonyServerCodegen codegen = new PhpSymfonyServerCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final PhpSymfonyServerCodegen codegen = new PhpSymfonyServerCodegen();
        codegen.setHideGenerationTimestamp(false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final PhpSymfonyServerCodegen codegen = new PhpSymfonyServerCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
    }

    @Test
    public void testGeneratePing() throws Exception {
        Map<String, Object> properties = new HashMap<>();

        File output = Files.createTempDirectory("test").toFile();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("php-symfony")
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_0/ping.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        Assert.assertEquals(files.size(), 33);
        TestUtils.ensureContainsFile(files, output, ".coveralls.yml");
        TestUtils.ensureContainsFile(files, output, ".gitignore");
        TestUtils.ensureContainsFile(files, output, ".openapi-generator-ignore");
        TestUtils.ensureContainsFile(files, output, ".openapi-generator/FILES");
        TestUtils.ensureContainsFile(files, output, ".openapi-generator/VERSION");
        TestUtils.ensureContainsFile(files, output, ".php_cs.dist");
        TestUtils.ensureContainsFile(files, output, ".travis.yml");
        TestUtils.ensureContainsFile(files, output, "autoload.php");
        TestUtils.ensureContainsFile(files, output, "composer.json");
        TestUtils.ensureContainsFile(files, output, "git_push.sh");
        TestUtils.ensureContainsFile(files, output, "phpunit.xml.dist");
        TestUtils.ensureContainsFile(files, output, "README.md");
        TestUtils.ensureContainsFile(files, output, "Api/ApiServer.php");
        TestUtils.ensureContainsFile(files, output, "Api/DefaultApiInterface.php");
        TestUtils.ensureContainsFile(files, output, "Controller/Controller.php");
        TestUtils.ensureContainsFile(files, output, "Controller/DefaultController.php");
        TestUtils.ensureContainsFile(files, output, "DependencyInjection/Compiler/OpenAPIServerApiPass.php");
        TestUtils.ensureContainsFile(files, output, "DependencyInjection/OpenAPIServerExtension.php");
        TestUtils.ensureContainsFile(files, output, "docs/Api/DefaultApiInterface.md");
        TestUtils.ensureContainsFile(files, output, "OpenAPIServerBundle.php");
        TestUtils.ensureContainsFile(files, output, "Resources/config/routing.yaml");
        TestUtils.ensureContainsFile(files, output, "Resources/config/services.yaml");
        TestUtils.ensureContainsFile(files, output, "Service/JmsSerializer.php");
        TestUtils.ensureContainsFile(files, output, "Service/SerializerInterface.php");
        TestUtils.ensureContainsFile(files, output, "Service/StrictJsonDeserializationVisitor.php");
        TestUtils.ensureContainsFile(files, output, "Service/StrictJsonDeserializationVisitorFactory.php");
        TestUtils.ensureContainsFile(files, output, "Service/SymfonyValidator.php");
        TestUtils.ensureContainsFile(files, output, "Service/TypeMismatchException.php");
        TestUtils.ensureContainsFile(files, output, "Service/ValidatorInterface.php");
        TestUtils.ensureContainsFile(files, output, "Tests/Api/DefaultApiInterfaceTest.php");
        TestUtils.ensureContainsFile(files, output, "Tests/AppKernel.php");
        TestUtils.ensureContainsFile(files, output, "Tests/Controller/ControllerTest.php");
        TestUtils.ensureContainsFile(files, output, "Tests/test_config.yaml");

        output.deleteOnExit();
    }

    @Test
    public void testGeneratePingWithDifferentSourceDirectory() throws Exception {
        Map<String, Object> properties = new HashMap<>();
        properties.put(AbstractPhpCodegen.SRC_BASE_PATH, "src");

        File output = Files.createTempDirectory("test").toFile();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("php-symfony")
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_0/ping.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        Assert.assertEquals(files.size(), 33);
        TestUtils.ensureContainsFile(files, output, ".coveralls.yml");
        TestUtils.ensureContainsFile(files, output, ".gitignore");
        TestUtils.ensureContainsFile(files, output, ".openapi-generator-ignore");
        TestUtils.ensureContainsFile(files, output, ".openapi-generator/FILES");
        TestUtils.ensureContainsFile(files, output, ".openapi-generator/VERSION");
        TestUtils.ensureContainsFile(files, output, ".php_cs.dist");
        TestUtils.ensureContainsFile(files, output, ".travis.yml");
        TestUtils.ensureContainsFile(files, output, "autoload.php");
        TestUtils.ensureContainsFile(files, output, "composer.json");
        TestUtils.ensureContainsFile(files, output, "git_push.sh");
        TestUtils.ensureContainsFile(files, output, "phpunit.xml.dist");
        TestUtils.ensureContainsFile(files, output, "README.md");
        TestUtils.ensureContainsFile(files, output, "docs/Api/DefaultApiInterface.md");
        TestUtils.ensureContainsFile(files, output, "src/Api/ApiServer.php");
        TestUtils.ensureContainsFile(files, output, "src/Api/DefaultApiInterface.php");
        TestUtils.ensureContainsFile(files, output, "src/Controller/Controller.php");
        TestUtils.ensureContainsFile(files, output, "src/Controller/DefaultController.php");
        TestUtils.ensureContainsFile(files, output, "src/DependencyInjection/Compiler/OpenAPIServerApiPass.php");
        TestUtils.ensureContainsFile(files, output, "src/DependencyInjection/OpenAPIServerExtension.php");
        TestUtils.ensureContainsFile(files, output, "src/OpenAPIServerBundle.php");
        TestUtils.ensureContainsFile(files, output, "src/Resources/config/routing.yaml");
        TestUtils.ensureContainsFile(files, output, "src/Resources/config/services.yaml");
        TestUtils.ensureContainsFile(files, output, "src/Service/JmsSerializer.php");
        TestUtils.ensureContainsFile(files, output, "src/Service/SerializerInterface.php");
        TestUtils.ensureContainsFile(files, output, "src/Service/StrictJsonDeserializationVisitor.php");
        TestUtils.ensureContainsFile(files, output, "src/Service/StrictJsonDeserializationVisitorFactory.php");
        TestUtils.ensureContainsFile(files, output, "src/Service/SymfonyValidator.php");
        TestUtils.ensureContainsFile(files, output, "src/Service/TypeMismatchException.php");
        TestUtils.ensureContainsFile(files, output, "src/Service/ValidatorInterface.php");
        TestUtils.ensureContainsFile(files, output, "src/Tests/Api/DefaultApiInterfaceTest.php");
        TestUtils.ensureContainsFile(files, output, "src/Tests/AppKernel.php");
        TestUtils.ensureContainsFile(files, output, "src/Tests/Controller/ControllerTest.php");
        TestUtils.ensureContainsFile(files, output, "src/Tests/test_config.yaml");

        output.deleteOnExit();
    }
}
