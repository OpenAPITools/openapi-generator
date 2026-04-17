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

import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.AbstractPhpCodegen;
import org.openapitools.codegen.languages.PhpSymfonyServerCodegen;
import org.testng.Assert;
import org.testng.SkipException;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;


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

    /**
     * OpenAPI 3.1 + dotted schema keys + {@code components.parameters} {@code $ref}: enum-ref query
     * parameters must use a short model class name in {@code DefaultApiInterface} (aligned with
     * {@code use} imports), not a bogus flattened namespace token.
     * <p>
     * Also verifies PHPDoc {@code @param} uses the same short name (not {@code \\FQCN}) and, when
     * {@code php} is on {@code PATH}, that {@code php -l} accepts the generated file (valid syntax).
     */
    @Test
    public void testPetstoreDottedEnumRefQueryParameterUsesShortClassInApiInterface() throws Exception {
        Map<String, Object> properties = new HashMap<>();
        properties.put("invokerPackage", "Org\\OpenAPITools\\Petstore");

        File output = Files.createTempDirectory("test").toFile();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("php-symfony")
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_1/php-symfony/petstore-dotted-enum-ref-query-param-component.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        File apiInterfaceFile = files.stream()
                .filter(f -> "DefaultApiInterface.php".equals(f.getName()) && f.getPath().contains("Api" + File.separator))
                .findFirst()
                .orElseThrow(() -> new AssertionError("DefaultApiInterface.php not generated"));

        String apiContent = Files.readString(apiInterfaceFile.toPath(), StandardCharsets.UTF_8);
        Assert.assertFalse(
                apiContent.contains("OrgOpenAPIToolsPetstoreModel"),
                "Must not emit flattened invoker+model token in interface");
        Assert.assertTrue(
                apiContent.contains("use Org\\OpenAPITools\\Petstore\\Model\\PetModelPetStatus;"),
                "Expected enum model import");
        // This spec sets default: available on the enum $ref; the handler must be non-nullable (no leading "?" /
        // "|null") because the controller always supplies a value after applying the OpenAPI default.
        Assert.assertTrue(
                Pattern.compile("public function listPets\\(\\s*PetModelPetStatus\\s+\\$status,").matcher(apiContent).find(),
                "Expected defaulted enum-ref query param to use short non-nullable class in type hint");
        Assert.assertFalse(
                Pattern.compile("public function listPets\\(\\s*\\?PetModelPetStatus\\s+\\$status").matcher(apiContent).find(),
                "Defaulted enum-ref query param must not use nullable type hint (?PetModelPetStatus)");
        Assert.assertTrue(
                Pattern.compile("@param\\s+PetModelPetStatus\\s+\\$status\\b").matcher(apiContent).find(),
                "PHPDoc @param should use short PetModelPetStatus without |null when OpenAPI default is set");
        Assert.assertFalse(
                Pattern.compile("@param\\s+PetModelPetStatus\\|null\\s+\\$status\\b").matcher(apiContent).find(),
                "PHPDoc must not document |null for enum ref when OpenAPI default is set");
        Assert.assertFalse(
                apiContent.contains("?\\Org\\OpenAPITools\\Petstore\\Model\\PetModelPetStatus $status"),
                "Signature must not use leading-backslash FQCN when a matching use import exists");
        Assert.assertFalse(
                apiContent.contains("@param  \\Org\\"),
                "PHPDoc @param must not use leading-backslash FQCN for enum ref");

        assertGeneratedPhpSyntaxValid(apiInterfaceFile);

        output.deleteOnExit();
    }

    /**
     * Guards {@code php-symfony} {@code JmsSerializer.mustache}: invalid query values for a generated PHP
     * {@code BackedEnum} are deserialized in {@code JmsSerializer::deserializeString()}. The generated
     * {@code DefaultController} wraps {@code deserialize(...)} with {@code catch (SerializerRuntimeException)},
     * an alias of {@code JMS\Serializer\Exception\RuntimeException}. Only the unknown-enum branch must throw that
     * type; other string-deserialization errors may keep using PHP's global {@code RuntimeException}.
     * <p>
     * This test asserts the generated {@code JmsSerializer.php} keeps {@code use RuntimeException;} and adds
     * {@code use JMS\Serializer\Exception\RuntimeException as SerializerRuntimeException;}, throws
     * {@code SerializerRuntimeException} for {@code tryFrom} failure, and that {@code DefaultController.php} still
     * catches {@code SerializerRuntimeException}.
     * <p>
     * Spec: {@code src/test/resources/3_1/php-symfony/jms-enum-query-invalid-deserialization.yaml}. Background:
     * {@code fix_jms_enum_ex.md}.
     */
    @Test
    public void testJmsSerializerUsesJmsRuntimeExceptionForBackedEnumStringDeserializationErrors() throws Exception {
        Map<String, Object> properties = new HashMap<>();
        properties.put("invokerPackage", "Org\\OpenAPITools\\PetstoreEnum");

        File output = Files.createTempDirectory("test").toFile();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("php-symfony")
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_1/php-symfony/jms-enum-query-invalid-deserialization.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        File jmsSerializer = files.stream()
                .filter(f -> "JmsSerializer.php".equals(f.getName()) && f.getPath().contains("Service" + File.separator))
                .findFirst()
                .orElseThrow(() -> new AssertionError("JmsSerializer.php not generated"));

        String jms = Files.readString(jmsSerializer.toPath(), StandardCharsets.UTF_8);

        Assert.assertTrue(
                jms.contains("Unknown %s value in %s enum"),
                "Expected BackedEnum tryFrom failure message in generated JmsSerializer");
        Assert.assertTrue(
                Pattern.compile("^use RuntimeException;\\s*$", Pattern.MULTILINE).matcher(jms).find(),
                "JmsSerializer should keep use RuntimeException for generic unsupported-type errors");
        Assert.assertTrue(
                jms.contains("use JMS\\Serializer\\Exception\\RuntimeException as SerializerRuntimeException;"),
                "JmsSerializer must alias JMS RuntimeException as SerializerRuntimeException (same as DefaultController)");
        Assert.assertTrue(
                jms.contains("throw new SerializerRuntimeException(sprintf(\"Unknown %s value in %s enum\", $data, $type));"),
                "Invalid BackedEnum tryFrom must throw SerializerRuntimeException so DefaultController catch applies");

        File defaultController = files.stream()
                .filter(f -> "DefaultController.php".equals(f.getName()) && f.getPath().contains("Controller" + File.separator))
                .findFirst()
                .orElseThrow(() -> new AssertionError("DefaultController.php not generated"));
        String controller = Files.readString(defaultController.toPath(), StandardCharsets.UTF_8);
        Assert.assertTrue(
                controller.contains("use JMS\\Serializer\\Exception\\RuntimeException as SerializerRuntimeException;"),
                "Expected DefaultController to catch SerializerRuntimeException alias");
        Assert.assertTrue(
                controller.contains("catch (SerializerRuntimeException $exception)"),
                "Expected deserialize() to catch SerializerRuntimeException");

        assertGeneratedPhpSyntaxValid(jmsSerializer);
        assertGeneratedPhpSyntaxValid(defaultController);

        output.deleteOnExit();
    }

    /**
     * Optional {@code in: query} parameter: {@code required: false}, schema is an enum {@code $ref} with a valid
     * {@code default} (see OpenAPI 3.x). Omitting the query key must be equivalent to sending that default; the
     * generated controller must not reject the request in validation solely because the value was absent.
     * <p>
     * Spec: {@code src/test/resources/3_1/php-symfony/optional-enum-query-ref-default.yaml}. Product doc:
     * {@code php-symfony.md} section &quot;可选 query：带默认值的非必填枚举 {@code $ref} 缺省却仍被拒绝&quot;.
     * <p>
     * Expected generated behavior (any one is acceptable):
     * <ul>
     *   <li>Pass the OpenAPI default into {@code Request::query->get} for {@code tone}, and/or</li>
     *   <li>Apply the Elvis default line ({@code $tone = $tone?:...}) after the read (see {@code api_controller.mustache}), and/or</li>
     *   <li>Wrap enum {@code Assert\\Type} in {@code Assert\\Optional} for non-required enum refs (see {@code api_input_validation.mustache}).</li>
     * </ul>
     * Also asserts the integer optional {@code limit} parameter still receives {@code get('limit', 10)} as a control.
     * <p>
     * <b>Note:</b> This test fails on the generator until optional enum-ref query parameters expose
     * {@link org.openapitools.codegen.CodegenParameter#defaultValue} (or equivalent) so templates apply the OpenAPI
     * default and/or skip strict {@code Assert\\Type} on {@code null}. It is intended to lock the fix described in the
     * php-symfony troubleshooting doc.
     */
    @Test
    public void testOptionalEnumRefQueryParameterWithDefaultAppliesOpenApiSemantics() throws Exception {
        Map<String, Object> properties = new HashMap<>();
        properties.put("invokerPackage", "Org\\OpenAPITools\\FeedHints");
        properties.put(AbstractPhpCodegen.SRC_BASE_PATH, "src");

        File output = Files.createTempDirectory("test").toFile();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("php-symfony")
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_1/php-symfony/optional-enum-query-ref-default.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        File apiInterfaceFile = files.stream()
                .filter(f -> "DefaultApiInterface.php".equals(f.getName()) && f.getPath().contains("Api" + File.separator))
                .findFirst()
                .orElseThrow(() -> new AssertionError("DefaultApiInterface.php not generated"));
        String apiContent = Files.readString(apiInterfaceFile.toPath(), StandardCharsets.UTF_8);
        Assert.assertTrue(
                Pattern.compile("public function listFeedHints\\(\\s*PetAnnouncementTone\\s+\\$tone,").matcher(apiContent).find(),
                "Expected defaulted enum-ref query param to use short non-nullable class in API interface type hint");
        Assert.assertFalse(
                Pattern.compile("public function listFeedHints\\(\\s*\\?PetAnnouncementTone\\s+\\$tone").matcher(apiContent).find(),
                "Defaulted enum-ref query param must not use nullable type hint (?PetAnnouncementTone)");
        Assert.assertTrue(
                Pattern.compile("@param\\s+PetAnnouncementTone\\s+\\$tone\\b").matcher(apiContent).find(),
                "PHPDoc @param should use PetAnnouncementTone without |null when OpenAPI default is set");
        Assert.assertFalse(
                Pattern.compile("@param\\s+PetAnnouncementTone\\|null\\s+\\$tone\\b").matcher(apiContent).find(),
                "PHPDoc must not document |null for enum ref when OpenAPI default is set");
        assertGeneratedPhpSyntaxValid(apiInterfaceFile);

        File controllerFile = files.stream()
                .filter(f -> "DefaultController.php".equals(f.getName()) && f.getPath().contains("Controller" + File.separator))
                .findFirst()
                .orElseThrow(() -> new AssertionError("DefaultController.php not generated"));

        String controller = Files.readString(controllerFile.toPath(), StandardCharsets.UTF_8);

        Assert.assertTrue(
                controller.contains("$request->query->get('limit', 10)"),
                "Integer optional query with default should pass default as second argument to query->get (control case)");

        boolean defaultInGet = Pattern.compile("\\$request->query->get\\('tone',\\s*").matcher(controller).find();
        boolean elvisDefault = Pattern.compile("\\$tone\\s*=\\s*\\$tone\\?:").matcher(controller).find();
        boolean optionalEnumTypeAssert =
                controller.contains("new Assert\\Optional(")
                        && controller.contains("PetAnnouncementTone");

        Assert.assertTrue(
                defaultInGet || elvisDefault || optionalEnumTypeAssert,
                "Omitted optional enum-ref query with OpenAPI default must apply default (get/Elvis) and/or use "
                        + "Assert\\Optional around enum Type so null is valid before default is applied; "
                        + "see optional-enum-query-ref-default.yaml and php-symfony troubleshooting doc");

        assertGeneratedPhpSyntaxValid(controllerFile);

        output.deleteOnExit();
    }

    /**
     * Runs {@code php -l} on the file. Skips if {@code php} is not available (optional toolchain).
     */
    private static void assertGeneratedPhpSyntaxValid(File phpFile) throws Exception {
        ProcessBuilder pb = new ProcessBuilder("php", "-l", phpFile.getAbsolutePath());
        pb.redirectErrorStream(true);
        final Process p;
        try {
            p = pb.start();
        } catch (IOException e) {
            throw new SkipException("php not available on PATH, skipping syntax check: " + e.getMessage());
        }
        Assert.assertTrue(p.waitFor(30, TimeUnit.SECONDS), "php -l timed out");
        String out = new String(p.getInputStream().readAllBytes(), StandardCharsets.UTF_8).trim();
        Assert.assertEquals(p.exitValue(), 0, "php -l must accept generated interface: " + out);
    }
}
