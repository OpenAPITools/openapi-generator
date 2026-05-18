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

package org.openapitools.codegen.php;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.parser.core.models.ParseOptions;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.languages.PhpNextgenClientCodegen;
import org.openapitools.codegen.testutils.ConfigAssert;
import org.testng.Assert;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

public class PhpNextgenClientCodegenTest {

    protected PhpNextgenClientCodegen codegen;

    @BeforeMethod
    public void before() {
        codegen = new PhpNextgenClientCodegen();
    }

    @Test
    public void testInitialConfigValues() throws Exception {
        final PhpNextgenClientCodegen codegen = new PhpNextgenClientCodegen();

        codegen.processOpts();

        Assert.assertEquals(codegen.isSupportStreaming(), false);
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final PhpNextgenClientCodegen codegen = new PhpNextgenClientCodegen();

        codegen.setSupportStreaming(true);

        codegen.processOpts();

        Assert.assertEquals(codegen.isSupportStreaming(), true);
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValuesWithFalseValue() throws Exception {
        final PhpNextgenClientCodegen codegen = new PhpNextgenClientCodegen();

        codegen.additionalProperties().put(PhpNextgenClientCodegen.SUPPORT_STREAMING, false);

        codegen.processOpts();
        ConfigAssert configAssert = new ConfigAssert(codegen.additionalProperties());

        configAssert.assertValue(PhpNextgenClientCodegen.SUPPORT_STREAMING, codegen::isSupportStreaming, Boolean.FALSE);
        Assert.assertEquals(codegen.isSupportStreaming(), false);
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValuesWithTrueValue() throws Exception {
        final PhpNextgenClientCodegen codegen = new PhpNextgenClientCodegen();

        codegen.additionalProperties().put(PhpNextgenClientCodegen.SUPPORT_STREAMING, true);

        codegen.processOpts();
        ConfigAssert configAssert = new ConfigAssert(codegen.additionalProperties());

        configAssert.assertValue(PhpNextgenClientCodegen.SUPPORT_STREAMING, codegen::isSupportStreaming, Boolean.TRUE);
        Assert.assertEquals(codegen.isSupportStreaming(), true);
    }

    @Test
    public void testEnumUnknownDefaultCaseDeserializationEnabled() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/bugs/issue_20593.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CodegenConstants.ENUM_UNKNOWN_DEFAULT_CASE, "true");

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        List<String> modelContent = Files
                .readAllLines(files.get("Pet.php").toPath())
                .stream()
                .map(String::trim)
                .collect(Collectors.toList());

        Assert.assertListContains(modelContent, a -> a.equals("$color = self::COLOR_UNKNOWN_DEFAULT_OPEN_API;"), "");
        Assert.assertListNotContains(modelContent, a -> a.equals("\"Invalid value '%s' for 'color', must be one of '%s'\","), "");
    }

    @Test
    public void testDiscriminatorUsesModelPackageNamespace() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/php-nextgen/petstore-with-fake-endpoints-models-for-testing.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());
        // Set invokerPackage="MyApp" and modelPackage="Entities" (relative suffix).
        // AbstractPhpCodegen.processOpts() will produce final modelPackage = "MyApp\Entities".
        // The old bug would have emitted '\MyApp\Model\' (invokerPackage + \Model\).
        codegen.additionalProperties().put(CodegenConstants.INVOKER_PACKAGE, "MyApp");
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "Entities");

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        List<String> objectSerializerContent = Files
                .readAllLines(files.get("ObjectSerializer.php").toPath())
                .stream()
                .map(String::trim)
                .collect(Collectors.toList());

        // The discriminator subclass lookup must use modelPackage (\MyApp\Entities\),
        // NOT invokerPackage + '\Model' (\MyApp\Model\).
        Assert.assertListContains(objectSerializerContent,
                a -> a.contains("'\\MyApp\\Entities\\\\'"),
                "ObjectSerializer discriminator subclass lookup must use modelPackage namespace");
        Assert.assertListNotContains(objectSerializerContent,
                a -> a.contains("'\\MyApp\\Model\\\\'"),
                "ObjectSerializer discriminator must NOT use invokerPackage\\Model namespace");
    }

    @Test
    public void testEnumUnknownDefaultCaseDeserializationDisabled() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/bugs/issue_20593.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        List<String> modelContent = Files
                .readAllLines(files.get("Pet.php").toPath())
                .stream()
                .map(String::trim)
                .collect(Collectors.toList());

        Assert.assertListNotContains(modelContent, a -> a.equals("$color = self::COLOR_UNKNOWN_DEFAULT_OPEN_API;"), "");
        Assert.assertListContains(modelContent, a -> a.equalsIgnoreCase("\"Invalid value '%s' for 'color', must be one of '%s'\","), "");
    }

    @Test
    public void testOneOfDiscriminatorEnumGeneration() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/bugs/issue_php_nextgen_oneOf_discriminator_enum.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        Assert.assertNotNull(files.get("PetAnimal.php"));

        List<String> combinedModelContent = Files
                .readAllLines(files.get("PetAnimal.php").toPath())
                .stream()
                .map(String::trim)
                .collect(Collectors.toList());
        int styleAllowableValuesStart = combinedModelContent.indexOf("public static function getStyleAllowableValues()");
        Assert.assertTrue(styleAllowableValuesStart >= 0,
                "Expected combined oneOf wrapper to declare getStyleAllowableValues");
        int styleAllowableValuesEnd = combinedModelContent.subList(styleAllowableValuesStart, combinedModelContent.size()).indexOf("];");
        Assert.assertTrue(styleAllowableValuesEnd >= 0,
                "Expected combined oneOf wrapper to close the style allowable-values array");
        List<String> styleAllowableValuesContent = combinedModelContent.subList(
                styleAllowableValuesStart,
                styleAllowableValuesStart + styleAllowableValuesEnd + 1);
        Assert.assertListContains(combinedModelContent,
                a -> a.equals("public const STYLE_PAGE = 'page';"),
                "Expected combined oneOf wrapper to declare STYLE_PAGE constant");
        Assert.assertListContains(combinedModelContent,
                a -> a.equals("public const STYLE_TEXT = 'text';"),
                "Expected combined oneOf wrapper to declare STYLE_TEXT constant");
        Assert.assertListContains(combinedModelContent,
                a -> a.equals("public const STYLE_VIEWPORT = 'viewport';"),
                "Expected combined oneOf wrapper to declare STYLE_VIEWPORT constant");
        Assert.assertListContains(styleAllowableValuesContent,
                a -> a.equals("self::STYLE_PAGE,"),
                "Expected combined oneOf wrapper to use style enum values");
        Assert.assertListContains(styleAllowableValuesContent,
                a -> a.equals("self::STYLE_TEXT,"),
                "Expected combined oneOf wrapper to use style enum values");
        Assert.assertListContains(styleAllowableValuesContent,
                a -> a.equals("self::STYLE_VIEWPORT,"),
                "Expected combined oneOf wrapper to use style enum values");
        Assert.assertListNotContains(styleAllowableValuesContent,
                a -> a.equals("self::TYPE_PET_DOG,"),
                "Combined oneOf wrapper should not use type enum values for style");
        Assert.assertListNotContains(styleAllowableValuesContent,
                a -> a.equals("self::TYPE_PET_CAT,"),
                "Combined oneOf wrapper should not use type enum values for style");
    }

    @Test
    public void testDiscriminatorConstantsPreservedForNonEnumDiscriminatorModels() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/php-nextgen/petstore-with-fake-endpoints-models-for-testing.yaml", null, new ParseOptions())
                .getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        Assert.assertNotNull(files.get("Animal.php"));
        Assert.assertNotNull(files.get("DiscriminatorBase.php"));

        List<String> animalModelContent = Files
                .readAllLines(files.get("Animal.php").toPath())
                .stream()
                .map(String::trim)
                .collect(Collectors.toList());
        Assert.assertListContains(animalModelContent,
                a -> a.equals("public const CLASS_NAME_DOG = 'DOG';"),
                "Expected Animal to preserve discriminator constant CLASS_NAME_DOG");
        Assert.assertListContains(animalModelContent,
                a -> a.equals("public const CLASS_NAME_CAT = 'CAT';"),
                "Expected Animal to preserve discriminator constant CLASS_NAME_CAT");

        List<String> discriminatorBaseModelContent = Files
                .readAllLines(files.get("DiscriminatorBase.php").toPath())
                .stream()
                .map(String::trim)
                .collect(Collectors.toList());
        Assert.assertListContains(discriminatorBaseModelContent,
                a -> a.equals("public const TYPE_DISCRIMINATOR_CHILD = 'DiscriminatorChild';"),
                "Expected DiscriminatorBase to preserve inferred discriminator constants");
    }

    @Test
    public void testDifferentResponseSchemasWithEmpty() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/bugs/issue_22817.yaml", null, new ParseOptions())
                .getOpenAPI();


        codegen.setOutputDir(output.getAbsolutePath());
        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        List<String> modelContent = Files
                .readAllLines(files.get("DefaultApi.php").toPath())
                .stream()
                .map(String::trim)
                .collect(Collectors.toList());

        Assert.assertListContains(modelContent, a -> a.equals("): int|string|null"), "Expected to find nullable return type declaration.");
        Assert.assertListNotContains(modelContent, a -> a.equals("): ?int|string"), "Expected to not find invalid union type with '?'.");
    }
}
