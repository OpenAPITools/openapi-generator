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
import io.swagger.v3.oas.models.media.DateTimeSchema;
import io.swagger.v3.oas.models.media.ObjectSchema;
import io.swagger.v3.oas.models.media.StringSchema;
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

    @Test(enabled = false)
    public void testOneOfDiscriminatorEnumGeneration() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/bugs/issue_23813.yaml", null, new ParseOptions()).getOpenAPI();

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

        // A body-less *success* response (204) means the method can return null, so the union
        // gains a `|null` member — never the `?` shorthand, which is illegal on a union.
        Assert.assertListContains(modelContent, a -> a.equals("): int|string|null"),
                "an empty success (204) response makes the union return type nullable");
        Assert.assertListNotContains(modelContent, a -> a.equals("): ?int|string"),
                "a union return type must never use the invalid `?` shorthand");

        // A body-less *error* response (500) throws an ApiException rather than returning null,
        // so it must NOT make the return type nullable.
        Assert.assertListContains(modelContent, a -> a.equals("): float"),
                "an empty error (500) response does not make the return type nullable");
        Assert.assertListNotContains(modelContent, a -> a.equals("): ?float"),
                "an empty error response must not add a nullable member to the return type");
    }

    @Test
    public void testOneOfPolymorphism() throws IOException {
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

        // The marker interface used to recognize oneOf models is generated.
        Assert.assertTrue(files.containsKey("OneOfInterface.php"), "Expected OneOfInterface.php to be generated.");

        // A oneOf without a discriminator becomes a dispatcher exposing its member types.
        List<String> fruit = Files.readAllLines(files.get("Fruit.php").toPath())
                .stream().map(String::trim).collect(Collectors.toList());
        Assert.assertListContains(fruit, a -> a.equals("class Fruit implements OneOfInterface"),
                "Fruit must implement OneOfInterface");
        Assert.assertListContains(fruit, a -> a.equals("public const DISCRIMINATOR = null;"),
                "Fruit has no discriminator");
        Assert.assertListContains(fruit, a -> a.equals("'\\OpenAPI\\Client\\Model\\Apple',"),
                "Fruit lists Apple as a member type");
        Assert.assertListContains(fruit, a -> a.equals("'\\OpenAPI\\Client\\Model\\Banana'"),
                "Fruit lists Banana as a member type");

        // A oneOf with a discriminator exposes its property name and value mapping.
        List<String> mammal = Files.readAllLines(files.get("Mammal.php").toPath())
                .stream().map(String::trim).collect(Collectors.toList());
        Assert.assertListContains(mammal, a -> a.equals("public const DISCRIMINATOR = 'className';"),
                "Mammal exposes its discriminator property by its wire (base) name");
        Assert.assertListContains(mammal, a -> a.equals("'whale' => '\\OpenAPI\\Client\\Model\\Whale',"),
                "Mammal maps the whale discriminator value");
        Assert.assertListContains(mammal, a -> a.equals("'zebra' => '\\OpenAPI\\Client\\Model\\Zebra'"),
                "Mammal maps the zebra discriminator value");

        // References to a oneOf are type-hinted as the PHP union of its members.
        List<String> fakeApi = Files.readAllLines(files.get("FakeApi.php").toPath())
                .stream().map(String::trim).collect(Collectors.toList());
        Assert.assertListContains(fakeApi,
                a -> a.contains("\\OpenAPI\\Client\\Model\\Whale|\\OpenAPI\\Client\\Model\\Zebra"),
                "Mammal request body is hinted as a union");
        Assert.assertListContains(fakeApi,
                a -> a.contains("\\OpenAPI\\Client\\Model\\Apple|\\OpenAPI\\Client\\Model\\Banana"),
                "Fruit response is hinted as a union");

        // The phpdoc must describe the concrete member union too, not the oneOf alias: the members
        // do not inherit from the alias, so `@param Mammal` / `@return Fruit` would be a lie.
        Assert.assertListContains(fakeApi,
                a -> a.startsWith("* @param") && a.contains("\\OpenAPI\\Client\\Model\\Whale|\\OpenAPI\\Client\\Model\\Zebra"),
                "Mammal request body @param phpdoc expands to the member union");
        Assert.assertListContains(fakeApi,
                a -> a.startsWith("* @return") && a.contains("\\OpenAPI\\Client\\Model\\Apple|\\OpenAPI\\Client\\Model\\Banana"),
                "Fruit response @return phpdoc expands to the member union");
        // The WithHttpInfo tuple phpdoc is a real array-shape type and expands the oneOf alias too.
        Assert.assertListContains(fakeApi,
                a -> a.startsWith("* @return array{0: ") && a.contains("\\OpenAPI\\Client\\Model\\Apple|\\OpenAPI\\Client\\Model\\Banana")
                        && a.contains("1: int, 2: array<string, string[]>}"),
                "WithHttpInfo @return is an array{...} shape that expands the Fruit oneOf to its member union");

        // The bare alias must not leak into any @param or @return phpdoc (including the
        // WithHttpInfo array-shape tuple line).
        Assert.assertListNotContains(fakeApi,
                a -> (a.startsWith("* @param") || a.startsWith("* @return"))
                        && (a.contains("\\OpenAPI\\Client\\Model\\Mammal") || a.contains("\\OpenAPI\\Client\\Model\\Fruit")),
                "the oneOf alias must not leak into the @param/@return phpdoc");

        // Repeated response types collapse: the WithHttpInfo @return must not list a type twice.
        Assert.assertListNotContains(fakeApi,
                a -> a.contains("\\OpenAPI\\Client\\Model\\ErrorResponse|\\OpenAPI\\Client\\Model\\ErrorResponse"),
                "duplicate response types must be collapsed in the WithHttpInfo @return");

        // The API doc example must instantiate a concrete oneOf member, not the abstract
        // dispatcher: `new Mammal()` is not usable, so the first member `new Whale()` is used.
        List<String> fakeApiDoc = Files.readAllLines(files.get("FakeApi.md").toPath())
                .stream().map(String::trim).collect(Collectors.toList());
        Assert.assertListContains(fakeApiDoc,
                a -> a.contains("$mammal = new \\OpenAPI\\Client\\Model\\Whale()"),
                "the doc example instantiates the first oneOf member (Whale)");
        Assert.assertListNotContains(fakeApiDoc,
                a -> a.contains("new \\OpenAPI\\Client\\Model\\Mammal()"),
                "the doc example must not instantiate the abstract Mammal oneOf dispatcher");

        // The oneOf model doc documents its concrete member types, not the umbrella's flattened
        // properties, and names the wire discriminator.
        List<String> mammalDoc = Files.readAllLines(files.get("Mammal.md").toPath())
                .stream().map(String::trim).collect(Collectors.toList());
        Assert.assertListContains(mammalDoc,
                a -> a.contains("[**\\OpenAPI\\Client\\Model\\Whale**](Whale.md)"),
                "Mammal doc links to its Whale member");
        Assert.assertListContains(mammalDoc,
                a -> a.contains("[**\\OpenAPI\\Client\\Model\\Zebra**](Zebra.md)"),
                "Mammal doc links to its Zebra member");
        Assert.assertListContains(mammalDoc,
                a -> a.contains("`class_name` discriminator"),
                "Mammal doc names the discriminator with the same property convention as the model docs (class_name)");
        Assert.assertListNotContains(mammalDoc,
                a -> a.equals("## Properties") || a.contains("**has_baleen**") || a.contains("**has_teeth**"),
                "Mammal doc must not present the umbrella's flattened member properties");

        // ObjectSerializer dispatches oneOf deserialization to the member types.
        List<String> objectSerializer = Files.readAllLines(files.get("ObjectSerializer.php").toPath())
                .stream().map(String::trim).collect(Collectors.toList());
        Assert.assertListContains(objectSerializer,
                a -> a.contains("private static function deserializeOneOf("),
                "ObjectSerializer resolves oneOf schemas");
    }

    @Test
    public void testOneOfAsPropertyType() throws IOException {
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

        // Zoo references the oneOf schemas Mammal (with discriminator) and Fruit (without) as
        // property types, so the accessors must be hinted with the PHP union of the member types.
        List<String> zoo = Files.readAllLines(files.get("Zoo.php").toPath())
                .stream().map(String::trim).collect(Collectors.toList());

        // A required oneOf property is hinted as the bare union (no `|null`).
        Assert.assertListContains(zoo,
                a -> a.equals("public function getFavoriteMammal(): \\OpenAPI\\Client\\Model\\Whale|\\OpenAPI\\Client\\Model\\Zebra"),
                "required Mammal property getter is hinted as the member union");
        Assert.assertListContains(zoo,
                a -> a.equals("public function setFavoriteMammal(\\OpenAPI\\Client\\Model\\Whale|\\OpenAPI\\Client\\Model\\Zebra $favorite_mammal): static"),
                "required Mammal property setter is hinted as the member union");

        // PHP forbids `?` on unions, so an optional oneOf property gains an explicit `|null` member.
        Assert.assertListContains(zoo,
                a -> a.equals("public function getOptionalMammal(): \\OpenAPI\\Client\\Model\\Whale|\\OpenAPI\\Client\\Model\\Zebra|null"),
                "optional Mammal property getter appends |null to the union");
        Assert.assertListContains(zoo,
                a -> a.equals("public function getSnack(): \\OpenAPI\\Client\\Model\\Apple|\\OpenAPI\\Client\\Model\\Banana|null"),
                "optional Fruit property getter appends |null to the union");
        Assert.assertListNotContains(zoo,
                a -> a.contains("?\\OpenAPI\\Client\\Model\\Whale") || a.contains("?\\OpenAPI\\Client\\Model\\Apple"),
                "a union type must never use the nullable shorthand `?`");

        // An array of a oneOf degrades to a plain `array` hint (the union lives only in the
        // phpdoc); being optional it keeps the `?` shorthand that is legal on a non-union type.
        Assert.assertListContains(zoo,
                a -> a.equals("public function getMammals(): ?array"),
                "array of Mammal property getter degrades to a (nullable) array hint");

        // The phpdoc must describe the concrete member union, not the oneOf alias: Apple and
        // Banana do not inherit from Fruit, so `@param Fruit` would be a lie.
        Assert.assertListContains(zoo,
                a -> a.equals("* @return \\OpenAPI\\Client\\Model\\Apple|\\OpenAPI\\Client\\Model\\Banana|null"),
                "snack @return phpdoc expands the Fruit oneOf to its member union");
        Assert.assertListContains(zoo,
                a -> a.equals("* @param \\OpenAPI\\Client\\Model\\Apple|\\OpenAPI\\Client\\Model\\Banana|null $snack snack"),
                "snack @param phpdoc expands the Fruit oneOf to its member union");
        Assert.assertListNotContains(zoo,
                a -> a.contains("@param \\OpenAPI\\Client\\Model\\Fruit") || a.contains("@return \\OpenAPI\\Client\\Model\\Fruit"),
                "the Fruit oneOf alias must not leak into the phpdoc");

        // An array of a oneOf is documented as `(Type1|Type2)[]` so the brackets bind to the union.
        Assert.assertListContains(zoo,
                a -> a.equals("* @return (\\OpenAPI\\Client\\Model\\Whale|\\OpenAPI\\Client\\Model\\Zebra)[]|null"),
                "mammals @return phpdoc nests the member union inside the array");
        Assert.assertListContains(zoo,
                a -> a.equals("* @param (\\OpenAPI\\Client\\Model\\Whale|\\OpenAPI\\Client\\Model\\Zebra)[]|null $mammals mammals"),
                "mammals @param phpdoc nests the member union inside the array");
    }

    @Test
    public void testFormParamsBlockOnlyEmittedWhenBodyOrFormParamsExist() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/php-nextgen/form-body-params.yaml", null, new ParseOptions())
                .getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());
        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        String apiContent = Files.readString(files.get("DefaultApi.php").toPath());

        // Each operation produces a "*Request" helper holding its own httpBody handling.
        String noBodyNoForm = extractMethod(apiContent, "noBodyNoFormRequest");
        String bodyParam = extractMethod(apiContent, "bodyParamRequest");
        String formParam = extractMethod(apiContent, "formParamRequest");

        // No body, no form: the dead form-params block must not be generated at all.
        Assert.assertFalse(noBodyNoForm.contains("if (count($formParams) > 0)"),
                "Operation without body or form params must not emit the form-params block");
        Assert.assertFalse(noBodyNoForm.contains("// for model (json/xml)"),
                "Operation without body or form params must not emit the json/xml comment");

        // Body param: emits the body block followed by the elseif form-params branch.
        Assert.assertTrue(bodyParam.contains("if (isset($thing)) {"),
                "Body param operation must emit the body serialization block");
        Assert.assertTrue(bodyParam.contains("} elseif (count($formParams) > 0) {"),
                "Body param operation must keep the elseif form-params branch");

        // Form param: emits the standalone form-params block.
        Assert.assertTrue(formParam.contains("if (count($formParams) > 0) {"),
                "Form param operation must emit the standalone form-params block");
        Assert.assertFalse(formParam.contains("} elseif (count($formParams) > 0) {"),
                "Form-only operation must not have an elseif (no body branch precedes it)");
    }

    /**
     * Extracts the source of a single PHP method (from its declaration up to, but not
     * including, the next method declaration) so per-operation assertions don't leak
     * across operations sharing the same generated file.
     */
    private static String extractMethod(String content, String methodName) {
        int start = content.indexOf("function " + methodName + "(");
        Assert.assertTrue(start >= 0, "Expected to find method " + methodName + " in generated API");
        int next = content.indexOf("\n    public function ", start + 1);
        if (next < 0) {
            next = content.indexOf("\n    protected function ", start + 1);
        }
        return next < 0 ? content.substring(start) : content.substring(start, next);
    }
    @Test
    public void testDateTimeLengthValidationIsNotGenerated() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        ObjectSchema model = new ObjectSchema();
        model.addProperties("startsAt", new DateTimeSchema().minLength(20).maxLength(25));
        model.addProperties("title", new StringSchema().minLength(2).maxLength(10));

        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("ProductDeal", model);
        codegen.setOutputDir(output.getAbsolutePath());

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        String modelPhp = String.join("\n", Files.readAllLines(files.get("ProductDeal.php").toPath()));

        Assert.assertFalse(modelPhp.contains("mb_strlen($this->container['starts_at'])"), modelPhp);
        Assert.assertFalse(modelPhp.contains("mb_strlen($starts_at)"), modelPhp);
        Assert.assertTrue(modelPhp.contains("mb_strlen($this->container['title']) > 10"), modelPhp);
        Assert.assertTrue(modelPhp.contains("mb_strlen($title) > 10"), modelPhp);
    }
}