package org.openapitools.codegen.typescript.fetch;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.MapSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import java.util.Collections;
import java.util.Locale;
import java.util.stream.Stream;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.AbstractTypeScriptClientCodegen;
import org.openapitools.codegen.languages.TypeScriptFetchClientCodegen;
import org.openapitools.codegen.typescript.TypeScriptGroups;
import org.openapitools.codegen.utils.ModelUtils;
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

import static org.assertj.core.api.Assertions.assertThat;

@Test(groups = {TypeScriptGroups.TYPESCRIPT, TypeScriptGroups.TYPESCRIPT_FETCH})
public class TypeScriptFetchClientCodegenTest {
    @Test
    public void testSnapshotVersion() {
        OpenAPI api = TestUtils.createOpenAPI();
        TypeScriptFetchClientCodegen codegen = new TypeScriptFetchClientCodegen();

        codegen.additionalProperties().put("npmName", "@openapi/typescript-fetch-petstore");
        codegen.additionalProperties().put("snapshot", true);
        codegen.additionalProperties().put("npmVersion", "1.0.0-SNAPSHOT");
        codegen.processOpts();
        codegen.preprocessOpenAPI(api);

        Assert.assertTrue(codegen.getNpmVersion().matches("^1.0.0-SNAPSHOT.[0-9]{12}$"));

        codegen = new TypeScriptFetchClientCodegen();
        codegen.additionalProperties().put("npmName", "@openapi/typescript-fetch-petstore");
        codegen.additionalProperties().put("snapshot", true);
        codegen.additionalProperties().put("npmVersion", "3.0.0-M1");
        codegen.processOpts();
        codegen.preprocessOpenAPI(api);

        Assert.assertTrue(codegen.getNpmVersion().matches("^3.0.0-M1-SNAPSHOT.[0-9]{12}$"));

    }

    @Test
    public void testOptionalResponseImports() {
        TypeScriptFetchClientCodegen codegen = new TypeScriptFetchClientCodegen();
        final OpenAPI openApi = TestUtils.parseFlattenSpec("src/test/resources/3_0/optionalResponse.yaml");
        codegen.setOpenAPI(openApi);
        PathItem path = openApi.getPaths().get("/api/Users/{userId}");
        CodegenOperation operation = codegen.fromOperation("/api/Users/{userId}", "get", path.getGet(), path.getServers());
        Assert.assertEquals(operation.isResponseOptional, true);
    }

    @Test
    public void testModelsWithoutPaths() throws IOException {
        final String specPath = "src/test/resources/3_1/reusable-components-without-paths.yaml";

        Map<String, Object> properties = new HashMap<>();
        properties.put("supportsES6", true);

        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("typescript-fetch")
                .setInputSpec(specPath)
                .setAdditionalProperties(properties)
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        Generator generator = new DefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        TestUtils.assertFileExists(Paths.get(output + "/index.ts"));
        TestUtils.assertFileExists(Paths.get(output + "/runtime.ts"));
        TestUtils.assertFileExists(Paths.get(output + "/models/Pet.ts"));
        TestUtils.assertFileExists(Paths.get(output + "/models/index.ts"));
    }

    @Test
    public void testWithoutSnapshotVersion() {
        OpenAPI api = TestUtils.createOpenAPI();
        TypeScriptFetchClientCodegen codegen = new TypeScriptFetchClientCodegen();

        codegen.additionalProperties().put("npmName", "@openapi/typescript-fetch-petstore");
        codegen.additionalProperties().put("snapshot", false);
        codegen.additionalProperties().put("npmVersion", "1.0.0-SNAPSHOT");
        codegen.processOpts();
        codegen.preprocessOpenAPI(api);

        Assert.assertTrue(codegen.getNpmVersion().matches("^1.0.0-SNAPSHOT$"));

        codegen = new TypeScriptFetchClientCodegen();
        codegen.additionalProperties().put("npmName", "@openapi/typescript-fetch-petstore");
        codegen.additionalProperties().put("snapshot", false);
        codegen.additionalProperties().put("npmVersion", "3.0.0-M1");
        codegen.processOpts();
        codegen.preprocessOpenAPI(api);

        Assert.assertTrue(codegen.getNpmVersion().matches("^3.0.0-M1$"));

    }

    @Test
    public void toVarName() {
        TypeScriptFetchClientCodegen codegen = new TypeScriptFetchClientCodegen();
        codegen.processOpts();
        Assert.assertEquals(codegen.toVarName("valid_var"), "validVar");

        codegen = new TypeScriptFetchClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.MODEL_PROPERTY_NAMING, "original");
        codegen.processOpts();
        Assert.assertEquals(codegen.toVarName("valid_var"), "valid_var");
    }

    @Test
    public void toEnumVarName() {
        TypeScriptFetchClientCodegen codegen = new TypeScriptFetchClientCodegen();
        codegen.processOpts();
        Assert.assertEquals(codegen.toEnumVarName("", "string"), "Empty");
        Assert.assertEquals(codegen.toEnumVarName("$", "string"), "Dollar");
        Assert.assertEquals(codegen.toEnumVarName("valid_var", "string"), "ValidVar");
        Assert.assertEquals(codegen.toEnumVarName("-valid_var+", "string"), "ValidVar");
        Assert.assertEquals(codegen.toEnumVarName("30valid_+var", "string"), "_30validVar");
        Assert.assertEquals(codegen.toEnumVarName("VALID:var", "string"), "ValidVar");

        codegen = new TypeScriptFetchClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.ENUM_PROPERTY_NAMING, "original");
        codegen.processOpts();
        Assert.assertEquals(codegen.toEnumVarName("", "string"), "empty");
        Assert.assertEquals(codegen.toEnumVarName("$", "string"), "Dollar");
        Assert.assertEquals(codegen.toEnumVarName("valid_var", "string"), "valid_var");
        Assert.assertEquals(codegen.toEnumVarName("-valid_var+", "string"), "valid_var");
        Assert.assertEquals(codegen.toEnumVarName("30valid_+var", "string"), "_30valid_var");
        Assert.assertEquals(codegen.toEnumVarName("VALID:var", "string"), "VALID_var");

        codegen = new TypeScriptFetchClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.ENUM_PROPERTY_NAMING, "UPPERCASE");
        codegen.additionalProperties().put(AbstractTypeScriptClientCodegen.ENUM_PROPERTY_NAMING_REPLACE_SPECIAL_CHAR, "true");
        codegen.processOpts();
        Assert.assertEquals(codegen.toEnumVarName("", "string"), "EMPTY");
        Assert.assertEquals(codegen.toEnumVarName("$", "string"), "DOLLAR");
        Assert.assertEquals(codegen.toEnumVarName("valid_var", "string"), "VALID_VAR");
        Assert.assertEquals(codegen.toEnumVarName("-valid_+var", "string"), "MINUS_VALID_PLUS_VAR");
        Assert.assertEquals(codegen.toEnumVarName("-valid_var+", "string"), "MINUS_VALID_VAR_PLUS");
        Assert.assertEquals(codegen.toEnumVarName("30valid_+var", "string"), "_30VALID_PLUS_VAR");
        Assert.assertEquals(codegen.toEnumVarName("VALID:var", "string"), "VALID_VAR");
    }

    @Test
    public void getTypeDeclarationTest() {
        Schema<?> childSchema = new ArraySchema().items(new StringSchema());

        OpenAPI api = TestUtils.createOpenAPI();
        api.getComponents().addSchemas("Child", childSchema);

        TypeScriptFetchClientCodegen codegen = new TypeScriptFetchClientCodegen();
        codegen.setOpenAPI(api);

        // Cf. issue #4968: Array of Alias of Array
        Schema<?> parentSchema = new ArraySchema().items(
                new Schema().$ref("#/components/schemas/Child")
        );

        ModelUtils.setGenerateAliasAsModel(false);
        Assert.assertEquals(codegen.getTypeDeclaration(parentSchema), "Array<Array<string>>");

        ModelUtils.setGenerateAliasAsModel(true);
        Assert.assertEquals(codegen.getTypeDeclaration(parentSchema), "Array<Child>");

        // Same for Map
        parentSchema = new MapSchema().additionalProperties(new Schema().$ref("#/components/schemas/Child"));

        ModelUtils.setGenerateAliasAsModel(false);
        Assert.assertEquals(codegen.getTypeDeclaration(parentSchema), "{ [key: string]: Array<string>; }");

        ModelUtils.setGenerateAliasAsModel(true);
        Assert.assertEquals(codegen.getTypeDeclaration(parentSchema), "{ [key: string]: Child; }");
    }

    @Test
    public void containsESMTSConfigFileInCaseOfES6AndNPM() {
        TypeScriptFetchClientCodegen codegen = new TypeScriptFetchClientCodegen();

        codegen.additionalProperties().put("npmName", "@openapi/typescript-fetch-petstore");
        codegen.additionalProperties().put("snapshot", false);
        codegen.additionalProperties().put("npmVersion", "1.0.0-SNAPSHOT");
        codegen.additionalProperties().put("supportsES6", true);

        codegen.processOpts();

        assertThat(codegen.supportingFiles()).contains(new SupportingFile("tsconfig.mustache", "", "tsconfig.json"));
        assertThat(codegen.supportingFiles()).contains(new SupportingFile("tsconfig.esm.mustache", "", "tsconfig.esm.json"));
    }

    @Test
    public void doesNotContainESMTSConfigFileInCaseOfES5AndNPM() {
        TypeScriptFetchClientCodegen codegen = new TypeScriptFetchClientCodegen();

        codegen.additionalProperties().put("npmName", "@openapi/typescript-fetch-petstore");
        codegen.additionalProperties().put("snapshot", false);
        codegen.additionalProperties().put("npmVersion", "1.0.0-SNAPSHOT");
        codegen.additionalProperties().put("supportsES6", false);

        codegen.processOpts();

        assertThat(codegen.supportingFiles()).contains(new SupportingFile("tsconfig.mustache", "", "tsconfig.json"));
        assertThat(codegen.supportingFiles()).doesNotContain(new SupportingFile("tsconfig.esm.mustache", "", "tsconfig.esm.json"));
    }

    @Test(description = "Verify file name formatting from model name in PascalCase")
    public void testModelFileNameInPascalCase() {
        final TypeScriptFetchClientCodegen codegen = new TypeScriptFetchClientCodegen();
        codegen.setFileNaming(TypeScriptFetchClientCodegen.PASCAL_CASE);
        Assert.assertEquals("FirstSimpleModel", codegen.toModelFilename("FirstSimpleModel"));
        codegen.setModelNameSuffix("suffix");
        Assert.assertEquals("FirstSimpleModelSuffix", codegen.toModelFilename("FirstSimpleModel"));
        codegen.setModelNamePrefix("prefix");
        Assert.assertEquals("PrefixFirstSimpleModelSuffix", codegen.toModelFilename("FirstSimpleModel"));
    }

    @Test(description = "Verify file name formatting from model name in camelCase")
    public void testModelFileNameInCamelCase() {
        final TypeScriptFetchClientCodegen codegen = new TypeScriptFetchClientCodegen();
        codegen.setFileNaming(TypeScriptFetchClientCodegen.CAMEL_CASE);
        Assert.assertEquals("firstSimpleModel", codegen.toModelFilename("FirstSimpleModel"));
        codegen.setModelNameSuffix("suffix");
        Assert.assertEquals("firstSimpleModelSuffix", codegen.toModelFilename("FirstSimpleModel"));
        codegen.setModelNamePrefix("prefix");
        Assert.assertEquals("prefixFirstSimpleModelSuffix", codegen.toModelFilename("FirstSimpleModel"));
    }

    @Test(description = "Verify file name formatting from model name in kebab-case")
    public void testModelFileNameInKebabCase() {
        final TypeScriptFetchClientCodegen codegen = new TypeScriptFetchClientCodegen();
        codegen.setFileNaming("kebab-case");
        Assert.assertEquals("first-simple-model", codegen.toModelFilename("FirstSimpleModel"));
        codegen.setModelNameSuffix("suffix");
        Assert.assertEquals("first-simple-model-suffix", codegen.toModelFilename("FirstSimpleModel"));
        codegen.setModelNamePrefix("prefix");
        Assert.assertEquals("prefix-first-simple-model-suffix", codegen.toModelFilename("FirstSimpleModel"));
    }

    @Test(description = "Verify file name formatting from api name in PascalCase, camelCase and kebab-case")
    public void testApiFileNameInVariousFormat() {
        final TypeScriptFetchClientCodegen codegen = new TypeScriptFetchClientCodegen();
        codegen.setFileNaming(TypeScriptFetchClientCodegen.PASCAL_CASE);
        String prefix = codegen.getApiNamePrefix() != null ? codegen.getApiNamePrefix() : "";
        String suffix = codegen.getApiNameSuffix() != null ? codegen.getApiNameSuffix() : "";
        Assert.assertEquals(StringUtils.capitalize(prefix + "FirstSimpleController") + StringUtils.capitalize(suffix),
                codegen.toApiFilename("FirstSimpleController"));
        codegen.setFileNaming(TypeScriptFetchClientCodegen.CAMEL_CASE);
        Assert.assertEquals(StringUtils.uncapitalize(prefix + "FirstSimpleController") + StringUtils.capitalize(suffix),
                codegen.toApiFilename("FirstSimpleController"));
        codegen.setFileNaming(TypeScriptFetchClientCodegen.KEBAB_CASE);
        Assert.assertEquals((prefix.isBlank() ? "" : (StringUtils.lowerCase(suffix) + "-")) + "first-simple-controller" + (suffix.isBlank() ? "" : ("-" + StringUtils.lowerCase(suffix))),
                codegen.toApiFilename("FirstSimpleController"));
    }

    @Test(description = "Verify names of files generated in kebab-case and imports with additional model prefix")
    public void testGeneratedFilenamesInPascalCaseWithAdditionalModelPrefix() throws IOException {

        Map<String, Object> properties = new HashMap<>();
        properties.put("fileNaming", TypeScriptFetchClientCodegen.PASCAL_CASE);
        properties.put(CodegenConstants.MODEL_NAME_PREFIX, "SomePrefix");

        File output = generate(properties);

        Path pet = Paths.get(output + "/models/SomePrefixPet.ts");
        TestUtils.assertFileExists(pet);
        TestUtils.assertFileContains(pet, "} from './SomePrefixPetCategory';");
        TestUtils.assertFileExists(Paths.get(output + "/models/SomePrefixPetCategory.ts"));
        TestUtils.assertFileExists(Paths.get(output + "/apis/PetControllerApi.ts"));
    }

    @Test(description = "Verify names of files generated in kebab-case and imports")
    public void testGeneratedFilenamesInKebabCase() throws IOException {

        Map<String, Object> properties = new HashMap<>();
        properties.put("fileNaming", TypeScriptFetchClientCodegen.KEBAB_CASE);

        File output = generate(properties);

        Path pet = Paths.get(output + "/models/pet.ts");
        TestUtils.assertFileExists(pet);
        TestUtils.assertFileContains(pet, "} from './pet-category';");
        TestUtils.assertFileExists(Paths.get(output + "/models/pet-category.ts"));
        TestUtils.assertFileExists(Paths.get(output + "/apis/pet-controller-api.ts"));
    }

    @Test(description = "Verify names of files generated in kebab-case and imports with additional model prefix")
    public void testGeneratedFilenamesInKebabCaseWithAdditionalModelPrefix() throws IOException {

        Map<String, Object> properties = new HashMap<>();
        properties.put("fileNaming", TypeScriptFetchClientCodegen.KEBAB_CASE);
        properties.put(CodegenConstants.MODEL_NAME_PREFIX, "SomePrefix");

        File output = generate(properties);

        Path pet = Paths.get(output + "/models/some-prefix-pet.ts");
        TestUtils.assertFileExists(pet);
        TestUtils.assertFileContains(pet, "} from './some-prefix-pet-category';");
        TestUtils.assertFileExists(Paths.get(output + "/models/some-prefix-pet-category.ts"));
        TestUtils.assertFileExists(Paths.get(output + "/apis/pet-controller-api.ts"));
    }

    @Test(description = "Verify names of files generated in camelCase and imports")
    public void testGeneratedFilenamesInCamelCase() throws IOException {

        Map<String, Object> properties = new HashMap<>();
        properties.put("fileNaming", TypeScriptFetchClientCodegen.CAMEL_CASE);

        File output = generate(properties);

        Path pet = Paths.get(output + "/models/pet.ts");
        TestUtils.assertFileExists(pet);
        TestUtils.assertFileContains(pet, "} from './petCategory';");
        TestUtils.assertFileExists(Paths.get(output + "/models/petCategory.ts"));
        TestUtils.assertFileExists(Paths.get(output + "/apis/petControllerApi.ts"));
    }

    @Test(description = "Verify names of files generated in camelCase and imports with additional model prefix")
    public void testGeneratedFilenamesInCamelCaseWithAdditionalModelPrefix() throws IOException {
        Map<String, Object> properties = new HashMap<>();
        properties.put("fileNaming", TypeScriptFetchClientCodegen.CAMEL_CASE);
        properties.put(CodegenConstants.MODEL_NAME_PREFIX, "SomePrefix");

        File output = generate(properties);

        Path pet = Paths.get(output + "/models/somePrefixPet.ts");
        TestUtils.assertFileExists(pet);
        TestUtils.assertFileContains(pet, "} from './somePrefixPetCategory';");
        TestUtils.assertFileExists(Paths.get(output + "/models/somePrefixPetCategory.ts"));
        TestUtils.assertFileExists(Paths.get(output + "/apis/petControllerApi.ts"));
    }

    @Test(description = "Issue #21295")
    public void givenSchemaIsOneOfAndComposedSchemasArePrimitiveThenReturnStatementsAreCorrect() throws Exception {
        File outputPath = generate(
            Collections.emptyMap(),
            "src/test/resources/bugs/issue_21259.yaml"
        );

        Path exampleModelPath = Paths.get(outputPath + "/models/MyCustomSpeed.ts");
        //FromJSON
        TestUtils.assertFileContains(exampleModelPath, "(typeof json !== 'object')");
        TestUtils.assertFileContains(exampleModelPath, "(instanceOfMyNumericValue(json))");
        TestUtils.assertFileContains(exampleModelPath, "(typeof json === 'number' && (json === 10 || json === 20 || json === 30))");
        TestUtils.assertFileContains(exampleModelPath, "(typeof json === 'string' && (json === 'fixed-value-a' || json === 'fixed-value-b' || json === 'fixed-value-c'))");
        TestUtils.assertFileContains(exampleModelPath, "(isNaN(new Date(json).getTime())");
        TestUtils.assertFileContains(exampleModelPath, "(json.every(item => typeof item === 'number'))");
        TestUtils.assertFileContains(exampleModelPath, "(json.every(item => typeof item === 'string' && (item === 'oneof-array-enum-a' || item === 'oneof-array-enum-b' || item === 'oneof-array-enum-c')))");
        //ToJSON
        TestUtils.assertFileContains(exampleModelPath, "(typeof value !== 'object')");
        TestUtils.assertFileContains(exampleModelPath, "(instanceOfMyNumericValue(value))");
        TestUtils.assertFileContains(exampleModelPath, "(typeof value === 'number' && (value === 10 || value === 20 || value === 30))");
        TestUtils.assertFileContains(exampleModelPath, "(typeof value === 'string' && (value === 'fixed-value-a' || value === 'fixed-value-b' || value === 'fixed-value-c'))");
        TestUtils.assertFileContains(exampleModelPath, "(value instanceof Date)");
        TestUtils.assertFileContains(exampleModelPath, "(value.every(item => typeof item === 'number'))");
        TestUtils.assertFileContains(exampleModelPath, "(value.every(item => typeof item === 'string' && (item === 'oneof-array-enum-a' || item === 'oneof-array-enum-b' || item === 'oneof-array-enum-c')))");
    }

    /**
     * Issue #19909
     * When using oneOf, the Typescript Fetch generator should not import primitive types.
     * Complex types should be imported, when the response has the type itself or the type is part
     * of an array.
     */
    @Test(description = "Verify oneOf model files do not import primitive types")
    public void testOneOfModelsDoNotImportPrimitiveTypes() throws IOException {
        File output = generate(Collections.emptyMap(), "src/test/resources/3_0/typescript-fetch/oneOf.yaml");

        Path testResponse = Paths.get(output + "/models/TestResponse.ts");
        TestUtils.assertFileExists(testResponse);
        TestUtils.assertFileContains(testResponse, "import type { TestA } from './TestA'");
        TestUtils.assertFileContains(testResponse, "import type { TestB } from './TestB'");
        TestUtils.assertFileNotContains(testResponse, "import type { string } from './string'");
        TestUtils.assertFileContains(testResponse, "export type TestResponse = TestA | TestB | string");

        Path testArrayResponse = Paths.get(output + "/models/TestArrayResponse.ts");
        TestUtils.assertFileExists(testArrayResponse);
        TestUtils.assertFileContains(testArrayResponse, "import type { TestA } from './TestA'");
        TestUtils.assertFileContains(testArrayResponse, "import type { TestB } from './TestB'");
        TestUtils.assertFileNotContains(testResponse, "import type { string } from './string'");
        TestUtils.assertFileContains(testArrayResponse, "export type TestArrayResponse = Array<TestA> | Array<TestB> | Array<string>");

        Path testDiscriminatorResponse = Paths.get(output + "/models/TestDiscriminatorResponse.ts");
        TestUtils.assertFileExists(testDiscriminatorResponse);
        TestUtils.assertFileContains(testDiscriminatorResponse, "import type { OptionOne } from './OptionOne'");
        TestUtils.assertFileContains(testDiscriminatorResponse, "import type { OptionTwo } from './OptionTwo'");
        TestUtils.assertFileContains(testDiscriminatorResponse, "export type TestDiscriminatorResponse = { discriminatorField: 'optionOne' } & OptionOne | { discriminatorField: 'optionTwo' } & OptionTwo");
    }

    /**
     * Issue #21587
     * When using oneOf, the Typescript Fetch generator should import modelled types except for
     * types built-in primitive types, even those marked with additional properties.
     */
    @Test()
    public void testOneOfModelsImportNonPrimitiveTypes() throws IOException {
        File output = generate(
            Collections.emptyMap(),
            "src/test/resources/3_0/typescript-fetch/issue_21587.yaml"
        );

        Path testResponse = Paths.get(output + "/models/OneOfResponse.ts");
        TestUtils.assertFileExists(testResponse);

        // Primitive built-in types should not be included. This list is based off the type mappings
        // and language specific primitive keywords established in the AbstractTypeScriptClientCodegen
        Stream.of(
            "Set",
            "Array",
            "boolean",
            "string",
            "number",
            "object",
            "any",
            "Date",
            "Error"
        ).forEach(primitiveType ->
            TestUtils.assertFileNotContains(
                testResponse,
                String.format(Locale.ROOT, "import type { %s } from './%s'", primitiveType, primitiveType)
            )
        );
        TestUtils.assertFileContains(testResponse, "import type { OptionOne } from './OptionOne'");
        TestUtils.assertFileContains(testResponse, "import type { OptionTwo } from './OptionTwo'");
        TestUtils.assertFileContains(testResponse, "import type { OptionThree } from './OptionThree'");
    }

    private static File generate(
        Map<String, Object> properties
    ) throws IOException {
        return generate(
            properties,
            "src/test/resources/3_0/typescript-fetch/example-for-file-naming-option.yaml"
        );
    }

    private static File generate(
        Map<String, Object> properties,
        String inputSpec
    ) throws IOException {
        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("typescript-fetch")
                .setInputSpec(inputSpec)
                .setAdditionalProperties(properties)
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        Generator generator = new DefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);
        return output;
    }
}
