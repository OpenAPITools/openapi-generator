package org.openapitools.codegen.typescript.axios;

import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.TypeScriptAxiosClientCodegen;
import org.openapitools.codegen.typescript.TypeScriptGroups;
import org.testng.annotations.Test;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.testng.Assert.assertEquals;

@Test(groups = {TypeScriptGroups.TYPESCRIPT, TypeScriptGroups.TYPESCRIPT_AXIOS})
public class TypeScriptAxiosClientCodegenTest {

    TypeScriptAxiosClientCodegen codegen = new TypeScriptAxiosClientCodegen();

    @Test
    public void testToEnumVarNameOriginalNamingType() {
        codegen.additionalProperties().put(CodegenConstants.ENUM_PROPERTY_NAMING, CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.original.name());
        codegen.processOpts();
        assertEquals(codegen.toEnumVarName("SCIENCE", "string"), "SCIENCE");
        assertEquals(codegen.toEnumVarName("SCIENCE_FICTION", "string"), "SCIENCE_FICTION");
        assertEquals(codegen.toEnumVarName("science", "string"), "science");
        assertEquals(codegen.toEnumVarName("science_fiction", "string"), "science_fiction");
        assertEquals(codegen.toEnumVarName("scienceFiction", "string"), "scienceFiction");
        assertEquals(codegen.toEnumVarName("ScienceFiction", "string"), "ScienceFiction");
        assertEquals(codegen.toEnumVarName("A", "string"), "A");
        assertEquals(codegen.toEnumVarName("b", "string"), "b");
    }

    @Test
    public void testToEnumVarNameCamelCaseNamingType() {
        codegen.additionalProperties().put(CodegenConstants.ENUM_PROPERTY_NAMING, CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.camelCase.name());
        codegen.processOpts();
        assertEquals(codegen.toEnumVarName("SCIENCE", "string"), "science");
        assertEquals(codegen.toEnumVarName("SCIENCE_FICTION", "string"), "scienceFiction");
        assertEquals(codegen.toEnumVarName("science", "string"), "science");
        assertEquals(codegen.toEnumVarName("science_fiction", "string"), "scienceFiction");
        assertEquals(codegen.toEnumVarName("scienceFiction", "string"), "scienceFiction");
        assertEquals(codegen.toEnumVarName("ScienceFiction", "string"), "scienceFiction");
    }

    @Test
    public void testToEnumVarNamePascalCaseNamingType() {
        codegen.additionalProperties().put(CodegenConstants.ENUM_PROPERTY_NAMING, CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.PascalCase.name());
        codegen.processOpts();
        assertEquals(codegen.toEnumVarName("SCIENCE", "string"), "Science");
        assertEquals(codegen.toEnumVarName("SCIENCE_FICTION", "string"), "ScienceFiction");
        assertEquals(codegen.toEnumVarName("science", "string"), "Science");
        assertEquals(codegen.toEnumVarName("science_fiction", "string"), "ScienceFiction");
        assertEquals(codegen.toEnumVarName("scienceFiction", "string"), "ScienceFiction");
        assertEquals(codegen.toEnumVarName("ScienceFiction", "string"), "ScienceFiction");
        assertEquals(codegen.toEnumVarName("A", "string"), "A");
        assertEquals(codegen.toEnumVarName("b", "string"), "B");
    }

    @Test
    public void testToEnumVarNameSnakeCaseNamingType() {
        codegen.additionalProperties().put(CodegenConstants.ENUM_PROPERTY_NAMING, CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.snake_case.name());
        codegen.processOpts();
        assertEquals(codegen.toEnumVarName("SCIENCE", "string"), "science");
        assertEquals(codegen.toEnumVarName("SCIENCE_FICTION", "string"), "science_fiction");
        assertEquals(codegen.toEnumVarName("science", "string"), "science");
        assertEquals(codegen.toEnumVarName("science_fiction", "string"), "science_fiction");
        assertEquals(codegen.toEnumVarName("scienceFiction", "string"), "science_fiction");
        assertEquals(codegen.toEnumVarName("ScienceFiction", "string"), "science_fiction");
        assertEquals(codegen.toEnumVarName("A", "string"), "a");
        assertEquals(codegen.toEnumVarName("b", "string"), "b");
    }

    @Test
    public void testToEnumVarNameUpperCaseNamingType() {
        codegen.additionalProperties().put(CodegenConstants.ENUM_PROPERTY_NAMING, CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.UPPERCASE.name());
        codegen.processOpts();
        assertEquals(codegen.toEnumVarName("SCIENCE", "string"), "SCIENCE");
        assertEquals(codegen.toEnumVarName("SCIENCE_FICTION", "string"), "SCIENCE_FICTION");
        assertEquals(codegen.toEnumVarName("science", "string"), "SCIENCE");
        assertEquals(codegen.toEnumVarName("science_fiction", "string"), "SCIENCE_FICTION");
        assertEquals(codegen.toEnumVarName("scienceFiction", "string"), "SCIENCE_FICTION");
        assertEquals(codegen.toEnumVarName("ScienceFiction", "string"), "SCIENCE_FICTION");
        assertEquals(codegen.toEnumVarName("A", "string"), "A");
        assertEquals(codegen.toEnumVarName("b", "string"), "B");
    }

    @Test
    public void containsESMTSConfigFileInCaseOfES6AndNPM() {
        TypeScriptAxiosClientCodegen codegen = new TypeScriptAxiosClientCodegen();

        codegen.additionalProperties().put("npmName", "@openapi/typescript-axios-petstore");
        codegen.additionalProperties().put("snapshot", false);
        codegen.additionalProperties().put("npmVersion", "1.0.0-SNAPSHOT");
        codegen.additionalProperties().put("supportsES6", true);

        codegen.processOpts();

        assertThat(codegen.supportingFiles()).contains(new SupportingFile("tsconfig.mustache", "", "tsconfig.json"));
        assertThat(codegen.supportingFiles()).contains(new SupportingFile("tsconfig.esm.mustache", "", "tsconfig.esm.json"));
    }

    @Test
    public void doesNotContainESMTSConfigFileInCaseOfES5AndNPM() {
        TypeScriptAxiosClientCodegen codegen = new TypeScriptAxiosClientCodegen();

        codegen.additionalProperties().put("npmName", "@openapi/typescript-axios-petstore");
        codegen.additionalProperties().put("snapshot", false);
        codegen.additionalProperties().put("npmVersion", "1.0.0-SNAPSHOT");
        codegen.additionalProperties().put("supportsES6", false);

        codegen.processOpts();

        assertThat(codegen.supportingFiles()).contains(new SupportingFile("tsconfig.mustache", "", "tsconfig.json"));
        assertThat(codegen.supportingFiles()).doesNotContain(new SupportingFile("tsconfig.esm.mustache", "", "tsconfig.esm.json"));
    }

    @Test
    public void testAppliesDefaultAxiosVersion() {
        TypeScriptAxiosClientCodegen codegen = new TypeScriptAxiosClientCodegen();

        codegen.processOpts();

        assertEquals(codegen.additionalProperties().get("axiosVersion"), TypeScriptAxiosClientCodegen.DEFAULT_AXIOS_VERSION);
    }

    @Test
    public void testAppliesCustomAxiosVersion() {
        TypeScriptAxiosClientCodegen codegen = new TypeScriptAxiosClientCodegen();
        codegen.additionalProperties().put("axiosVersion", "^1.2.3");

        codegen.processOpts();

        assertEquals(codegen.additionalProperties().get("axiosVersion"), "^1.2.3");
    }

    @Test(description = "Verify @deprecated annotation is generated for array-type properties")
    public void testDeprecatedArrayAttribute() throws Exception {
        final File output = Files.createTempDirectory("typescript_axios_deprecated_array_").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("typescript-axios")
                .setInputSpec("src/test/resources/3_1/typescript-axios/deprecated-array-attribute.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        final DefaultGenerator generator = new DefaultGenerator();
        final List<File> files = generator.opts(clientOptInput).generate();
        files.forEach(File::deleteOnExit);

        Path file = Paths.get(output + "/api.ts");
        String content = Files.readString(file);

        // The model has three deprecated properties:
        //   'age' (integer), 'tags' (array of strings), 'oldTags' (array of $ref Tag objects)
        // and one non-deprecated array property: 'nicknames'
        // There should be exactly 3 occurrences of @deprecated in the model file
        assertEquals(TestUtils.countOccurrences(content, "@deprecated"), 3,
                "Expected @deprecated on 'age' (scalar), 'tags' (array of strings), and 'oldTags' (array of $ref objects)");

        // Verify the @deprecated annotation appears in the generated output
        TestUtils.assertFileContains(file, "* @deprecated");

        // Verify the deprecated array property 'tags' (array of primitives) is present with correct type
        TestUtils.assertFileContains(file, "'tags'?: Array<string>");

        // Verify the deprecated array property 'oldTags' (array of $ref objects) is present with correct type
        TestUtils.assertFileContains(file, "'oldTags'?: Array<Tag>");

        // Verify the non-deprecated array property 'nicknames' is also present
        TestUtils.assertFileContains(file, "'nicknames'?: Array<string>");
    }

    @Test(description = "Nullable oneOf references do not make other uses of the referenced schema nullable")
    public void testNullableReferenceDoesNotLeakIntoModelProperty() throws Exception {
        final File output = Files.createTempDirectory("typescript_axios_nullable_reference_").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("typescript-axios")
                .setInputSpec("src/test/resources/3_1/issue_23340.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path api = Paths.get(output + "/api.ts");
        TestUtils.assertFileContains(api,
                "export interface TitleGroupHierarchyLite {",
                "'content_type': ContentType;");
        TestUtils.assertFileNotContains(api, "'content_type': ContentType | null;");
        TestUtils.assertFileContains(api, "search: async (contentType?: ContentType | null");
    }

    @Test(description = "Explode query params that are not objects at runtime are sent as a single value")
    public void testExplodeQueryParamWithOneOfPrimitives() throws Exception {
        final File output = Files.createTempDirectory("typescript_axios_explode_oneof_primitives_").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("typescript-axios")
                .setInputSpec("src/test/resources/3_0/typescript-axios/explode-query-param-oneof-primitives.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path api = Paths.get(output + "/api.ts");
        TestUtils.assertFileContains(api,
                "if (typeof cursor === 'object' && cursor !== null && !Array.isArray(cursor)) {",
                "localVarQueryParameter['cursor'] = cursor;");
        TestUtils.assertFileContains(api,
                "if (typeof filter === 'object' && filter !== null && !Array.isArray(filter)) {",
                "for (const [key, value] of Object.entries(filter)) {");
    }

    @Test(description = "Verify multipart file arrays use repeated form fields")
    public void testMultipartFileArrayUsesRepeatedFormFields() throws Exception {
        final File output = Files.createTempDirectory("typescript_axios_multipart_file_array_").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("typescript-axios")
                .setInputSpec("src/test/resources/3_0/form-multipart-binary-array.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        final DefaultGenerator generator = new DefaultGenerator();
        final List<File> files = generator.opts(clientOptInput).generate();
        files.forEach(File::deleteOnExit);

        Path api = Paths.get(output + "/api.ts");
        TestUtils.assertFileExists(api);
        TestUtils.assertFileContains(api, "files?: Array<File>");
        TestUtils.assertFileContains(api, "files.forEach((element) => {");
        TestUtils.assertFileContains(api, "localVarFormParams.append('files', element as any);");
        TestUtils.assertFileNotContains(api, "files.join(COLLECTION_FORMATS.csv)");
    }

    @Test
    public void generatesNullUnionsForOpenApi31NullableContainers() throws Exception {
        final File output = Files.createTempDirectory("typescript_axios_nullable_container_types_").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("typescript-axios")
                .setInputSpec("src/test/resources/3_1/typescript-axios/nullable-container-types.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        final DefaultGenerator generator = new DefaultGenerator();
        final List<File> files = generator.opts(clientOptInput).generate();
        files.forEach(File::deleteOnExit);

        Path file = Paths.get(output + "/api.ts");

        TestUtils.assertFileContains(file, "'validation'?: { [key: string]: string; } | null;");
        TestUtils.assertFileContains(file, "'requirements'?: Array<string> | null;");
        TestUtils.assertFileContains(file, "'settings': MappingItemResourceSettings | null;");
    }

    @Test
    public void generatesTrailingCommasInAsConstEnumObjects() throws Exception {
        final File output = Files.createTempDirectory("typescript_axios_trailing_commas_").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("typescript-axios")
                .setInputSpec("src/test/resources/3_0/java/petstore-with-fake-endpoints-models-for-testing-with-http-signature.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        final DefaultGenerator generator = new DefaultGenerator();
        final List<File> files = generator.opts(clientOptInput).generate();
        files.forEach(File::deleteOnExit);

        Path file = Paths.get(output + "/api.ts");
        String content = Files.readString(file);

        assertThat(content).contains(
                "export const ChildCatPetTypeEnum = {\n" +
                        "    ChildCat: 'ChildCat',\n" +
                        "} as const;");
        assertThat(content).contains(
                "export const OuterEnum = {\n" +
                        "    Placed: 'placed',\n" +
                        "    Approved: 'approved',\n" +
                        "    Delivered: 'delivered',\n" +
                        "} as const;");
        assertThat(content).contains(
                "export const TestEnumParametersEnumHeaderStringEnum = {\n" +
                        "    Abc: '_abc',\n" +
                        "    Efg: '-efg',\n" +
                        "    Xyz: '(xyz)',\n" +
                        "} as const;");
    }


    @Test(description = "Generated package.json supports TypeScript 6")
    public void testGeneratedPackageJsonSupportsTypeScript6() throws Exception {
        final File output = Files.createTempDirectory("typescript_axios_typescript_6_").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("typescript-axios")
                .setInputSpec("src/test/resources/3_0/petstore.yaml")
                .addAdditionalProperty("npmName", "@openapi/typescript-axios-petstore")
                .addAdditionalProperty("snapshot", false)
                .addAdditionalProperty("npmVersion", "1.0.0-SNAPSHOT")
                .addAdditionalProperty("supportsES6", true)
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        TestUtils.assertFileContains(Paths.get(output + "/package.json"),
                "\"typescript\": \"^4.0 || ^5.0 || ^6.0\"");
        assertThat(Files.readString(Paths.get(output + "/tsconfig.json")))
                .doesNotContain("\"moduleResolution\": \"node\"");

    }

    @Test(description = "generateAnyOfAsUnion is an opt-in option that is off by default")
    public void testGenerateAnyOfAsUnionOption() {
        final CliOption option = codegen.cliOptions().stream()
                .filter(o -> TypeScriptAxiosClientCodegen.GENERATE_ANY_OF_AS_UNION.equals(o.getOpt()))
                .findFirst()
                .orElseThrow(() -> new AssertionError("generateAnyOfAsUnion option is not registered"));
        assertEquals(option.getDefault(), "false");

        codegen.processOpts();
        assertThat(codegen.isGenerateAnyOfAsUnion()).isFalse();

        final TypeScriptAxiosClientCodegen enabled = new TypeScriptAxiosClientCodegen();
        enabled.additionalProperties().put(TypeScriptAxiosClientCodegen.GENERATE_ANY_OF_AS_UNION, "true");
        enabled.processOpts();
        assertThat(enabled.isGenerateAnyOfAsUnion()).isTrue();
    }

    @Test(description = "Without generateAnyOfAsUnion an anyOf keeps being rendered as one merged interface")
    public void testAnyOfIsAMergedInterfaceByDefault() throws Exception {
        final Path output = generateAnyOfUnionSample(null, false);

        final Path api = output.resolve("api.ts");
        TestUtils.assertFileContains(api,
                "export interface ListItems200ResponseItemsInner {",
                "export interface GetSettings200ResponseListUnion {",
                "export interface ListItems422Response {");
        TestUtils.assertFileNotContains(api,
                "export type ListItems200ResponseItemsInner",
                "export type ListItems422Response");
    }

    @Test(description = "generateAnyOfAsUnion renders anyOf as a union type with separate models and api")
    public void testAnyOfAsUnionWithSeparateModelsAndApi() throws Exception {
        final Path output = generateAnyOfUnionSample(true, true);
        final Path models = output.resolve("models");

        // anyOf of objects: before, one interface requiring the fields of every member
        TestUtils.assertFileContains(models.resolve("list-items200-response-items-inner.ts"),
                "import type { Detailed } from './detailed';",
                "import type { Summary } from './summary';",
                "export type ListItems200ResponseItemsInner = Detailed | Summary;");
        TestUtils.assertFileNotContains(models.resolve("list-items200-response-items-inner.ts"), "'size': number;");
        // anyOf of an object and a dictionary: before, the dictionary was lost
        TestUtils.assertFileContains(models.resolve("get-settings200-response-meta.ts"),
                "export type GetSettings200ResponseMeta = Known | { [key: string]: string; };");
        // anyOf of a value or a list of values: before, an empty interface
        TestUtils.assertFileContains(models.resolve("get-settings200-response-overrides-value.ts"),
                "export type GetSettings200ResponseOverridesValue = Array<string> | string;");
        // anyOf of two lists: before, an empty interface
        TestUtils.assertFileContains(models.resolve("get-settings200-response-list-union.ts"),
                "export type GetSettings200ResponseListUnion = Array<Detailed> | Array<Summary>;");
        // anyOf of two error bodies: before, one interface requiring the fields of both
        TestUtils.assertFileContains(models.resolve("list-items422-response.ts"),
                "export type ListItems422Response = ActionError | Message;");
        // anyOf of primitives and a dictionary; nullability is kept where the schema is referenced
        TestUtils.assertFileContains(models.resolve("mixed-value.ts"),
                "export type MixedValue = boolean | number | string | { [key: string]: object; };");
        TestUtils.assertFileContains(models.resolve("holder.ts"), "'value': MixedValue | null;");
        // an anyOf that declares its own properties stays an interface
        TestUtils.assertFileContains(models.resolve("labelled.ts"),
                "export interface Labelled {",
                "'label'?: string;");
    }

    @Test(description = "generateAnyOfAsUnion renders anyOf as a union type when models are generated in api.ts")
    public void testAnyOfAsUnionInSingleApiFile() throws Exception {
        final Path output = generateAnyOfUnionSample(true, false);

        TestUtils.assertFileContains(output.resolve("api.ts"),
                "export type ListItems200ResponseItemsInner = Detailed | Summary;",
                "export type GetSettings200ResponseMeta = Known | { [key: string]: string; };",
                "export type GetSettings200ResponseOverridesValue = Array<string> | string;",
                "export type GetSettings200ResponseListUnion = Array<Detailed> | Array<Summary>;",
                "export type ListItems422Response = ActionError | Message;",
                "export type MixedValue = boolean | number | string | { [key: string]: object; };",
                "export interface Labelled {");
        TestUtils.assertFileNotContains(output.resolve("api.ts"),
                "export interface ListItems200ResponseItemsInner",
                "export interface ListItems422Response");
    }

    @Test(description = "generateAnyOfAsUnion renders an anyOf with a discriminator as a discriminated union")
    public void testAnyOfWithDiscriminatorAsUnion() throws Exception {
        final String discriminatedUnion = "export type Pet = { petType: 'cat' } & Cat | { petType: 'dog' } & Dog;";

        // separate models
        TestUtils.assertFileContains(generateAnyOfUnionSample(true, true).resolve("models/pet.ts"),
                "import type { Cat } from './cat';",
                "import type { Dog } from './dog';",
                discriminatedUnion);
        // models in api.ts
        final Path api = generateAnyOfUnionSample(true, false).resolve("api.ts");
        TestUtils.assertFileContains(api, discriminatedUnion);
        TestUtils.assertFileNotContains(api, "export interface Pet {");
        // without the option the anyOf is still one interface merging the members
        TestUtils.assertFileContains(generateAnyOfUnionSample(null, false).resolve("api.ts"), "export interface Pet {");
    }

    @Test(description = "generateAnyOfAsUnion keeps an anyOf combined with other keywords as an interface")
    public void testAnyOfCombinedWithOtherKeywordsStaysAnInterface() throws Exception {
        for (boolean separateModelsAndApi : new boolean[]{true, false}) {
            final Path output = generateAnyOfUnionSample(true, separateModelsAndApi);
            final Path[] files = separateModelsAndApi
                    ? new Path[]{output.resolve("models/all-of-with-any-of.ts"), output.resolve("models/one-of-with-any-of.ts"),
                    output.resolve("models/dictionary-with-any-of.ts"), output.resolve("models/required-with-any-of.ts")}
                    : new Path[]{output.resolve("api.ts")};
            for (Path file : files) {
                // allOf or oneOf members would be dropped, and so would additionalProperties and required
                TestUtils.assertFileNotContains(file,
                        "export type AllOfWithAnyOf",
                        "export type OneOfWithAnyOf",
                        "export type DictionaryWithAnyOf",
                        "export type RequiredWithAnyOf");
            }
        }
        final Path models = generateAnyOfUnionSample(true, true).resolve("models");
        TestUtils.assertFileContains(models.resolve("all-of-with-any-of.ts"), "export interface AllOfWithAnyOf {");
        TestUtils.assertFileContains(models.resolve("dictionary-with-any-of.ts"),
                "export interface DictionaryWithAnyOf {",
                "[key: string]: string | any;");
        TestUtils.assertFileContains(models.resolve("required-with-any-of.ts"),
                "export interface RequiredWithAnyOf {",
                "'message': string;");
        TestUtils.assertFileContains(models.resolve("one-of-with-any-of.ts"), "export interface OneOfWithAnyOf {");
        // models in api.ts: the schemas must be present as interfaces, not only absent as types
        TestUtils.assertFileContains(generateAnyOfUnionSample(true, false).resolve("api.ts"),
                "export interface AllOfWithAnyOf {",
                "export interface OneOfWithAnyOf {",
                "export interface DictionaryWithAnyOf {",
                "export interface RequiredWithAnyOf {");
    }

    private Path generateAnyOfUnionSample(Boolean generateAnyOfAsUnion, boolean separateModelsAndApi) throws Exception {
        final File output = Files.createTempDirectory("typescript_axios_any_of_union_").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("typescript-axios")
                .setInputSpec("src/test/resources/3_0/typescript-axios/any-of-union.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));
        if (generateAnyOfAsUnion != null) {
            configurator.addAdditionalProperty(TypeScriptAxiosClientCodegen.GENERATE_ANY_OF_AS_UNION, generateAnyOfAsUnion);
        }
        if (separateModelsAndApi) {
            configurator.addAdditionalProperty(TypeScriptAxiosClientCodegen.SEPARATE_MODELS_AND_API, true)
                    .addAdditionalProperty(CodegenConstants.MODEL_PACKAGE, "models")
                    .addAdditionalProperty(CodegenConstants.API_PACKAGE, "api");
        }

        final List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);
        return output.toPath();
    }
}
