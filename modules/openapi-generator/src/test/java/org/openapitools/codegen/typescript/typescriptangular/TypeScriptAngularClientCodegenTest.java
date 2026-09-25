package org.openapitools.codegen.typescript.typescriptangular;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.media.ComposedSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.responses.ApiResponses;
import org.openapitools.codegen.*;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.TypeScriptAngularClientCodegen;
import org.openapitools.codegen.typescript.TypeScriptGroups;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;


@Test(groups = {TypeScriptGroups.TYPESCRIPT, TypeScriptGroups.TYPESCRIPT_ANGULAR})
public class TypeScriptAngularClientCodegenTest {
    @Test
    public void toVarName() {
        TypeScriptAngularClientCodegen codegen = new TypeScriptAngularClientCodegen();
        codegen.processOpts();
        Assert.assertEquals(codegen.toVarName("valid_var"), "valid_var");

        codegen = new TypeScriptAngularClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.MODEL_PROPERTY_NAMING, "camelCase");
        codegen.processOpts();
        Assert.assertEquals(codegen.toVarName("valid_var"), "validVar");
    }

    @Test
    public void toVarNameWithAtSign() {
        TypeScriptAngularClientCodegen codegen = new TypeScriptAngularClientCodegen();
        codegen.processOpts();
        Assert.assertEquals(codegen.toVarName("@id"), "at_id");

        codegen = new TypeScriptAngularClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.MODEL_PROPERTY_NAMING, "camelCase");
        codegen.processOpts();
        Assert.assertEquals(codegen.toVarName("@id"), "atId");
    }

    @Test
    public void toEnumVarName() {
        TypeScriptAngularClientCodegen codegen = new TypeScriptAngularClientCodegen();
        // unspecified option should default to PascalCase
        codegen.processOpts();
        Assert.assertEquals(codegen.toEnumVarName("valid_id", "string"), "ValidId");
        Assert.assertEquals(codegen.toEnumVarName("illegal-id+", "string"), "IllegalId");

        codegen = new TypeScriptAngularClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.ENUM_PROPERTY_NAMING, CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.original.name());
        codegen.processOpts();
        Assert.assertEquals(codegen.toEnumVarName("valid_id", "string"), "valid_id");
        Assert.assertEquals(codegen.toEnumVarName("illegal-id+", "string"), "illegal_id");

        codegen = new TypeScriptAngularClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.ENUM_PROPERTY_NAMING, CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.UPPERCASE.name());
        codegen.processOpts();
        Assert.assertEquals(codegen.toEnumVarName("valid_id", "string"), "VALID_ID");
        Assert.assertEquals(codegen.toEnumVarName("illegal-id+", "string"), "ILLEGAL_ID");

        codegen = new TypeScriptAngularClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.ENUM_PROPERTY_NAMING, CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.snake_case.name());
        codegen.processOpts();
        Assert.assertEquals(codegen.toEnumVarName("valid_ID", "string"), "valid_id");
        Assert.assertEquals(codegen.toEnumVarName("Illegal-Id+", "string"), "illegal_id");
    }

    @Test
    public void testModelSuffix() {
        TypeScriptAngularClientCodegen codegen = new TypeScriptAngularClientCodegen();
        codegen.additionalProperties().put("modelSuffix", "MySuffix");
        codegen.processOpts();

        Assert.assertEquals(codegen.toModelName("TestName"), "TestNameMySuffix");
        Assert.assertEquals(codegen.toModelName("Error"), "ErrorMySuffix");
    }

    @Test
    public void testToEnumName() {
        TypeScriptAngularClientCodegen codegen = new TypeScriptAngularClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.ENUM_NAME_SUFFIX, "Enum");
        codegen.processOpts();

        Assert.assertEquals(codegen.toEnumName(makeEnumProperty("TestName")), "TestNameEnum");
        Assert.assertEquals(codegen.toEnumName(makeEnumProperty("123")), "_123Enum");

        // enum value should not use model suffix
        codegen = new TypeScriptAngularClientCodegen();
        codegen.additionalProperties().put(TypeScriptAngularClientCodegen.MODEL_SUFFIX, "Model");
        codegen.additionalProperties().put(CodegenConstants.ENUM_NAME_SUFFIX, "Enum2");
        codegen.processOpts();
        Assert.assertEquals(codegen.toEnumName(makeEnumProperty("TestName")), "TestNameEnum2");

        codegen = new TypeScriptAngularClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.ENUM_NAME_SUFFIX, "");
        codegen.processOpts();
        Assert.assertEquals(codegen.toEnumName(makeEnumProperty("TestName")), "TestName");
    }

    private CodegenProperty makeEnumProperty(String name) {
        CodegenProperty enumProperty = new CodegenProperty();
        enumProperty.name = name;
        return enumProperty;
    }

    @Test
    public void testModelFileSuffix() {
        TypeScriptAngularClientCodegen codegen = new TypeScriptAngularClientCodegen();
        codegen.additionalProperties().put("modelFileSuffix", "MySuffix");
        codegen.additionalProperties().put("modelSuffix", "MySuffix");
        codegen.processOpts();

        Assert.assertEquals("./testNameMySuffix", codegen.toModelFilename("testName"));
    }

    @Test
    public void testOperationIdParser() {
        OpenAPI openAPI = TestUtils.createOpenAPI();
        Operation operation1 = new Operation().operationId("123_test_@#$%_special_tags").responses(new ApiResponses().addApiResponse("201", new ApiResponse().description("OK")));
        openAPI.path("another-fake/dummy/", new PathItem().get(operation1));
        final TypeScriptAngularClientCodegen codegen = new TypeScriptAngularClientCodegen();
        codegen.setOpenAPI(openAPI);

        CodegenOperation co1 = codegen.fromOperation("/another-fake/dummy/", "get", operation1, null);
        org.testng.Assert.assertEquals(co1.operationId, "_123testSpecialTags");

    }

    @Test
    public void testSnapshotVersion() {
        OpenAPI openAPI = TestUtils.createOpenAPI();

        TypeScriptAngularClientCodegen codegen = new TypeScriptAngularClientCodegen();
        codegen.additionalProperties().put("npmName", "@openapi/typescript-angular-petstore");
        codegen.additionalProperties().put("snapshot", true);
        codegen.additionalProperties().put("npmVersion", "1.0.0-SNAPSHOT");
        codegen.processOpts();
        codegen.preprocessOpenAPI(openAPI);

        Assert.assertTrue(codegen.getNpmVersion().matches("^1.0.0-SNAPSHOT.[0-9]{12}$"));

        codegen = new TypeScriptAngularClientCodegen();
        codegen.additionalProperties().put("npmName", "@openapi/typescript-angular-petstore");
        codegen.additionalProperties().put("snapshot", true);
        codegen.additionalProperties().put("npmVersion", "3.0.0-M1");
        codegen.processOpts();
        codegen.preprocessOpenAPI(openAPI);

        Assert.assertTrue(codegen.getNpmVersion().matches("^3.0.0-M1-SNAPSHOT.[0-9]{12}$"));

    }

    @Test
    public void testWithoutSnapshotVersion() {
        OpenAPI openAPI = TestUtils.createOpenAPI();

        TypeScriptAngularClientCodegen codegen = new TypeScriptAngularClientCodegen();
        codegen.additionalProperties().put("npmName", "@openapi/typescript-angular-petstore");
        codegen.additionalProperties().put("snapshot", false);
        codegen.additionalProperties().put("npmVersion", "1.0.0-SNAPSHOT");
        codegen.processOpts();
        codegen.preprocessOpenAPI(openAPI);

        Assert.assertTrue(codegen.getNpmVersion().matches("^1.0.0-SNAPSHOT$"));

        codegen = new TypeScriptAngularClientCodegen();
        codegen.additionalProperties().put("npmName", "@openapi/typescript-angular-petstore");
        codegen.additionalProperties().put("snapshot", false);
        codegen.additionalProperties().put("npmVersion", "3.0.0-M1");
        codegen.processOpts();
        codegen.preprocessOpenAPI(openAPI);

        Assert.assertTrue(codegen.getNpmVersion().matches("^3.0.0-M1$"));

    }

    @Test
    public void testRemovePrefixSuffix() {
        TypeScriptAngularClientCodegen codegen = new TypeScriptAngularClientCodegen();

        // simple noop test
        Assert.assertEquals("TestName", codegen.removeModelPrefixSuffix("TestName"));

        codegen.setModelNamePrefix("abc");
        codegen.setModelNameSuffix("def");
        codegen.additionalProperties().put("modelSuffix", "Ghi");
        codegen.processOpts();

        Assert.assertEquals("TestName", codegen.removeModelPrefixSuffix("TestName"));
        Assert.assertEquals("TestName", codegen.removeModelPrefixSuffix("TestNameGhi"));
        Assert.assertEquals("TestNameghi", codegen.removeModelPrefixSuffix("TestNameghi"));
        Assert.assertEquals("abcTestName", codegen.removeModelPrefixSuffix("abcTestName"));
        Assert.assertEquals("TestName", codegen.removeModelPrefixSuffix("AbcTestName"));
        Assert.assertEquals("AbcTestName", codegen.removeModelPrefixSuffix("AbcAbcTestName"));
        Assert.assertEquals("TestName", codegen.removeModelPrefixSuffix("TestNameDef"));
        Assert.assertEquals("TestNamedef", codegen.removeModelPrefixSuffix("TestNamedef"));
        Assert.assertEquals("TestNamedefghi", codegen.removeModelPrefixSuffix("TestNamedefghi"));
        Assert.assertEquals("TestNameDefghi", codegen.removeModelPrefixSuffix("TestNameDefghi"));
        Assert.assertEquals("TestName", codegen.removeModelPrefixSuffix("TestNameDefGhi"));
    }

    @Test
    public void testSchema() {
        TypeScriptAngularClientCodegen codegen = new TypeScriptAngularClientCodegen();

        ComposedSchema composedSchema = new ComposedSchema();

        Schema<Object> schema1 = new Schema<>();
        schema1.set$ref("SchemaOne");
        Schema<Object> schema2 = new Schema<>();
        schema2.set$ref("SchemaTwo");
        Schema<Object> schema3 = new Schema<>();
        schema3.set$ref("SchemaThree");

        composedSchema.addAnyOfItem(schema1);
        composedSchema.addAnyOfItem(schema2);
        composedSchema.addAnyOfItem(schema3);

        String schemaType = codegen.getSchemaType(composedSchema);
        Assert.assertEquals(schemaType, "SchemaOne | SchemaTwo | SchemaThree");
    }

    @Test
    public void testKebabCasedModelFilenames() {
        TypeScriptAngularClientCodegen codegen = new TypeScriptAngularClientCodegen();
        codegen.additionalProperties().put(TypeScriptAngularClientCodegen.FILE_NAMING, "kebab-case");
        codegen.processOpts();

        final String modelName = "FooResponse__links";
        final Schema schema = new Schema()
                .name(modelName)
                .description("an inline model with name previously prefixed with underscore")
                .addRequiredItem("self")
                .addProperty("self", new StringSchema());

        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("test", schema);
        codegen.setOpenAPI(openAPI);

        Assert.assertEquals(codegen.toModelImport(modelName), "../model/foo-response-links");
        Assert.assertEquals(codegen.toModelFilename(modelName), "./foo-response-links");
    }

    @Test
    public void testToParamNaming() {
        TypeScriptAngularClientCodegen codegen = new TypeScriptAngularClientCodegen();
        // unspecified option should default to camelcase
        codegen.processOpts();
        Assert.assertEquals(codegen.toParamName("valid_id"), "validId");
        Assert.assertEquals(codegen.toParamName("illegal-id+"), "illegalId");

        codegen = new TypeScriptAngularClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.PARAM_NAMING, CodegenConstants.PARAM_NAMING_TYPE.original.name());
        codegen.processOpts();
        Assert.assertEquals(codegen.toParamName("valid_id"), "valid_id");
        Assert.assertEquals(codegen.toParamName("illegal-id+"), "illegal_id");

        codegen = new TypeScriptAngularClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.PARAM_NAMING, CodegenConstants.PARAM_NAMING_TYPE.snake_case.name());
        codegen.processOpts();
        Assert.assertEquals(codegen.toParamName("valid_ID"), "valid_id");
        Assert.assertEquals(codegen.toParamName("Illegal-Id+"), "illegal_id");

        codegen = new TypeScriptAngularClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.PARAM_NAMING, CodegenConstants.PARAM_NAMING_TYPE.PascalCase.name());
        codegen.processOpts();
        Assert.assertEquals(codegen.toParamName("valid_id"), "ValidId");
        Assert.assertEquals(codegen.toParamName("illegal-id+"), "IllegalId");
    }

    @Test
    public void testCorrectlyProducesImportsWithImportMapping() {
        TypeScriptAngularClientCodegen codegen = new TypeScriptAngularClientCodegen();
        final String importedModel = "SharedApiModel";
        final String importName = "@lib/custom/model";
        codegen.importMapping().put(importedModel, importName);
        Assert.assertEquals(codegen.toModelImport(importedModel), importName);
    }

    @Test
    public void testTaggedUnionImports() throws Exception {
        final String specPath = "src/test/resources/3_0/allOf_composition_discriminator_recursive.yaml";

        Map<String, Object> properties = new HashMap<>();
        properties.put(TypeScriptAngularClientCodegen.TAGGED_UNIONS, "true");

        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("typescript-angular")
                .setInputSpec(specPath)
                .setAdditionalProperties(properties)
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();

        Generator generator = new DefaultGenerator();
        generator.opts(clientOptInput).generate();

        TestUtils.assertFileContains(
                Paths.get(output + "/model/expressionToken.ts"),
                "import { Token } from './token'", // imports the parent schema
                "import { TokenMetadata } from './tokenMetadata'", // imports a schema referenced in an inherited property
                "export interface ExpressionToken {" // no inheritance
        );

        TestUtils.assertFileNotContains(
                Paths.get(output + "/model/stringToken.ts"),
                "import { Token } from './token'"
        );

        TestUtils.assertFileContains(
                Paths.get(output + "/model/token.ts"),
                "import { ExpressionToken } from './expressionToken'",
                "export type Token = ExpressionToken | StringToken"
        );
    }

    @Test
    public void testModelNameMappings() throws Exception {
        final String specPath = "src/test/resources/2_0/issue_8289.json";

        Map<String, Object> properties = new HashMap<>();
        properties.put(TypeScriptAngularClientCodegen.TAGGED_UNIONS, "true");

        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        Map<String, String> modelNames = new HashMap<>();
        modelNames.put("File", "SystemFile");

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setModelNameMappings(modelNames)
                .setGeneratorName("typescript-angular")
                .setInputSpec(specPath)
                .setAdditionalProperties(properties)
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();

        Generator generator = new DefaultGenerator();
        generator.opts(clientOptInput).generate();

        TestUtils.assertFileContains(
                Paths.get(output + "/model/folder.ts"),
                "files?: Array<SystemFile>;" // ensure it's an array of SystemFile (not Any)
        );
    }

    @Test
    public void testAngularDependenciesFromCliOptions() {
        // GIVEN
        OpenAPI openAPI = TestUtils.createOpenAPI();

        TypeScriptAngularClientCodegen codegen = new TypeScriptAngularClientCodegen();
        codegen.additionalProperties().put("npmName", "@openapi/typescript-angular-petstore");
        codegen.additionalProperties().put("tsVersion", "12");
        codegen.additionalProperties().put("rxjsVersion", "23");
        codegen.additionalProperties().put("ngPackagrVersion", "34");
        codegen.additionalProperties().put("zonejsVersion", "45");

        // WHEN
        codegen.processOpts();
        codegen.preprocessOpenAPI(openAPI);

        // THEN
        assertThat(codegen.additionalProperties()).containsEntry("tsVersion", "12");
        assertThat(codegen.additionalProperties()).containsEntry("rxjsVersion", "23");
        assertThat(codegen.additionalProperties()).containsEntry("ngPackagrVersion", "34");
        assertThat(codegen.additionalProperties()).containsEntry("zonejsVersion", "45");
    }

    @Test
    public void testAngularDependenciesFromConfigFile() {
        // GIVEN
        OpenAPI openAPI = TestUtils.createOpenAPI();

        TypeScriptAngularClientCodegen codegen = new TypeScriptAngularClientCodegen();
        codegen.additionalProperties().put("npmName", "@openapi/typescript-angular-petstore");
        // We fix ngVersion to do not update this test on every new angular release.
        codegen.additionalProperties().put("ngVersion", "19.0.0");

        // WHEN
        codegen.processOpts();
        codegen.preprocessOpenAPI(openAPI);

        // THEN
        assertThat(codegen.additionalProperties()).containsEntry("tsVersion", ">=5.5.0 <5.7.0");
        assertThat(codegen.additionalProperties()).containsEntry("rxjsVersion", "7.4.0");
        assertThat(codegen.additionalProperties()).containsEntry("ngPackagrVersion", "19.0.0");
        assertThat(codegen.additionalProperties()).containsEntry("zonejsVersion", "0.15.0");
    }

    @Test
    public void testNoDuplicateAuthentication() throws IOException {
        // GIVEN
        final String specPath = "src/test/resources/3_0/spring/petstore-auth.yaml";

        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        // WHEN
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("typescript-angular")
                .setInputSpec(specPath)
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();

        Generator generator = new DefaultGenerator();
        generator.opts(clientOptInput).generate();

        // THEN
        final String fileContents = Files.readString(Paths.get(output + "/api/default.service.ts"));
        assertThat(fileContents).containsOnlyOnce("localVarHeaders = this.configuration.addCredentialToHeaders('OAuth2', 'Authorization', localVarHeaders, 'Bearer ');");
    }

    @Test
    public void testBasePath() throws IOException {
        // GIVEN
        final String specPath = "src/test/resources/3_0/typescript-angular/issue_20760.yaml";

        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        // WHEN
        final CodegenConfigurator configurator = new CodegenConfigurator()
            .setGeneratorName("typescript-angular")
            .setInputSpec(specPath)
            .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();

        Generator generator = new DefaultGenerator();
        generator.opts(clientOptInput).generate();

        // THEN
        final String fileContents = Files.readString(Paths.get(output + "/api.base.service.ts"));
        assertThat(fileContents).containsOnlyOnce("basePath = '/relative/url'");
    }

    @Test
    public void testEnumAsConst() throws IOException {
        // GIVEN
        final String specPath = "src/test/resources/3_0/enum.yaml";

        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        // WHEN
        final CodegenConfigurator configurator = new CodegenConfigurator()
            .setGeneratorName("typescript-angular")
            .setInputSpec(specPath)
            .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();

        Generator generator = new DefaultGenerator();
        generator.opts(clientOptInput).generate();

        // THEN
        final String fileContents = Files.readString(Paths.get(output + "/model/type.ts"));
        assertThat(fileContents).containsOnlyOnce("} as const;");
        assertThat(fileContents).doesNotContain(" as Type");
    }

    @Test
    public void testDeepObject() throws IOException {
        // GIVEN
        final String specPath = "src/test/resources/3_0/deepobject.yaml";

        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        // WHEN
        final CodegenConfigurator configurator = new CodegenConfigurator()
            .setGeneratorName("typescript-angular")
            .setInputSpec(specPath)
            .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();

        Generator generator = new DefaultGenerator();
        generator.opts(clientOptInput).generate();

        // THEN
        final String fileContents = Files.readString(Paths.get(output + "/api/default.service.ts"));
        assertThat(fileContents).containsSubsequence("'options',\n", "<any>options,\n", "QueryParamStyle.DeepObject,\n", "true,\n");
        assertThat(fileContents).containsSubsequence("'inputOptions',\n", "<any>inputOptions,\n", "QueryParamStyle.DeepObject,\n", "true,\n");
    }

    @Test
    public void testOpenIdCredentialsAreSet() throws IOException {
        // GIVEN
        final String specPath = "src/test/resources/3_1/issue_21245.yaml";

        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        // WHEN
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("typescript-angular")
                .setInputSpec(specPath)
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();

        Generator generator = new DefaultGenerator();
        generator.opts(clientOptInput).generate();

        //THEN
        final String fileContents = Files.readString(Paths.get(output + "/api/default.service.ts"));
        String credentialsSet = "localVarHeaders = this.configuration.addCredentialToHeaders('oidc', 'Authorization', localVarHeaders, 'Bearer ');";
        assertThat(fileContents).contains(credentialsSet);
    }

    private static final String HTTP_RESOURCE_SPEC = "src/test/resources/3_0/typescript-angular/http-resource.yaml";

    private File generateAngular(String specPath, Map<String, Object> properties) throws IOException {
        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("typescript-angular")
                .setInputSpec(specPath)
                .setAdditionalProperties(properties)
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        return output;
    }

    @Test
    public void testHttpResourceIsOffByDefault() throws IOException {
        File output = generateAngular(HTTP_RESOURCE_SPEC, new HashMap<>());

        TestUtils.assertFileNotContains(Paths.get(output + "/api/pet.service.ts"),
                "HttpResourceRef", "httpResource<", "httpResource.", "Resource(", "RequestParams");
        TestUtils.assertFileNotContains(Paths.get(output + "/index.ts"), "http.resource.options");
        TestUtils.assertFileNotExists(Paths.get(output + "/http.resource.options.ts"));
    }

    @Test
    public void testHttpResourceOnlyForGetOperationsWithoutBody() throws IOException {
        Map<String, Object> properties = new HashMap<>();
        properties.put(TypeScriptAngularClientCodegen.WITH_HTTP_RESOURCE, "true");
        File output = generateAngular(HTTP_RESOURCE_SPEC, properties);

        Path petService = Paths.get(output + "/api/pet.service.ts");
        TestUtils.assertFileContains(petService,
                "import { HttpResourceRef, HttpResourceRequest, httpResource } from '@angular/common/http';",
                "import { ApiHttpResourceOptions } from '../http.resource.options';",
                "export interface GetPetByIdRequestParams {",
                "public getPetByIdResource(params: () => GetPetByIdRequestParams | undefined, options?: ApiHttpResourceOptions<Pet>): HttpResourceRef<Pet | undefined>;",
                "public getPetByIdResource(params: () => GetPetByIdRequestParams | undefined, options: ApiHttpResourceOptions<Pet> & { defaultValue: Pet }): HttpResourceRef<Pet>;",
                // every parameter optional: the params function may be left out
                "public listPetsResource(params?: () => ListPetsRequestParams | undefined, options?: ApiHttpResourceOptions<Array<Pet>>): HttpResourceRef<Array<Pet> | undefined>;",
                // a missing required parameter keeps the resource idle instead of throwing
                "if (petId === null || petId === undefined) { return undefined; }",
                "params: localVarQueryParameters.toHttpParams(),",
                "transferCache: localVarTransferCache ?? true,");
        // the Observable methods are unchanged, and writes and a GET with a body get no resource
        TestUtils.assertFileContains(petService,
                "public addPet(pet: Pet, observe?: 'body'",
                "public searchPets(pet?: Pet, observe?: 'body'");
        TestUtils.assertFileNotContains(petService,
                "addPetResource", "deletePetResource", "searchPetsResource",
                "export interface AddPetRequestParams", "export interface SearchPetsRequestParams");

        // an operation without parameters takes only the options
        TestUtils.assertFileContains(Paths.get(output + "/api/store.service.ts"),
                "public getInventoryResource(options?: ApiHttpResourceOptions<{ [key: string]: number; }>)",
                "localVarQueryParameters = this.configuration.addCredentialToQuery('api_key_query', 'api_key', localVarQueryParameters);");

        TestUtils.assertFileContains(Paths.get(output + "/http.resource.options.ts"),
                "export interface ApiHttpResourceOptions<T> {");
        TestUtils.assertFileContains(Paths.get(output + "/index.ts"),
                "export * from './http.resource.options';");
    }

    @Test
    public void testHttpResourceFactoryFollowsTheAcceptHeader() throws IOException {
        Map<String, Object> properties = new HashMap<>();
        properties.put(TypeScriptAngularClientCodegen.WITH_HTTP_RESOURCE, true);
        File output = generateAngular(HTTP_RESOURCE_SPEC, properties);

        Path petService = Paths.get(output + "/api/pet.service.ts");
        TestUtils.assertFileContains(petService,
                // application/xml and application/json: the JSON type wins, as in Configuration.selectHeaderAccept
                "return httpResource<Array<Pet>>(",
                "localVarHeaders = localVarHeaders.set('Accept', 'application/json');",
                "return httpResource<Pet>(",
                // text/plain only
                "return httpResource.text<string>(",
                "localVarHeaders = localVarHeaders.set('Accept', 'text/plain');",
                // binary
                "return httpResource.blob<Blob>(",
                "localVarHeaders = localVarHeaders.set('Accept', 'application/octet-stream');");
    }

    @Test
    public void testHttpResourceRequiresAngular20() {
        TypeScriptAngularClientCodegen codegen = new TypeScriptAngularClientCodegen();
        codegen.additionalProperties().put(TypeScriptAngularClientCodegen.NG_VERSION, "19.2.0");
        codegen.additionalProperties().put(TypeScriptAngularClientCodegen.WITH_HTTP_RESOURCE, "true");
        IllegalArgumentException error = Assert.expectThrows(IllegalArgumentException.class, codegen::processOpts);
        assertThat(error.getMessage()).contains("withHttpResource requires Angular v20+");

        codegen = new TypeScriptAngularClientCodegen();
        codegen.additionalProperties().put(TypeScriptAngularClientCodegen.NG_VERSION, "20.0.0");
        codegen.additionalProperties().put(TypeScriptAngularClientCodegen.WITH_HTTP_RESOURCE, "true");
        codegen.processOpts();
        assertThat(codegen.additionalProperties()).containsEntry(TypeScriptAngularClientCodegen.WITH_HTTP_RESOURCE, true);

        // without the option, older versions are not affected
        codegen = new TypeScriptAngularClientCodegen();
        codegen.additionalProperties().put(TypeScriptAngularClientCodegen.NG_VERSION, "19.2.0");
        codegen.processOpts();
        assertThat(codegen.additionalProperties()).containsEntry(TypeScriptAngularClientCodegen.WITH_HTTP_RESOURCE, false);
    }

    @Test
    public void testHttpResourceSkipsAnOperationWhoseNameIsTaken() throws IOException {
        Path spec = Files.createTempFile("http-resource-collision", ".yaml");
        spec.toFile().deleteOnExit();
        Files.writeString(spec, String.join("\n",
                "openapi: 3.0.1",
                "info: {title: collision, version: 1.0.0}",
                "paths:",
                "  /pet:",
                "    get:",
                "      operationId: getPet",
                "      responses: {'200': {description: ok, content: {application/json: {schema: {type: string}}}}}",
                "  /pet-resource:",
                "    get:",
                "      operationId: getPetResource",
                "      responses: {'200': {description: ok, content: {application/json: {schema: {type: string}}}}}",
                ""));

        Map<String, Object> properties = new HashMap<>();
        properties.put(TypeScriptAngularClientCodegen.WITH_HTTP_RESOURCE, "true");
        File output = generateAngular(spec.toString(), properties);

        Path service = Paths.get(output + "/api/default.service.ts");
        TestUtils.assertFileContains(service, "public getPetResourceResource(options?: ApiHttpResourceOptions<string>)");
        TestUtils.assertFileNotContains(service, "public getPetResource(options?: ApiHttpResourceOptions<string>)");
    }
}
