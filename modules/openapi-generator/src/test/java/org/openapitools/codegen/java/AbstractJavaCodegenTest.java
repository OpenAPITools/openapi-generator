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

package org.openapitools.codegen.java;

import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.*;

import java.time.LocalDate;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Date;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.AbstractJavaCodegen;
import org.openapitools.codegen.utils.ModelUtils;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;

public class AbstractJavaCodegenTest {

    private final AbstractJavaCodegen fakeJavaCodegen = new P_AbstractJavaCodegen();

    @Test
    public void toEnumVarNameShouldNotShortenUnderScore() throws Exception {
        Assert.assertEquals("UNDERSCORE", fakeJavaCodegen.toEnumVarName("_", "String"));
        Assert.assertEquals("__", fakeJavaCodegen.toEnumVarName("__", "String"));
        Assert.assertEquals("__", fakeJavaCodegen.toEnumVarName("_,.", "String"));
    }

    @Test
    public void toVarNameShouldAvoidOverloadingGetClassMethod() throws Exception {
        Assert.assertEquals("propertyClass", fakeJavaCodegen.toVarName("class"));
        Assert.assertEquals("propertyClass", fakeJavaCodegen.toVarName("_class"));
        Assert.assertEquals("propertyClass", fakeJavaCodegen.toVarName("__class"));
    }

    @Test
    public void toModelNameShouldUseProvidedMapping() throws Exception {
        fakeJavaCodegen.importMapping().put("json_myclass", "com.test.MyClass");
        Assert.assertEquals("com.test.MyClass", fakeJavaCodegen.toModelName("json_myclass"));
    }

    @Test
    public void toModelNameUsesPascalCase() throws Exception {
        Assert.assertEquals("JsonAnotherclass", fakeJavaCodegen.toModelName("json_anotherclass"));
    }

    @Test
    public void testPreprocessOpenAPI() throws Exception {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/petstore.yaml");
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();

        codegen.preprocessOpenAPI(openAPI);

        Assert.assertEquals(codegen.getArtifactVersion(), openAPI.getInfo().getVersion());
        Assert.assertEquals(openAPI.getPaths().get("/pet").getPost().getExtensions().get("x-accepts"), "application/json");
    }

    @Test
    public void testPreprocessOpenAPINumVersion() throws Exception {
        final OpenAPI openAPIOtherNumVersion = TestUtils.parseFlattenSpec("src/test/resources/2_0/duplicateOperationIds.yaml");
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();

        codegen.preprocessOpenAPI(openAPIOtherNumVersion);

        Assert.assertEquals(codegen.getArtifactVersion(), openAPIOtherNumVersion.getInfo().getVersion());
    }

    @Test
    public void convertVarName() throws Exception {
        Assert.assertEquals(fakeJavaCodegen.toVarName("name"), "name");
        Assert.assertEquals(fakeJavaCodegen.toVarName("$name"), "$name");
        Assert.assertEquals(fakeJavaCodegen.toVarName("nam$$e"), "nam$$e");
        Assert.assertEquals(fakeJavaCodegen.toVarName("user-name"), "userName");
        Assert.assertEquals(fakeJavaCodegen.toVarName("user_name"), "userName");
        Assert.assertEquals(fakeJavaCodegen.toVarName("user|name"), "userName");
        Assert.assertEquals(fakeJavaCodegen.toVarName("uSername"), "uSername");
        Assert.assertEquals(fakeJavaCodegen.toVarName("USERname"), "usERname");
        Assert.assertEquals(fakeJavaCodegen.toVarName("USERNAME"), "USERNAME");
        Assert.assertEquals(fakeJavaCodegen.toVarName("USER123NAME"), "USER123NAME");
        Assert.assertEquals(fakeJavaCodegen.toVarName("1"), "_1");
        Assert.assertEquals(fakeJavaCodegen.toVarName("1a"), "_1a");
        Assert.assertEquals(fakeJavaCodegen.toVarName("1A"), "_1A");
        Assert.assertEquals(fakeJavaCodegen.toVarName("1AAAA"), "_1AAAA");
        Assert.assertEquals(fakeJavaCodegen.toVarName("1AAaa"), "_1aAaa");
    }

    @Test
    public void convertModelName() throws Exception {
        Assert.assertEquals(fakeJavaCodegen.toModelName("name"), "Name");
        Assert.assertEquals(fakeJavaCodegen.toModelName("$name"), "Name");
        Assert.assertEquals(fakeJavaCodegen.toModelName("nam#e"), "Name");
        Assert.assertEquals(fakeJavaCodegen.toModelName("$another-fake?"), "AnotherFake");
        Assert.assertEquals(fakeJavaCodegen.toModelName("1a"), "Model1a");
        Assert.assertEquals(fakeJavaCodegen.toModelName("1A"), "Model1A");
        Assert.assertEquals(fakeJavaCodegen.toModelName("AAAb"), "AAAb");
        Assert.assertEquals(fakeJavaCodegen.toModelName("aBB"), "ABB");
        Assert.assertEquals(fakeJavaCodegen.toModelName("AaBBa"), "AaBBa");
        Assert.assertEquals(fakeJavaCodegen.toModelName("A_B"), "AB");
        Assert.assertEquals(fakeJavaCodegen.toModelName("A-B"), "AB");
        Assert.assertEquals(fakeJavaCodegen.toModelName("Aa_Bb"), "AaBb");
        Assert.assertEquals(fakeJavaCodegen.toModelName("Aa-Bb"), "AaBb");
        Assert.assertEquals(fakeJavaCodegen.toModelName("Aa_bb"), "AaBb");
        Assert.assertEquals(fakeJavaCodegen.toModelName("Aa-bb"), "AaBb");
    }

    @Test
    public void testInitialConfigValues() throws Exception {
        OpenAPI openAPI = TestUtils.createOpenAPI();

        final AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        codegen.processOpts();
        codegen.preprocessOpenAPI(openAPI);

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
        Assert.assertEquals(codegen.modelPackage(), "invalidPackageName");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "invalidPackageName");
        Assert.assertEquals(codegen.apiPackage(), "invalidPackageName");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "invalidPackageName");
        Assert.assertEquals(codegen.getInvokerPackage(), "org.openapitools");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "org.openapitools");
        Assert.assertEquals(codegen.additionalProperties().get(AbstractJavaCodegen.BOOLEAN_GETTER_PREFIX), "get");
        Assert.assertEquals(codegen.getArtifactVersion(), openAPI.getInfo().getVersion());
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.ARTIFACT_VERSION), openAPI.getInfo().getVersion());
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        OpenAPI openAPI = TestUtils.createOpenAPI();

        final AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();

        codegen.setHideGenerationTimestamp(true);
        codegen.setModelPackage("xyz.yyyyy.zzzzzzz.model");
        codegen.setApiPackage("xyz.yyyyy.zzzzzzz.api");
        codegen.setInvokerPackage("xyz.yyyyy.zzzzzzz.invoker");
        codegen.setBooleanGetterPrefix("is");
        codegen.setArtifactVersion("0.9.0-SNAPSHOT");

        codegen.processOpts();
        codegen.preprocessOpenAPI(openAPI);

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
        Assert.assertEquals(codegen.modelPackage(), "xyz.yyyyy.zzzzzzz.model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "xyz.yyyyy.zzzzzzz.model");
        Assert.assertEquals(codegen.apiPackage(), "xyz.yyyyy.zzzzzzz.api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "xyz.yyyyy.zzzzzzz.api");
        Assert.assertEquals(codegen.getInvokerPackage(), "xyz.yyyyy.zzzzzzz.invoker");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "xyz.yyyyy.zzzzzzz.invoker");
        Assert.assertEquals(codegen.additionalProperties().get(AbstractJavaCodegen.BOOLEAN_GETTER_PREFIX), "is");
        Assert.assertEquals(codegen.getArtifactVersion(), "0.9.0-SNAPSHOT");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.ARTIFACT_VERSION), "0.9.0-SNAPSHOT");
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        OpenAPI openAPI = TestUtils.createOpenAPI();

        final AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, false);
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "xyz.yyyyy.model.oooooo");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "xyz.yyyyy.api.oooooo");
        codegen.additionalProperties().put(CodegenConstants.INVOKER_PACKAGE, "xyz.yyyyy.invoker.oooooo");
        codegen.additionalProperties().put(AbstractJavaCodegen.BOOLEAN_GETTER_PREFIX, "getBoolean");
        codegen.additionalProperties().put(CodegenConstants.ARTIFACT_VERSION, "0.8.0-SNAPSHOT");
        codegen.processOpts();
        codegen.preprocessOpenAPI(openAPI);


        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
        Assert.assertEquals(codegen.modelPackage(), "xyz.yyyyy.model.oooooo");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "xyz.yyyyy.model.oooooo");
        Assert.assertEquals(codegen.apiPackage(), "xyz.yyyyy.api.oooooo");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "xyz.yyyyy.api.oooooo");
        Assert.assertEquals(codegen.getInvokerPackage(), "xyz.yyyyy.invoker.oooooo");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "xyz.yyyyy.invoker.oooooo");
        Assert.assertEquals(codegen.additionalProperties().get(AbstractJavaCodegen.BOOLEAN_GETTER_PREFIX), "getBoolean");
        Assert.assertEquals(codegen.getArtifactVersion(), "0.8.0-SNAPSHOT");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.ARTIFACT_VERSION), "0.8.0-SNAPSHOT");
    }

    @Test
    public void toEnumValue() {
        final AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        Assert.assertEquals(codegen.toEnumValue("1", "Integer"), "1");
        Assert.assertEquals(codegen.toEnumValue("42", "Double"), "42");
        Assert.assertEquals(codegen.toEnumValue("1337", "Long"), "1337l");
        Assert.assertEquals(codegen.toEnumValue("3.14", "Float"), "3.14f");
    }

    @Test
    public void apiFileFolder() {
        final AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        codegen.setOutputDir("/User/open.api.tools");
        codegen.setSourceFolder("source.folder");
        codegen.setApiPackage("org.openapitools.codegen.api");
        Assert.assertEquals(codegen.apiFileFolder(), "/User/open.api.tools/source.folder/org/openapitools/codegen/api".replace('/', File.separatorChar));
    }

    @Test
    public void apiTestFileFolder() {
        final AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        codegen.setOutputDir("/User/open.api.tools");
        codegen.setTestFolder("test.folder");
        codegen.setApiPackage("org.openapitools.codegen.api");
        Assert.assertEquals(codegen.apiTestFileFolder(), "/User/open.api.tools/test.folder/org/openapitools/codegen/api".replace('/', File.separatorChar));
    }

    @Test
    public void modelTestFileFolder() {
        final AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        codegen.setOutputDir("/User/open.api.tools");
        codegen.setTestFolder("test.folder");
        codegen.setModelPackage("org.openapitools.codegen.model");
        Assert.assertEquals(codegen.modelTestFileFolder(), "/User/open.api.tools/test.folder/org/openapitools/codegen/model".replace('/', File.separatorChar));
    }

    @Test
    public void modelFileFolder() {
        final AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        codegen.setOutputDir("/User/open.api.tools");
        codegen.setSourceFolder("source.folder");
        codegen.setModelPackage("org.openapitools.codegen.model");
        Assert.assertEquals(codegen.modelFileFolder(), "/User/open.api.tools/source.folder/org/openapitools/codegen/model".replace('/', File.separatorChar));
    }

    @Test
    public void apiDocFileFolder() {
        final AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        codegen.setOutputDir("/User/open.api.tools");
        Assert.assertEquals(codegen.apiDocFileFolder(), "/User/open.api.tools/docs/".replace('/', File.separatorChar));
    }

    @Test(description = "tests if API version specification is used if no version is provided in additional properties")
    public void openApiversionTest() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();

        OpenAPI api = TestUtils.createOpenAPI();
        codegen.processOpts();
        codegen.preprocessOpenAPI(api);

        Assert.assertEquals(codegen.getArtifactVersion(), "1.0.7");
    }

    @Test(description = "tests if API version specification is used if no version is provided in additional properties with snapshot version")
    public void openApiSnapShotVersionTest() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();

        codegen.additionalProperties().put("snapshotVersion", "true");

        OpenAPI api = TestUtils.createOpenAPI();
        codegen.processOpts();
        codegen.preprocessOpenAPI(api);

        Assert.assertEquals(codegen.getArtifactVersion(), "1.0.7-SNAPSHOT");
    }

    @Test(description = "tests if artifactVersion additional property is used")
    public void additionalPropertyArtifactVersionTest() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();

        codegen.additionalProperties().put("artifactVersion", "1.1.1");

        OpenAPI api = TestUtils.createOpenAPI();
        codegen.processOpts();
        codegen.preprocessOpenAPI(api);

        Assert.assertEquals(codegen.getArtifactVersion(), "1.1.1");
    }

    @Test(description = "tests if artifactVersion additional property is used with snapshot parameter")
    public void additionalPropertyArtifactSnapShotVersionTest() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();

        codegen.additionalProperties().put("artifactVersion", "1.1.1");
        codegen.additionalProperties().put("snapshotVersion", "true");

        OpenAPI api = TestUtils.createOpenAPI();
        codegen.processOpts();
        codegen.preprocessOpenAPI(api);

        Assert.assertEquals(codegen.getArtifactVersion(), "1.1.1-SNAPSHOT");
    }

    @Test(description = "tests if default version is used when neither OpenAPI version nor artifactVersion additional property has been provided")
    public void defaultVersionTest() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        codegen.setArtifactVersion(null);

        OpenAPI api = TestUtils.createOpenAPI();
        api.getInfo().setVersion(null);
        codegen.processOpts();
        codegen.preprocessOpenAPI(api);

        Assert.assertEquals(codegen.getArtifactVersion(), "1.0.0");
    }

    @Test(description = "tests if default version with snapshot is used when neither OpenAPI version nor artifactVersion additional property has been provided")
    public void snapshotVersionTest() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();

        codegen.additionalProperties().put("snapshotVersion", "true");

        OpenAPI api = TestUtils.createOpenAPI();
        api.getInfo().setVersion(null);
        codegen.processOpts();
        codegen.preprocessOpenAPI(api);

        Assert.assertEquals(codegen.getArtifactVersion(), "1.0.0-SNAPSHOT");
    }

    @Test(description = "tests if default version with snapshot is used when OpenAPI version has been provided")
    public void snapshotVersionOpenAPITest() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();

        codegen.additionalProperties().put(CodegenConstants.SNAPSHOT_VERSION, "true");

        OpenAPI api = TestUtils.createOpenAPI();
        api.getInfo().setVersion("2.0");
        codegen.processOpts();
        codegen.preprocessOpenAPI(api);

        Assert.assertEquals(codegen.getArtifactVersion(), "2.0-SNAPSHOT");
    }

    @Test(description = "tests if setting an artifact version programmatically persists to additional properties, when openapi version is null")
    public void allowsProgrammaticallySettingArtifactVersionWithNullOpenApiVersion() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        final String version = "9.8.7-rc1";
        codegen.setArtifactVersion(version);

        OpenAPI api = TestUtils.createOpenAPI();
        api.getInfo().setVersion(null);
        codegen.processOpts();
        codegen.preprocessOpenAPI(api);

        Assert.assertEquals(codegen.getArtifactVersion(), version);
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.ARTIFACT_VERSION), version);
    }

    @Test(description = "tests if setting an artifact version programmatically persists to additional properties, even when openapi version is specified")
    public void allowsProgrammaticallySettingArtifactVersionWithSpecifiedOpenApiVersion() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        final String version = "9.8.7-rc1";
        codegen.setArtifactVersion(version);

        OpenAPI api = TestUtils.createOpenAPI();
        api.getInfo().setVersion("1.2.3-SNAPSHOT");
        codegen.processOpts();
        codegen.preprocessOpenAPI(api);

        Assert.assertEquals(codegen.getArtifactVersion(), version);
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.ARTIFACT_VERSION), version);
    }

    @Test(description = "tests if a null in addition properties artifactVersion results in default version")
    public void usesDefaultVersionWhenAdditionalPropertiesVersionIsNull() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        final String version = "1.0.0";

        OpenAPI api = TestUtils.createOpenAPI();
        api.getInfo().setVersion(null);
        codegen.setArtifactVersion(null);
        codegen.additionalProperties().put(CodegenConstants.ARTIFACT_VERSION, null);

        codegen.processOpts();
        codegen.preprocessOpenAPI(api);

        Assert.assertEquals(codegen.getArtifactVersion(), version);
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.ARTIFACT_VERSION), version);
    }



    @Test(description = "tests if default version with snapshot is used when setArtifactVersion is used")
    public void snapshotVersionAlreadySnapshotTest() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();

        codegen.additionalProperties().put(CodegenConstants.SNAPSHOT_VERSION, "true");

        OpenAPI api = TestUtils.createOpenAPI();
        codegen.setArtifactVersion("4.1.2-SNAPSHOT");
        codegen.processOpts();
        codegen.preprocessOpenAPI(api);

        Assert.assertEquals(codegen.getArtifactVersion(), "4.1.2-SNAPSHOT");
    }

    @Test
    public void toDefaultValueTest() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();

        Schema<?> schema = createObjectSchemaWithMinItems();
        String defaultValue = codegen.toDefaultValue(schema);
        Assert.assertNull(defaultValue);
        DateSchema dateSchema = new DateSchema();
        LocalDate defaultLocalDate = LocalDate.of(2019,2,15);
        Date date = Date.from(defaultLocalDate.atStartOfDay(ZoneId.systemDefault()).toInstant());
        dateSchema.setDefault(date);
        defaultValue = codegen.toDefaultValue(dateSchema);
        Assert.assertEquals(defaultLocalDate, LocalDate.parse(defaultValue));
    }

    @Test
    public void getTypeDeclarationTest() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();

        Schema<?> schema = createObjectSchemaWithMinItems();
        String defaultValue = codegen.getTypeDeclaration(schema);
        Assert.assertEquals(defaultValue, "Object");

        // Create an alias to an array schema
        Schema<?> nestedArraySchema = new ArraySchema().items(new IntegerSchema().format("int32"));
        codegen.setOpenAPI(new OpenAPI().components(new Components().addSchemas("NestedArray", nestedArraySchema)));

        // Create an array schema with item type set to the array alias
        schema = new ArraySchema().items(new Schema().$ref("#/components/schemas/NestedArray"));

        ModelUtils.setGenerateAliasAsModel(false);
        defaultValue = codegen.getTypeDeclaration(schema);
        Assert.assertEquals(defaultValue, "List<List<Integer>>");

        ModelUtils.setGenerateAliasAsModel(true);
        defaultValue = codegen.getTypeDeclaration(schema);
        Assert.assertEquals(defaultValue, "List<NestedArray>");

        // Create an array schema with item type set to the array alias
        schema = new ArraySchema().items(new Schema().$ref("#/components/schemas/NestedArray"));
        schema.setUniqueItems(true);

        ModelUtils.setGenerateAliasAsModel(false);
        defaultValue = codegen.getTypeDeclaration(schema);
        Assert.assertEquals(defaultValue, "Set<List<Integer>>");

        ModelUtils.setGenerateAliasAsModel(true);
        defaultValue = codegen.getTypeDeclaration(schema);
        Assert.assertEquals(defaultValue, "Set<NestedArray>");

        // Create a map schema with additionalProperties type set to array alias
        schema = new MapSchema().additionalProperties(new Schema().$ref("#/components/schemas/NestedArray"));

        ModelUtils.setGenerateAliasAsModel(false);
        defaultValue = codegen.getTypeDeclaration(schema);
        Assert.assertEquals(defaultValue, "Map<String, List<Integer>>");

        ModelUtils.setGenerateAliasAsModel(true);
        defaultValue = codegen.getTypeDeclaration(schema);
        Assert.assertEquals(defaultValue, "Map<String, NestedArray>");
    }

    @Test
    public void processOptsBooleanTrueFromString() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/petstore.yaml");
        codegen.additionalProperties().put(CodegenConstants.SNAPSHOT_VERSION, "true");
        codegen.preprocessOpenAPI(openAPI);
        Assert.assertTrue((boolean) codegen.additionalProperties().get(CodegenConstants.SNAPSHOT_VERSION));
    }

    @Test
    public void processOptsBooleanTrueFromBoolean() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/petstore.yaml");
        codegen.additionalProperties().put(CodegenConstants.SNAPSHOT_VERSION, true);
        codegen.preprocessOpenAPI(openAPI);
        Assert.assertTrue((boolean) codegen.additionalProperties().get(CodegenConstants.SNAPSHOT_VERSION));
    }

    @Test
    public void processOptsBooleanFalseFromString() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/petstore.yaml");
        codegen.additionalProperties().put(CodegenConstants.SNAPSHOT_VERSION, "false");
        codegen.preprocessOpenAPI(openAPI);
        Assert.assertFalse((boolean) codegen.additionalProperties().get(CodegenConstants.SNAPSHOT_VERSION));
    }

    @Test
    public void processOptsBooleanFalseFromBoolean() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/petstore.yaml");
        codegen.additionalProperties().put(CodegenConstants.SNAPSHOT_VERSION, false);
        codegen.preprocessOpenAPI(openAPI);
        Assert.assertFalse((boolean) codegen.additionalProperties().get(CodegenConstants.SNAPSHOT_VERSION));
    }

    @Test
    public void processOptsBooleanFalseFromGarbage() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/petstore.yaml");
        codegen.additionalProperties().put(CodegenConstants.SNAPSHOT_VERSION, "blibb");
        codegen.preprocessOpenAPI(openAPI);
        Assert.assertFalse((boolean) codegen.additionalProperties().get(CodegenConstants.SNAPSHOT_VERSION));
    }

    @Test
    public void processOptsBooleanFalseFromNumeric() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/petstore.yaml");
        codegen.additionalProperties().put(CodegenConstants.SNAPSHOT_VERSION, 42L);
        codegen.preprocessOpenAPI(openAPI);
        Assert.assertFalse((boolean) codegen.additionalProperties().get(CodegenConstants.SNAPSHOT_VERSION));
    }

    private static Schema<?> createObjectSchemaWithMinItems() {
        return new ObjectSchema()
                .addProperties("id", new IntegerSchema().format("int32"))
                .minItems(1);
    }

    private static class P_AbstractJavaCodegen extends AbstractJavaCodegen {
        @Override
        public CodegenType getTag() {
            return null;
        }

        @Override
        public String getName() {
            return null;
        }

        @Override
        public String getHelp() {
            return null;
        }

        /**
         * Gets artifact version.
         * Only for testing purposes.
         *
         * @return version
         */
        public String getArtifactVersion() {
            return this.artifactVersion;
        }
    }
}
