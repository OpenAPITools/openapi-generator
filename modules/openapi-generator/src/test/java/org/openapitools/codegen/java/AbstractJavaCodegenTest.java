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

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.*;

import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.parser.core.models.ParseOptions;

import java.math.BigDecimal;
import java.time.OffsetDateTime;
import java.time.ZonedDateTime;
import java.util.*;

import java.util.stream.Collectors;
import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.AbstractJavaCodegen;
import org.openapitools.codegen.utils.ModelUtils;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.time.LocalDate;
import java.time.ZoneId;

public class AbstractJavaCodegenTest {

    private final AbstractJavaCodegen fakeJavaCodegen = new P_AbstractJavaCodegen();

    @Test
    public void toEnumVarNameShouldNotShortenUnderScore() throws Exception {
        Assertions.assertEquals(fakeJavaCodegen.toEnumVarName("_", "String"), "UNDERSCORE");
        Assertions.assertEquals(fakeJavaCodegen.toEnumVarName("__", "String"), "__");
        Assertions.assertEquals(fakeJavaCodegen.toEnumVarName("_,.", "String"), "__");
    }

    /**
     * As of Java 9, '_' is a keyword, and may not be used as an identifier.
     */
    @Test
    public void toEnumVarNameShouldNotResultInSingleUnderscore() throws Exception {
        Assertions.assertEquals(fakeJavaCodegen.toEnumVarName(" ", "String"), "SPACE");
        Assertions.assertEquals(fakeJavaCodegen.toEnumVarName("==", "String"), "u");
    }

    @Test
    public void toVarNameShouldAvoidOverloadingGetClassMethod() throws Exception {
        Assertions.assertEquals(fakeJavaCodegen.toVarName("class"), "propertyClass");
        Assertions.assertEquals(fakeJavaCodegen.toVarName("_class"), "propertyClass");
        Assertions.assertEquals(fakeJavaCodegen.toVarName("__class"), "propertyClass");
    }

    @Test
    public void toModelNameShouldNotUseProvidedMapping() throws Exception {
        fakeJavaCodegen.importMapping().put("json_myclass", "com.test.MyClass");
        Assertions.assertEquals(fakeJavaCodegen.toModelName("json_myclass"), "JsonMyclass");
    }

    @Test
    public void toModelNameUsesPascalCase() throws Exception {
        Assertions.assertEquals(fakeJavaCodegen.toModelName("json_anotherclass"), "JsonAnotherclass");
    }

    @Test
    public void testPreprocessOpenApiIncludeAllMediaTypesInAcceptHeader() throws Exception {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/petstore.yaml");
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        codegen.preprocessOpenAPI(openAPI);

        Assertions.assertEquals(codegen.getArtifactVersion(), openAPI.getInfo().getVersion());

        Object xAccepts = openAPI.getPaths().get("/pet").getPost().getExtensions().get("x-accepts");
        Assertions.assertTrue(xAccepts instanceof String[]);
        Assertions.assertTrue(List.of((String[]) xAccepts).containsAll(List.of("application/json", "application/xml")));
    }

    @Test
    public void testPreprocessOpenAPINumVersion() throws Exception {
        final OpenAPI openAPIOtherNumVersion = TestUtils.parseFlattenSpec("src/test/resources/2_0/duplicateOperationIds.yaml");
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();

        codegen.preprocessOpenAPI(openAPIOtherNumVersion);

        Assertions.assertEquals(codegen.getArtifactVersion(), openAPIOtherNumVersion.getInfo().getVersion());
    }

    @Test
    public void convertVarName() {
        Assertions.assertEquals(fakeJavaCodegen.toVarName("name"), "name");
        Assertions.assertEquals(fakeJavaCodegen.toVarName("$name"), "$name");
        Assertions.assertEquals(fakeJavaCodegen.toVarName("nam$$e"), "nam$$e");
        Assertions.assertEquals(fakeJavaCodegen.toVarName("user-name"), "userName");
        Assertions.assertEquals(fakeJavaCodegen.toVarName("user_name"), "userName");
        Assertions.assertEquals(fakeJavaCodegen.toVarName("user|name"), "userName");
        Assertions.assertEquals(fakeJavaCodegen.toVarName("uSername"), "uSername");
        Assertions.assertEquals(fakeJavaCodegen.toVarName("USERname"), "usERname");
        Assertions.assertEquals(fakeJavaCodegen.toVarName("USERNAME"), "USERNAME");
        Assertions.assertEquals(fakeJavaCodegen.toVarName("USER123NAME"), "USER123NAME");
        Assertions.assertEquals(fakeJavaCodegen.toVarName("1"), "_1");
        Assertions.assertEquals(fakeJavaCodegen.toVarName("1a"), "_1a");
        Assertions.assertEquals(fakeJavaCodegen.toVarName("1A"), "_1A");
        Assertions.assertEquals(fakeJavaCodegen.toVarName("1AAAA"), "_1AAAA");
        Assertions.assertEquals(fakeJavaCodegen.toVarName("1AAaa"), "_1aAaa");

        AbstractJavaCodegen withCaml = new P_AbstractJavaCodegen();
        withCaml.setCamelCaseDollarSign(true);
        Assertions.assertEquals(withCaml.toVarName("$name"), "$Name");
        Assertions.assertEquals(withCaml.toVarName("1AAaa"), "_1AAaa");
    }

    @Test
    public void convertModelName() {
        Assertions.assertEquals(fakeJavaCodegen.toModelName("name"), "Name");
        Assertions.assertEquals(fakeJavaCodegen.toModelName("$name"), "Name");
        Assertions.assertEquals(fakeJavaCodegen.toModelName("nam#e"), "Name");
        Assertions.assertEquals(fakeJavaCodegen.toModelName("$another-fake?"), "AnotherFake");
        Assertions.assertEquals(fakeJavaCodegen.toModelName("1a"), "Model1a");
        Assertions.assertEquals(fakeJavaCodegen.toModelName("1A"), "Model1A");
        Assertions.assertEquals(fakeJavaCodegen.toModelName("AAAb"), "AAAb");
        Assertions.assertEquals(fakeJavaCodegen.toModelName("aBB"), "ABB");
        Assertions.assertEquals(fakeJavaCodegen.toModelName("AaBBa"), "AaBBa");
        Assertions.assertEquals(fakeJavaCodegen.toModelName("A_B"), "AB");
        Assertions.assertEquals(fakeJavaCodegen.toModelName("A-B"), "AB");
        Assertions.assertEquals(fakeJavaCodegen.toModelName("Aa_Bb"), "AaBb");
        Assertions.assertEquals(fakeJavaCodegen.toModelName("Aa-Bb"), "AaBb");
        Assertions.assertEquals(fakeJavaCodegen.toModelName("Aa_bb"), "AaBb");
        Assertions.assertEquals(fakeJavaCodegen.toModelName("Aa-bb"), "AaBb");
    }

    @Test
    public void testInitialConfigValues() throws Exception {
        OpenAPI openAPI = TestUtils.createOpenAPI();

        final AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        codegen.processOpts();
        codegen.preprocessOpenAPI(openAPI);

        Assertions.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assertions.assertFalse(codegen.isHideGenerationTimestamp());
        Assertions.assertEquals(codegen.modelPackage(), "invalidPackageName");
        Assertions.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "invalidPackageName");
        Assertions.assertEquals(codegen.apiPackage(), "invalidPackageName");
        Assertions.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "invalidPackageName");
        Assertions.assertEquals(codegen.getInvokerPackage(), "org.openapitools");
        Assertions.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "org.openapitools");
        Assertions.assertEquals(codegen.additionalProperties().get(AbstractJavaCodegen.BOOLEAN_GETTER_PREFIX), "get");
        Assertions.assertEquals(codegen.getArtifactVersion(), openAPI.getInfo().getVersion());
        Assertions.assertEquals(codegen.additionalProperties().get(CodegenConstants.ARTIFACT_VERSION), openAPI.getInfo().getVersion());
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

        Assertions.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assertions.assertTrue(codegen.isHideGenerationTimestamp());
        Assertions.assertEquals(codegen.modelPackage(), "xyz.yyyyy.zzzzzzz.model");
        Assertions.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "xyz.yyyyy.zzzzzzz.model");
        Assertions.assertEquals(codegen.apiPackage(), "xyz.yyyyy.zzzzzzz.api");
        Assertions.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "xyz.yyyyy.zzzzzzz.api");
        Assertions.assertEquals(codegen.getInvokerPackage(), "xyz.yyyyy.zzzzzzz.invoker");
        Assertions.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "xyz.yyyyy.zzzzzzz.invoker");
        Assertions.assertEquals(codegen.additionalProperties().get(AbstractJavaCodegen.BOOLEAN_GETTER_PREFIX), "is");
        Assertions.assertEquals(codegen.getArtifactVersion(), "0.9.0-SNAPSHOT");
        Assertions.assertEquals(codegen.additionalProperties().get(CodegenConstants.ARTIFACT_VERSION), "0.9.0-SNAPSHOT");
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


        Assertions.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assertions.assertFalse(codegen.isHideGenerationTimestamp());
        Assertions.assertEquals(codegen.modelPackage(), "xyz.yyyyy.model.oooooo");
        Assertions.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "xyz.yyyyy.model.oooooo");
        Assertions.assertEquals(codegen.apiPackage(), "xyz.yyyyy.api.oooooo");
        Assertions.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "xyz.yyyyy.api.oooooo");
        Assertions.assertEquals(codegen.getInvokerPackage(), "xyz.yyyyy.invoker.oooooo");
        Assertions.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "xyz.yyyyy.invoker.oooooo");
        Assertions.assertEquals(codegen.additionalProperties().get(AbstractJavaCodegen.BOOLEAN_GETTER_PREFIX), "getBoolean");
        Assertions.assertEquals(codegen.getArtifactVersion(), "0.8.0-SNAPSHOT");
        Assertions.assertEquals(codegen.additionalProperties().get(CodegenConstants.ARTIFACT_VERSION), "0.8.0-SNAPSHOT");
    }

    @Test
    public void testAdditionalModelTypeAnnotationsSemiColon() throws Exception {
        OpenAPI openAPI = TestUtils.createOpenAPI();

        final AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        codegen.additionalProperties().put(AbstractJavaCodegen.ADDITIONAL_MODEL_TYPE_ANNOTATIONS, "@Foo;@Bar");

        codegen.processOpts();
        codegen.preprocessOpenAPI(openAPI);

        final List<String> additionalModelTypeAnnotations = new ArrayList<String>();
        additionalModelTypeAnnotations.add("@Foo");
        additionalModelTypeAnnotations.add("@Bar");

        final List<String> sortedCodegenAdditionalModelTypeAnnotations = new ArrayList<>(codegen.getAdditionalModelTypeAnnotations());
        final List<String> sortedAdditionalModelTypeAnnotations = new ArrayList<>(codegen.getAdditionalModelTypeAnnotations());

        Collections.sort(sortedCodegenAdditionalModelTypeAnnotations);
        Collections.sort(sortedAdditionalModelTypeAnnotations);
        Assertions.assertEquals(sortedCodegenAdditionalModelTypeAnnotations, sortedAdditionalModelTypeAnnotations);
    }

    @Test
    public void testAdditionalModelTypeAnnotationsNewLineLinux() throws Exception {
        OpenAPI openAPI = TestUtils.createOpenAPI();

        final AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        codegen.additionalProperties().put(AbstractJavaCodegen.ADDITIONAL_MODEL_TYPE_ANNOTATIONS, "@Foo\n@Bar");

        codegen.processOpts();
        codegen.preprocessOpenAPI(openAPI);

        final List<String> additionalModelTypeAnnotations = new ArrayList<String>();
        additionalModelTypeAnnotations.add("@Foo");
        additionalModelTypeAnnotations.add("@Bar");

        final List<String> sortedCodegenAdditionalModelTypeAnnotations = new ArrayList<>(codegen.getAdditionalModelTypeAnnotations());
        final List<String> sortedAdditionalModelTypeAnnotations = new ArrayList<>(codegen.getAdditionalModelTypeAnnotations());

        Collections.sort(sortedCodegenAdditionalModelTypeAnnotations);
        Collections.sort(sortedAdditionalModelTypeAnnotations);
        Assertions.assertEquals(sortedCodegenAdditionalModelTypeAnnotations, sortedAdditionalModelTypeAnnotations);
    }

    @Test
    public void testAdditionalModelTypeAnnotationsNewLineWindows() throws Exception {
        OpenAPI openAPI = TestUtils.createOpenAPI();

        final AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        codegen.additionalProperties().put(AbstractJavaCodegen.ADDITIONAL_MODEL_TYPE_ANNOTATIONS, "@Foo\r\n@Bar");

        codegen.processOpts();
        codegen.preprocessOpenAPI(openAPI);

        final List<String> additionalModelTypeAnnotations = new ArrayList<String>();
        additionalModelTypeAnnotations.add("@Foo");
        additionalModelTypeAnnotations.add("@Bar");

        final List<String> sortedCodegenAdditionalModelTypeAnnotations = new ArrayList<>(codegen.getAdditionalModelTypeAnnotations());
        final List<String> sortedAdditionalModelTypeAnnotations = new ArrayList<>(codegen.getAdditionalModelTypeAnnotations());

        Collections.sort(sortedCodegenAdditionalModelTypeAnnotations);
        Collections.sort(sortedAdditionalModelTypeAnnotations);
        Assertions.assertEquals(sortedCodegenAdditionalModelTypeAnnotations, sortedAdditionalModelTypeAnnotations);
    }

    @Test
    public void testAdditionalModelTypeAnnotationsMixed() throws Exception {
        OpenAPI openAPI = TestUtils.createOpenAPI();

        final AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        codegen.additionalProperties().put(AbstractJavaCodegen.ADDITIONAL_MODEL_TYPE_ANNOTATIONS, " \t @Foo;\r\n@Bar  ;\n @Foobar  ");

        codegen.processOpts();
        codegen.preprocessOpenAPI(openAPI);

        final List<String> additionalModelTypeAnnotations = new ArrayList<String>();
        additionalModelTypeAnnotations.add("@Foo");
        additionalModelTypeAnnotations.add("@Bar");
        additionalModelTypeAnnotations.add("@Foobar");

        final List<String> sortedCodegenAdditionalModelTypeAnnotations = new ArrayList<>(codegen.getAdditionalModelTypeAnnotations());
        final List<String> sortedAdditionalModelTypeAnnotations = new ArrayList<>(codegen.getAdditionalModelTypeAnnotations());

        Collections.sort(sortedCodegenAdditionalModelTypeAnnotations);
        Collections.sort(sortedAdditionalModelTypeAnnotations);
        Assertions.assertEquals(sortedCodegenAdditionalModelTypeAnnotations, sortedAdditionalModelTypeAnnotations);
    }

    @Test
    public void testAdditionalModelTypeAnnotationsNoDuplicate() throws Exception {
        OpenAPI openAPI = TestUtils.createOpenAPI();

        final AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        codegen.additionalProperties().put(AbstractJavaCodegen.ADDITIONAL_MODEL_TYPE_ANNOTATIONS, "@Foo;@Bar;@Foo");

        codegen.processOpts();
        codegen.preprocessOpenAPI(openAPI);

        final List<String> additionalModelTypeAnnotations = new ArrayList<String>();
        additionalModelTypeAnnotations.add("@Foo");
        additionalModelTypeAnnotations.add("@Bar");

        final List<String> sortedCodegenAdditionalModelTypeAnnotations = new ArrayList<>(codegen.getAdditionalModelTypeAnnotations());
        final List<String> sortedAdditionalModelTypeAnnotations = new ArrayList<>(codegen.getAdditionalModelTypeAnnotations());

        Collections.sort(sortedCodegenAdditionalModelTypeAnnotations);
        Collections.sort(sortedAdditionalModelTypeAnnotations);
        Assertions.assertEquals(sortedCodegenAdditionalModelTypeAnnotations, sortedAdditionalModelTypeAnnotations);
    }

    @Test
    public void toEnumValue() {
        final AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        Assertions.assertEquals(codegen.toEnumValue("1", "Integer"), "1");
        Assertions.assertEquals(codegen.toEnumValue("42", "Double"), "42");
        Assertions.assertEquals(codegen.toEnumValue("1337", "Long"), "1337l");
        Assertions.assertEquals(codegen.toEnumValue("3.14", "Float"), "3.14f");
        Assertions.assertEquals(codegen.toEnumValue("schema.json", "URI"), "URI.create(\"schema.json\")");
    }

    @Test
    public void apiFileFolder() {
        final AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        codegen.setOutputDir("/User/open.api.tools");
        codegen.setSourceFolder("source.folder");
        codegen.setApiPackage("org.openapitools.codegen.api");
        Assertions.assertEquals(codegen.apiFileFolder(), "/User/open.api.tools/source.folder/org/openapitools/codegen/api".replace('/', File.separatorChar));
    }

    @Test
    public void apiTestFileFolder() {
        final AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        codegen.setOutputDir("/User/open.api.tools");
        codegen.setTestFolder("test.folder");
        codegen.setApiPackage("org.openapitools.codegen.api");
        Assertions.assertEquals(codegen.apiTestFileFolder(), "/User/open.api.tools/test.folder/org/openapitools/codegen/api".replace('/', File.separatorChar));
    }

    @Test
    public void modelTestFileFolder() {
        final AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        codegen.setOutputDir("/User/open.api.tools");
        codegen.setTestFolder("test.folder");
        codegen.setModelPackage("org.openapitools.codegen.model");
        Assertions.assertEquals(codegen.modelTestFileFolder(), "/User/open.api.tools/test.folder/org/openapitools/codegen/model".replace('/', File.separatorChar));
    }

    @Test
    public void apiTestFileFolderDirect() {
        final AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        codegen.setOutputTestFolder("/User/open.api.tools");
        codegen.setTestFolder("test.folder");
        codegen.setApiPackage("org.openapitools.codegen.api");
        Assertions.assertEquals(codegen.apiTestFileFolder(), "/User/open.api.tools/test.folder/org/openapitools/codegen/api".replace('/', File.separatorChar));
    }

    @Test
    public void modelTestFileFolderDirect() {
        final AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        codegen.setOutputTestFolder("/User/open.api.tools");
        codegen.setTestFolder("test.folder");
        codegen.setModelPackage("org.openapitools.codegen.model");
        Assertions.assertEquals(codegen.modelTestFileFolder(), "/User/open.api.tools/test.folder/org/openapitools/codegen/model".replace('/', File.separatorChar));
    }

    @Test
    public void modelFileFolder() {
        final AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        codegen.setOutputDir("/User/open.api.tools");
        codegen.setSourceFolder("source.folder");
        codegen.setModelPackage("org.openapitools.codegen.model");
        Assertions.assertEquals(codegen.modelFileFolder(), "/User/open.api.tools/source.folder/org/openapitools/codegen/model".replace('/', File.separatorChar));
    }

    @Test
    public void apiDocFileFolder() {
        final AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        codegen.setOutputDir("/User/open.api.tools");
        Assertions.assertEquals(codegen.apiDocFileFolder(), "/User/open.api.tools/docs/".replace('/', File.separatorChar));
    }

    @Test(description = "tests if API version specification is used if no version is provided in additional properties")
    public void openApiVersionTest() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();

        OpenAPI api = TestUtils.createOpenAPI();
        codegen.processOpts();
        codegen.preprocessOpenAPI(api);

        Assertions.assertEquals(codegen.getArtifactVersion(), "1.0.7");
    }

    @Test(description = "tests if API version specification is used if no version is provided in additional properties with snapshot version")
    public void openApiSnapShotVersionTest() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();

        codegen.additionalProperties().put("snapshotVersion", "true");

        OpenAPI api = TestUtils.createOpenAPI();
        codegen.processOpts();
        codegen.preprocessOpenAPI(api);

        Assertions.assertEquals(codegen.getArtifactVersion(), "1.0.7-SNAPSHOT");
    }

    @Test(description = "tests if artifactVersion additional property is used")
    public void additionalPropertyArtifactVersionTest() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();

        codegen.additionalProperties().put("artifactVersion", "1.1.1");

        OpenAPI api = TestUtils.createOpenAPI();
        codegen.processOpts();
        codegen.preprocessOpenAPI(api);

        Assertions.assertEquals(codegen.getArtifactVersion(), "1.1.1");
    }

    @Test(description = "tests if artifactVersion additional property is used with snapshot parameter")
    public void additionalPropertyArtifactSnapShotVersionTest() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();

        codegen.additionalProperties().put("artifactVersion", "1.1.1");
        codegen.additionalProperties().put("snapshotVersion", "true");

        OpenAPI api = TestUtils.createOpenAPI();
        codegen.processOpts();
        codegen.preprocessOpenAPI(api);

        Assertions.assertEquals(codegen.getArtifactVersion(), "1.1.1-SNAPSHOT");
    }

    @Test(description = "tests if default version is used when neither OpenAPI version nor artifactVersion additional property has been provided")
    public void defaultVersionTest() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        codegen.setArtifactVersion(null);

        OpenAPI api = TestUtils.createOpenAPI();
        api.getInfo().setVersion(null);
        codegen.processOpts();
        codegen.preprocessOpenAPI(api);

        Assertions.assertEquals(codegen.getArtifactVersion(), "1.0.0");
    }

    @Test(description = "tests if default version with snapshot is used when neither OpenAPI version nor artifactVersion additional property has been provided")
    public void snapshotVersionTest() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();

        codegen.additionalProperties().put("snapshotVersion", "true");

        OpenAPI api = TestUtils.createOpenAPI();
        api.getInfo().setVersion(null);
        codegen.processOpts();
        codegen.preprocessOpenAPI(api);

        Assertions.assertEquals(codegen.getArtifactVersion(), "1.0.0-SNAPSHOT");
    }

    @Test(description = "tests if default version with snapshot is used when OpenAPI version has been provided")
    public void snapshotVersionOpenAPITest() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();

        codegen.additionalProperties().put(CodegenConstants.SNAPSHOT_VERSION, "true");

        OpenAPI api = TestUtils.createOpenAPI();
        api.getInfo().setVersion("2.0");
        codegen.processOpts();
        codegen.preprocessOpenAPI(api);

        Assertions.assertEquals(codegen.getArtifactVersion(), "2.0-SNAPSHOT");
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

        Assertions.assertEquals(codegen.getArtifactVersion(), version);
        Assertions.assertEquals(codegen.additionalProperties().get(CodegenConstants.ARTIFACT_VERSION), version);
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

        Assertions.assertEquals(codegen.getArtifactVersion(), version);
        Assertions.assertEquals(codegen.additionalProperties().get(CodegenConstants.ARTIFACT_VERSION), version);
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

        Assertions.assertEquals(codegen.getArtifactVersion(), version);
        Assertions.assertEquals(codegen.additionalProperties().get(CodegenConstants.ARTIFACT_VERSION), version);
    }


    @Test(description = "tests if default version with snapshot is used when setArtifactVersion is used")
    public void snapshotVersionAlreadySnapshotTest() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();

        codegen.additionalProperties().put(CodegenConstants.SNAPSHOT_VERSION, "true");

        OpenAPI api = TestUtils.createOpenAPI();
        codegen.setArtifactVersion("4.1.2-SNAPSHOT");
        codegen.processOpts();
        codegen.preprocessOpenAPI(api);

        Assertions.assertEquals(codegen.getArtifactVersion(), "4.1.2-SNAPSHOT");
    }

    @Test
    public void toDefaultValueDateTimeLegacyTest() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        codegen.setDateLibrary("legacy");
        String defaultValue;

        // Test default value for date format (DateSchema)
        DateSchema dateSchema = new DateSchema();

        TimeZone.setDefault(TimeZone.getTimeZone("UTC"));
        LocalDate defaultLocalDate = LocalDate.of(2021, 5, 23);
        Date date = Date.from(defaultLocalDate.atStartOfDay(ZoneId.systemDefault()).toInstant());
        Assertions.assertEquals(date.toString(), "Sun May 23 00:00:00 UTC 2021");

        dateSchema.setDefault(date);
        defaultValue = codegen.toDefaultValue(dateSchema);

        // dateLibrary <> java8
        Assertions.assertEquals(defaultValue, "Sun May 23 00:00:00 UTC 2021");

        // Test default value for date format (DateTimeSchema)
        DateTimeSchema dateTimeSchema = new DateTimeSchema();

        OffsetDateTime defaultDateTime = OffsetDateTime.parse("1984-12-19T03:39:57-09:00");
        Assertions.assertEquals(defaultDateTime.toString(), "1984-12-19T03:39:57-09:00");

        dateTimeSchema.setDefault(defaultDateTime);
        defaultValue = codegen.toDefaultValue(dateTimeSchema);

        // dateLibrary <> java8
        Assertions.assertEquals(defaultValue, "1984-12-19T03:39:57-09:00");
    }

    @Test
    public void toDefaultValueTest() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        codegen.setDateLibrary("java8");


        Schema<?> schema = createObjectSchemaWithMinItems();
        String defaultValue = codegen.toDefaultValue(schema);
        Assertions.assertEquals(defaultValue, "null");

        // Create an alias to an array schema
        Schema<?> nestedArraySchema = new ArraySchema().items(new IntegerSchema().format("int32"));
        codegen.setOpenAPI(new OpenAPI().components(new Components().addSchemas("NestedArray", nestedArraySchema)));

        // Create an array schema with item type set to the array alias
        schema = new ArraySchema().items(new Schema().$ref("#/components/schemas/NestedArray"));

        ModelUtils.setGenerateAliasAsModel(false);
        defaultValue = codegen.toDefaultValue(codegen.fromProperty("", schema), schema);
        Assertions.assertEquals(defaultValue, "new ArrayList<>()");

        ModelUtils.setGenerateAliasAsModel(true);
        defaultValue = codegen.toDefaultValue(codegen.fromProperty("", schema), schema);
        Assertions.assertEquals(defaultValue, "new ArrayList<>()");

        // Create a map schema with additionalProperties type set to array alias
        schema = new MapSchema().additionalProperties(new Schema().$ref("#/components/schemas/NestedArray"));

        ModelUtils.setGenerateAliasAsModel(false);
        defaultValue = codegen.toDefaultValue(codegen.fromProperty("", schema), schema);
        Assertions.assertEquals(defaultValue, "new HashMap<>()");

        ModelUtils.setGenerateAliasAsModel(true);
        defaultValue = codegen.toDefaultValue(codegen.fromProperty("", schema), schema);
        Assertions.assertEquals(defaultValue, "new HashMap<>()");

        // Test default value for date format
        DateSchema dateSchema = new DateSchema();
        LocalDate defaultLocalDate = LocalDate.of(2019, 2, 15);
        Date date = Date.from(defaultLocalDate.atStartOfDay(ZoneId.systemDefault()).toInstant());
        dateSchema.setDefault(date);
        defaultValue = codegen.toDefaultValue(codegen.fromProperty("", schema), dateSchema);
        Assertions.assertEquals(defaultValue, "LocalDate.parse(\"" + defaultLocalDate.toString() + "\")");

        DateTimeSchema dateTimeSchema = new DateTimeSchema();
        OffsetDateTime defaultDateTime = OffsetDateTime.parse("1984-12-19T03:39:57-08:00");
        ZonedDateTime expectedDateTime = defaultDateTime.atZoneSameInstant(ZoneId.systemDefault());
        dateTimeSchema.setDefault(defaultDateTime);
        defaultValue = codegen.toDefaultValue(codegen.fromProperty("", schema), dateTimeSchema);
        Assertions.assertTrue(defaultValue.startsWith("OffsetDateTime.parse(\"" + expectedDateTime.toString()));

        // Test default value for number without format
        NumberSchema numberSchema = new NumberSchema();
        Double doubleValue = 100.0;
        numberSchema.setDefault(doubleValue);
        defaultValue = codegen.toDefaultValue(codegen.fromProperty("", schema), numberSchema);
        Assertions.assertEquals(defaultValue, "new BigDecimal(\"" + doubleValue + "\")");

        // Test default value for number with format set to double
        numberSchema.setFormat("double");
        defaultValue = codegen.toDefaultValue(codegen.fromProperty("", schema), numberSchema);
        Assertions.assertEquals(defaultValue, doubleValue + "d");
    }

    @Test
    public void dateDefaultValueIsIsoDate() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/spring/date-time-parameter-types-for-testing.yml");
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        codegen.setOpenAPI(openAPI);

        Set<String> imports = new HashSet<>();
        CodegenParameter parameter = codegen.fromParameter(openAPI.getPaths().get("/thingy/{date}").getGet().getParameters().get(2), imports);

        Assertions.assertEquals(parameter.dataType, "Date");
        Assertions.assertEquals(parameter.isDate, true);
        Assertions.assertEquals(parameter.defaultValue, "1974-01-01");
        Assertions.assertEquals(imports.size(), 1);
        Assertions.assertEquals(imports.iterator().next(), "Date");

        Assertions.assertNotNull(parameter.getSchema());
        Assertions.assertEquals(parameter.getSchema().baseType, "Date");
    }

    @Test
    public void dateDefaultValueIsIsoDateTime() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/spring/date-time-parameter-types-for-testing.yml");
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        codegen.setOpenAPI(openAPI);

        Set<String> imports = new HashSet<>();
        CodegenParameter parameter = codegen.fromParameter(openAPI.getPaths().get("/thingy/{date}").getGet().getParameters().get(1), imports);

        Assertions.assertEquals(parameter.dataType, "Date");
        Assertions.assertEquals(parameter.isDateTime, true);
        Assertions.assertEquals(parameter.defaultValue, "1973-12-19T03:39:57-08:00");
        Assertions.assertEquals(imports.size(), 1);
        Assertions.assertEquals(imports.iterator().next(), "Date");

        Assertions.assertNotNull(parameter.getSchema());
        Assertions.assertEquals(parameter.getSchema().baseType, "Date");
    }

    @Test
    public void getTypeDeclarationGivenSchemaMappingTest() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        codegen.schemaMapping().put("MyStringType", "com.example.foo");
        codegen.setOpenAPI(new OpenAPI().components(new Components().addSchemas("MyStringType", new StringSchema())));
        Schema<?> schema = new ArraySchema().items(new Schema().$ref("#/components/schemas/MyStringType"));
        String defaultValue = codegen.getTypeDeclaration(schema);
        Assertions.assertEquals(defaultValue, "List<com.example.foo>");
    }

    @Test
    public void getTypeDeclarationTest() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();

        Schema<?> schema = createObjectSchemaWithMinItems();
        String defaultValue = codegen.getTypeDeclaration(schema);
        Assertions.assertEquals(defaultValue, "Object");

        // Create an alias to an array schema
        Schema<?> nestedArraySchema = new ArraySchema().items(new IntegerSchema().format("int32"));
        codegen.setOpenAPI(new OpenAPI().components(new Components().addSchemas("NestedArray", nestedArraySchema)));

        // Create an array schema with item type set to the array alias
        schema = new ArraySchema().items(new Schema().$ref("#/components/schemas/NestedArray"));

        ModelUtils.setGenerateAliasAsModel(false);
        defaultValue = codegen.getTypeDeclaration(schema);
        Assertions.assertEquals(defaultValue, "List<List<Integer>>");

        ModelUtils.setGenerateAliasAsModel(true);
        defaultValue = codegen.getTypeDeclaration(schema);
        Assertions.assertEquals(defaultValue, "List<NestedArray>");

        // Create an array schema with item type set to the array alias
        schema = new ArraySchema().items(new Schema().$ref("#/components/schemas/NestedArray"));
        schema.setUniqueItems(true);

        ModelUtils.setGenerateAliasAsModel(false);
        defaultValue = codegen.getTypeDeclaration(schema);
        Assertions.assertEquals(defaultValue, "Set<List<Integer>>");

        ModelUtils.setGenerateAliasAsModel(true);
        defaultValue = codegen.getTypeDeclaration(schema);
        Assertions.assertEquals(defaultValue, "Set<NestedArray>");

        // Create a map schema with additionalProperties type set to array alias
        schema = new MapSchema().additionalProperties(new Schema().$ref("#/components/schemas/NestedArray"));

        ModelUtils.setGenerateAliasAsModel(false);
        defaultValue = codegen.getTypeDeclaration(schema);
        Assertions.assertEquals(defaultValue, "Map<String, List<Integer>>");

        ModelUtils.setGenerateAliasAsModel(true);
        defaultValue = codegen.getTypeDeclaration(schema);
        Assertions.assertEquals(defaultValue, "Map<String, NestedArray>");
    }

    @Test
    public void processOptsBooleanTrueFromString() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/petstore.yaml");
        codegen.additionalProperties().put(CodegenConstants.SNAPSHOT_VERSION, "true");
        codegen.preprocessOpenAPI(openAPI);
        Assertions.assertTrue((boolean) codegen.additionalProperties().get(CodegenConstants.SNAPSHOT_VERSION));
    }

    @Test
    public void processOptsBooleanTrueFromBoolean() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/petstore.yaml");
        codegen.additionalProperties().put(CodegenConstants.SNAPSHOT_VERSION, true);
        codegen.preprocessOpenAPI(openAPI);
        Assertions.assertTrue((boolean) codegen.additionalProperties().get(CodegenConstants.SNAPSHOT_VERSION));
    }

    @Test
    public void processOptsBooleanFalseFromString() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/petstore.yaml");
        codegen.additionalProperties().put(CodegenConstants.SNAPSHOT_VERSION, "false");
        codegen.preprocessOpenAPI(openAPI);
        Assertions.assertFalse((boolean) codegen.additionalProperties().get(CodegenConstants.SNAPSHOT_VERSION));
    }

    @Test
    public void processOptsBooleanFalseFromBoolean() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/petstore.yaml");
        codegen.additionalProperties().put(CodegenConstants.SNAPSHOT_VERSION, false);
        codegen.preprocessOpenAPI(openAPI);
        Assertions.assertFalse((boolean) codegen.additionalProperties().get(CodegenConstants.SNAPSHOT_VERSION));
    }

    @Test
    public void processOptsBooleanFalseFromGarbage() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/petstore.yaml");
        codegen.additionalProperties().put(CodegenConstants.SNAPSHOT_VERSION, "blibb");
        codegen.preprocessOpenAPI(openAPI);
        Assertions.assertFalse((boolean) codegen.additionalProperties().get(CodegenConstants.SNAPSHOT_VERSION));
    }

    @Test
    public void processOptsBooleanFalseFromNumeric() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/petstore.yaml");
        codegen.additionalProperties().put(CodegenConstants.SNAPSHOT_VERSION, 42L);
        codegen.preprocessOpenAPI(openAPI);
        Assertions.assertFalse((boolean) codegen.additionalProperties().get(CodegenConstants.SNAPSHOT_VERSION));
    }

    @Test
    public void nullDefaultValueForModelWithDynamicProperties() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/mapSchemas.yaml");
        codegen.additionalProperties().put(CodegenConstants.GENERATE_ALIAS_AS_MODEL, true);
        codegen.setOpenAPI(openAPI);

        Schema schema = openAPI.getComponents().getSchemas().get("ModelWithAdditionalProperties");
        CodegenModel cm = codegen.fromModel("ModelWithAdditionalProperties", schema);
        Assertions.assertEquals(cm.vars.size(), 1, "Expected single declared var");
        Assertions.assertEquals(cm.vars.get(0).name, "id");
        Assertions.assertNull(cm.defaultValue, "Expected no defined default value in spec");

        String defaultValue = codegen.toDefaultValue(schema);
        Assertions.assertEquals(defaultValue, "null");
    }

    @Test
    public void maplikeDefaultValueForModelWithStringToStringMapping() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/mapSchemas.yaml");
        codegen.additionalProperties().put(CodegenConstants.GENERATE_ALIAS_AS_MODEL, true);
        codegen.setOpenAPI(openAPI);

        Schema schema = openAPI.getComponents().getSchemas().get("ModelWithStringToStringMapping");
        CodegenModel cm = codegen.fromModel("ModelWithAdditionalProperties", schema);
        Assertions.assertEquals(cm.vars.size(), 0, "Expected no declared vars");
        Assertions.assertNull(cm.defaultValue, "Expected no defined default value in spec");

        String defaultValue = codegen.toDefaultValue(schema);
        Assertions.assertEquals(defaultValue, "null", "Expected string-string map aliased model to default to null since nullable is not set to true");
    }

    @Test
    public void maplikeDefaultValueForModelWithStringToModelMapping() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/mapSchemas.yaml");
        codegen.additionalProperties().put(CodegenConstants.GENERATE_ALIAS_AS_MODEL, true);
        codegen.setOpenAPI(openAPI);

        Schema schema = openAPI.getComponents().getSchemas().get("ModelWithStringToModelMapping");
        CodegenModel cm = codegen.fromModel("ModelWithStringToModelMapping", schema);
        Assertions.assertEquals(cm.vars.size(), 0, "Expected no declared vars");
        Assertions.assertNull(cm.defaultValue, "Expected no defined default value in spec");

        String defaultValue = codegen.toDefaultValue(schema);
        Assertions.assertEquals(defaultValue, "null", "Expected string-ref map aliased model to default to null since nullable is not set to tru");
    }

    @Test
    public void srcMainFolderShouldNotBeOperatingSystemSpecificPaths() {
        // it's not responsibility of the generator to fix OS-specific paths. This is left to template manager.
        // This path must be non-OS-specific for expectations in source outputs (e.g. gradle build files)
        Assertions.assertEquals(fakeJavaCodegen.getSourceFolder(), "src/main/java");
    }

    @Test
    public void srcTestFolderShouldNotBeOperatingSystemSpecificPaths() {
        // it's not responsibility of the generator to fix OS-specific paths. This is left to template manager.
        // This path must be non-OS-specific for expectations in source outputs (e.g. gradle build files)
        Assertions.assertEquals(fakeJavaCodegen.getTestFolder(), "src/test/java");
    }

    @Test
    public void testOneOfModelImports() throws Exception {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/oneOf_nonPrimitive.yaml");
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.preprocessOpenAPI(openAPI);

        Schema<?> schema = openAPI.getComponents().getSchemas().get("Example");
        CodegenModel cm = codegen.fromModel("Example", schema);
        Assertions.assertEquals(cm.imports.size(), 3);
        Assertions.assertTrue(cm.imports.contains("BigDecimal"));
        Assertions.assertTrue(cm.imports.contains("Date"));
        Assertions.assertTrue(cm.imports.contains("UUID"));
    }

    @Test
    public void arrayParameterDefaultValueDoesNotNeedBraces() throws Exception {
        ParseOptions parseOptions = new ParseOptions();
        parseOptions.setResolve(true);
        final OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/issue_16223.yaml", null, parseOptions)
                .getOpenAPI();
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        codegen.setOpenAPI(openAPI);

        Map<String, Schema> schemas = openAPI.getPaths().get("/test").getGet().getParameters().stream()
                .collect(Collectors.toMap(
                        Parameter::getName,
                        p -> ModelUtils.getReferencedSchema(openAPI, p.getSchema())));
        Assertions.assertEquals(codegen.toDefaultParameterValue(schemas.get("fileEnumWithDefault")), "A,B");
        Assertions.assertEquals(codegen.toDefaultParameterValue(schemas.get("fileEnumWithDefaultEmpty")), "");
        Assertions.assertEquals(codegen.toDefaultParameterValue(schemas.get("inlineEnumWithDefault")), "A,B");
        Assertions.assertEquals(codegen.toDefaultParameterValue(schemas.get("inlineEnumWithDefaultEmpty")), "");
    }

    @Test
    public void ignoreBeanValidationAnnotationsTest() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        codegen.additionalProperties().put("useBeanValidation", true);

        Schema<?> schema = new Schema<>().type("string").format("uuid").pattern("^[a-z]$").maxLength(36);
        String defaultValue = codegen.getTypeDeclaration(schema);
        Assertions.assertEquals(defaultValue, "UUID");

        schema = new Schema<>().type("string").format("uri").pattern("^[a-z]$").maxLength(36);
        defaultValue = codegen.getTypeDeclaration(schema);
        Assertions.assertEquals(defaultValue, "URI");

        schema = new Schema<>().type("string").format("byte").pattern("^[a-z]$").maxLength(36);
        defaultValue = codegen.getTypeDeclaration(schema);
        Assertions.assertEquals(defaultValue, "byte[]");

        schema = new Schema<>().type("string").format("binary").pattern("^[a-z]$").maxLength(36);
        defaultValue = codegen.getTypeDeclaration(schema);
        Assertions.assertEquals(defaultValue, "File");
    }

    @Test
    public void ignoreBeanValidationAnnotationsContainerTest() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        codegen.additionalProperties().put("useBeanValidation", true);

        Schema<?> schema = new ArraySchema().items(new Schema<>().type("string").format("uuid").pattern("^[a-z]$").maxLength(36));
        String defaultValue = codegen.getTypeDeclaration(schema);
        Assertions.assertEquals(defaultValue, "List<UUID>");

        schema = new ArraySchema().items(new Schema<>().type("string").format("uri").pattern("^[a-z]$").maxLength(36));
        defaultValue = codegen.getTypeDeclaration(schema);
        Assertions.assertEquals(defaultValue, "List<URI>");

        schema = new ArraySchema().items(new Schema<>().type("string").format("byte").pattern("^[a-z]$").maxLength(36));
        defaultValue = codegen.getTypeDeclaration(schema);
        Assertions.assertEquals(defaultValue, "List<byte[]>");

        schema = new ArraySchema().items(new Schema<>().type("string").format("binary").pattern("^[a-z]$").maxLength(36));
        defaultValue = codegen.getTypeDeclaration(schema);
        Assertions.assertEquals(defaultValue, "List<File>");
    }

    @Test
    public void AnnotationsContainerTest() {
        final P_AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        codegen.additionalProperties().put("useBeanValidation", true);

        // 1. string type
        Schema<?> schema = new ArraySchema().items(new Schema<>().type("string").pattern("^[a-z]$").minLength(0).maxLength(36));
        String defaultValue = codegen.getTypeDeclaration(schema);
        Assertions.assertEquals(defaultValue, "List<@Pattern(regexp = \"^[a-z]$\")@Size(min = 0, max = 36)String>");

        schema = new ArraySchema().items(new Schema<>().type("string").pattern("^[a-z]$").minLength(0));
        defaultValue = codegen.getTypeDeclaration(schema);
        Assertions.assertEquals(defaultValue, "List<@Pattern(regexp = \"^[a-z]$\")@Size(min = 0)String>");

        schema = new ArraySchema().items(new Schema<>().type("string").pattern("^[a-z]$").maxLength(36));
        defaultValue = codegen.getTypeDeclaration(schema);
        Assertions.assertEquals(defaultValue, "List<@Pattern(regexp = \"^[a-z]$\")@Size(max = 36)String>");

        schema = new ArraySchema().items(new Schema<>().type("string").format("email"));
        defaultValue = codegen.getTypeDeclaration(schema);
        Assertions.assertEquals(defaultValue, "List<@Email String>");

        // 2. string type with number format
        schema = new ArraySchema().items(new Schema<>().type("string").format("number").minimum(BigDecimal.ZERO).maximum(BigDecimal.TEN).exclusiveMinimum(Boolean.TRUE).exclusiveMaximum(Boolean.TRUE));
        defaultValue = codegen.getTypeDeclaration(schema);
        Assertions.assertEquals(defaultValue, "List<@DecimalMin(value = \"0\", inclusive = false) @DecimalMax(value = \"10\", inclusive = false)BigDecimal>");

        schema = new ArraySchema().items(new Schema<>().type("string").format("number").minimum(BigDecimal.ZERO).exclusiveMinimum(Boolean.TRUE));
        defaultValue = codegen.getTypeDeclaration(schema);
        Assertions.assertEquals(defaultValue, "List<@DecimalMin( value = \"0\", inclusive = false)BigDecimal>");

        schema = new ArraySchema().items(new Schema<>().type("string").format("number").maximum(BigDecimal.TEN).exclusiveMaximum(Boolean.TRUE));
        defaultValue = codegen.getTypeDeclaration(schema);
        Assertions.assertEquals(defaultValue, "List<@DecimalMax( value = \"10\", inclusive = false)BigDecimal>");

        // 3. number type
        schema = new ArraySchema().items(new Schema<>().type("number").minimum(BigDecimal.ZERO).maximum(BigDecimal.TEN).exclusiveMinimum(Boolean.TRUE).exclusiveMaximum(Boolean.TRUE));
        defaultValue = codegen.getTypeDeclaration(schema);
        Assertions.assertEquals(defaultValue, "List<@DecimalMin(value = \"0\", inclusive = false) @DecimalMax(value = \"10\", inclusive = false)BigDecimal>");

        schema = new ArraySchema().items(new Schema<>().type("number").minimum(BigDecimal.ZERO).exclusiveMinimum(Boolean.TRUE));
        defaultValue = codegen.getTypeDeclaration(schema);
        Assertions.assertEquals(defaultValue, "List<@DecimalMin( value = \"0\", inclusive = false)BigDecimal>");

        schema = new ArraySchema().items(new Schema<>().type("number").maximum(BigDecimal.TEN).exclusiveMaximum(Boolean.TRUE));
        defaultValue = codegen.getTypeDeclaration(schema);
        Assertions.assertEquals(defaultValue, "List<@DecimalMax( value = \"10\", inclusive = false)BigDecimal>");

        schema = new ArraySchema().items(new Schema<>().type("number").minimum(BigDecimal.ZERO).maximum(BigDecimal.TEN));
        defaultValue = codegen.getTypeDeclaration(schema);
        Assertions.assertEquals(defaultValue, "List<@DecimalMin(value = \"0\", inclusive = true) @DecimalMax(value = \"10\", inclusive = true)BigDecimal>");

        schema = new ArraySchema().items(new Schema<>().type("number").minimum(BigDecimal.ZERO));
        defaultValue = codegen.getTypeDeclaration(schema);
        Assertions.assertEquals(defaultValue, "List<@DecimalMin( value = \"0\", inclusive = true)BigDecimal>");

        schema = new ArraySchema().items(new Schema<>().type("number").maximum(BigDecimal.TEN));
        defaultValue = codegen.getTypeDeclaration(schema);
        Assertions.assertEquals(defaultValue, "List<@DecimalMax( value = \"10\", inclusive = true)BigDecimal>");

        // 4. integer type with int64 format
        schema = new ArraySchema().items(new Schema<>().type("integer").format("int64").minimum(BigDecimal.ZERO).maximum(BigDecimal.TEN));
        defaultValue = codegen.getTypeDeclaration(schema);
        Assertions.assertEquals(defaultValue, "List<@Min(0L) @Max(10L)Long>");

        schema = new ArraySchema().items(new Schema<>().type("integer").format("int64").minimum(BigDecimal.ZERO));
        defaultValue = codegen.getTypeDeclaration(schema);
        Assertions.assertEquals(defaultValue, "List<@Min(0L)Long>");

        schema = new ArraySchema().items(new Schema<>().type("integer").format("int64").maximum(BigDecimal.TEN));
        defaultValue = codegen.getTypeDeclaration(schema);
        Assertions.assertEquals(defaultValue, "List<@Max(10L)Long>");

        // 5. integer type
        schema = new ArraySchema().items(new Schema<>().type("integer").minimum(BigDecimal.ZERO).maximum(BigDecimal.TEN));
        defaultValue = codegen.getTypeDeclaration(schema);
        Assertions.assertEquals(defaultValue, "List<@Min(0) @Max(10)Integer>");

        schema = new ArraySchema().items(new Schema<>().type("integer").minimum(BigDecimal.ZERO));
        defaultValue = codegen.getTypeDeclaration(schema);
        Assertions.assertEquals(defaultValue, "List<@Min(0)Integer>");

        schema = new ArraySchema().items(new Schema<>().type("integer").maximum(BigDecimal.TEN));
        defaultValue = codegen.getTypeDeclaration(schema);
        Assertions.assertEquals(defaultValue, "List<@Max(10)Integer>");
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
