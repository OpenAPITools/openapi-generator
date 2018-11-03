/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.ruby;

import io.swagger.v3.oas.models.Operation;
import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.RubyClientCodegen;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.parser.core.models.ParseOptions;

import org.apache.commons.io.FileUtils;
import org.junit.rules.TemporaryFolder;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.util.*;

import static org.testng.Assert.*;

/**
 * Tests for RubyClientCodegen-generated templates
 */
public class RubyClientCodegenTest {

    public TemporaryFolder folder = new TemporaryFolder();

    @BeforeMethod
    public void setUp() throws Exception {
        folder.create();
    }

    @AfterMethod
    public void tearDown() throws Exception {
        folder.delete();
    }

    @Test
    public void testGenerateRubyClientWithHtmlEntity() throws Exception {
        final File output = folder.getRoot();

        final OpenAPI openAPI = new OpenAPIParser().readLocation("src/test/resources/2_0/pathWithHtmlEntity.yaml", null, new ParseOptions()).getOpenAPI();
        CodegenConfig codegenConfig = new RubyClientCodegen();
        codegenConfig.setOutputDir(output.getAbsolutePath());

        ClientOptInput clientOptInput = new ClientOptInput().opts(new ClientOpts()).openAPI(openAPI).config(codegenConfig);

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();
        boolean apiFileGenerated = false;
        for (File file : files) {
            if (file.getName().equals("default_api.rb")) {
                apiFileGenerated = true;
                // Ruby client should set the path unescaped in the api file
                assertTrue(FileUtils.readFileToString(file, StandardCharsets.UTF_8).contains("local_var_path = '/foo=bar'"));
            }
        }
        if (!apiFileGenerated) {
            fail("Default api file is not generated!");
        }
    }

    @Test
    public void testInitialConfigValues() throws Exception {
        final RubyClientCodegen codegen = new RubyClientCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
        Assert.assertEquals(codegen.modelPackage(), "models");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), null);
        Assert.assertEquals(codegen.apiPackage(), "api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), null);
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final RubyClientCodegen codegen = new RubyClientCodegen();
        codegen.setHideGenerationTimestamp(false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final RubyClientCodegen codegen = new RubyClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, false);
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "ruby-models");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "ruby-api");
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "ruby-models");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "ruby-api");
    }

    @Test
    public void testBooleanDefaultValue() throws Exception {
        final File output = folder.getRoot();

        final OpenAPI openAPI = new OpenAPIParser().readLocation("src/test/resources/2_0/npe1.yaml", null, new ParseOptions()).getOpenAPI();
        CodegenConfig codegenConfig = new RubyClientCodegen();
        codegenConfig.setOutputDir(output.getAbsolutePath());

        ClientOptInput clientOptInput = new ClientOptInput().opts(new ClientOpts()).openAPI(openAPI).config(codegenConfig);

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();
        boolean apiFileGenerated = false;
        for (File file : files) {
            if (file.getName().equals("default_api.rb")) {
                apiFileGenerated = true;
                // Ruby client should set the path unescaped in the api file
                assertTrue(FileUtils.readFileToString(file, StandardCharsets.UTF_8).contains("local_var_path = '/default/Resources/{id}'"));
            }
        }
        if (!apiFileGenerated) {
            fail("Default api file is not generated!");
        }
    }

    @Test(description = "verify enum parameters (query, form, header)")
    public void enumParameterTest() {
        final OpenAPI openAPI = new OpenAPIParser().readLocation("src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml", null, new ParseOptions()).getOpenAPI();
        final DefaultCodegen codegen = new RubyClientCodegen();
        final String path = "/fake";
        final Operation p = openAPI.getPaths().get(path).getGet();
        final CodegenOperation op = codegen.fromOperation(path, "get", p, openAPI.getComponents().getSchemas());
        Assert.assertEquals(op.formParams.size(), 2);
        CodegenParameter fp = op.formParams.get(0);
        Assert.assertEquals(fp.dataType, "Array<String>");
        Assert.assertEquals(fp.datatypeWithEnum, "Array<ENUM_FORM_STRING_ARRAY>");
        Assert.assertEquals(fp.enumName, "ENUM_FORM_STRING_ARRAY");
    }

    @Test(description = "test example value for body parameter")
    public void bodyParameterTest() {
        final OpenAPI openAPI = new OpenAPIParser().readLocation("src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml", null, new ParseOptions()).getOpenAPI();
        final RubyClientCodegen codegen = new RubyClientCodegen();
        codegen.setModuleName("OnlinePetstore");
        final String path = "/pet";
        final Operation p = openAPI.getPaths().get(path).getPost();
        final CodegenOperation op = codegen.fromOperation(path, "post", p, openAPI.getComponents().getSchemas());
        Assert.assertEquals(op.bodyParams.size(), 1);
        CodegenParameter bp = op.bodyParams.get(0);
        Assert.assertEquals(bp.example, "OnlinePetstore::Pet.new");
    }


    @Test(description = "test nullable for properties")
    public void nullablePropertyTest() {
        final OpenAPI openAPI = new OpenAPIParser().readLocation("src/test/resources/3_0/petstore_oas3_test.yaml", null, new ParseOptions()).getOpenAPI();
        final RubyClientCodegen codegen = new RubyClientCodegen();
        codegen.setModuleName("OnlinePetstore");
        final String path = "/pet";

        final Schema schema = openAPI.getComponents().getSchemas().get("NullablePet");
        CodegenModel nullablePet = codegen.fromModel("NullablePet", schema, openAPI.getComponents().getSchemas());
        CodegenProperty cp0 = nullablePet.getVars().get(0);
        Assert.assertTrue(cp0.isNullable);

        CodegenProperty cp1 = nullablePet.getVars().get(1);
        Assert.assertFalse(cp1.isNullable);

        CodegenProperty cp2 = nullablePet.getVars().get(2);
        Assert.assertTrue(cp2.isNullable);

        CodegenProperty cp3 = nullablePet.getVars().get(3);
        Assert.assertTrue(cp3.isNullable);

        CodegenProperty cp4 = nullablePet.getVars().get(4);
        Assert.assertFalse(cp4.isNullable);

        CodegenProperty cp5 = nullablePet.getVars().get(5);
        Assert.assertTrue(cp5.isNullable);
    }

    @Test(description = "test properties without nullable")
    public void propertiesWithoutNullableTest() {
        final OpenAPI openAPI = new OpenAPIParser().readLocation("src/test/resources/3_0/petstore_oas3_test.yaml", null, new ParseOptions()).getOpenAPI();
        final RubyClientCodegen codegen = new RubyClientCodegen();
        codegen.setModuleName("OnlinePetstore");
        final String path = "/pet";

        final Schema schema = openAPI.getComponents().getSchemas().get("Pet");
        CodegenModel nullablePet = codegen.fromModel("Pet", schema, openAPI.getComponents().getSchemas());
        CodegenProperty cp0 = nullablePet.getVars().get(0);
        Assert.assertFalse(cp0.isNullable);

        CodegenProperty cp1 = nullablePet.getVars().get(1);
        Assert.assertFalse(cp1.isNullable);

        CodegenProperty cp2 = nullablePet.getVars().get(2);
        Assert.assertFalse(cp2.isNullable);

        CodegenProperty cp3 = nullablePet.getVars().get(3);
        Assert.assertFalse(cp3.isNullable);

        CodegenProperty cp4 = nullablePet.getVars().get(4);
        Assert.assertFalse(cp4.isNullable);

        CodegenProperty cp5 = nullablePet.getVars().get(5);
        Assert.assertFalse(cp5.isNullable);
    }

    @Test(description = "test nullable for parameters (OAS3)")
    public void nullableParameterOAS3Test() {
        final OpenAPI openAPI = new OpenAPIParser().readLocation("src/test/resources/3_0/petstore_oas3_test.yaml", null, new ParseOptions()).getOpenAPI();
        final RubyClientCodegen codegen = new RubyClientCodegen();
        codegen.setModuleName("OnlinePetstore");
        final String path = "/pet/{petId}";

        final Operation p = openAPI.getPaths().get(path).getPost();
        final CodegenOperation op = codegen.fromOperation(path, "post", p, openAPI.getComponents().getSchemas());

        Assert.assertEquals(op.pathParams.size(), 1);
        CodegenParameter pp = op.pathParams.get(0);
        Assert.assertTrue(pp.isNullable);

        Assert.assertEquals(op.formParams.size(), 2);
        CodegenParameter name = op.formParams.get(0);
        Assert.assertFalse(name.isNullable);
        CodegenParameter status = op.formParams.get(1);
        Assert.assertTrue(status.isNullable);
    }

    @Test(description = "test nullable for parameters (OAS2)")
    public void nullableParameterOAS2Test() {
        final OpenAPI openAPI = new OpenAPIParser().readLocation("src/test/resources/2_0/petstore-nullable.yaml", null, new ParseOptions()).getOpenAPI();
        final RubyClientCodegen codegen = new RubyClientCodegen();
        codegen.setModuleName("OnlinePetstore");
        final String path = "/pet/{petId}";

        final Operation p = openAPI.getPaths().get(path).getPost();
        final CodegenOperation op = codegen.fromOperation(path, "post", p, openAPI.getComponents().getSchemas());

        // path parameter x-nullable test
        Assert.assertEquals(op.pathParams.size(), 1);
        CodegenParameter pp = op.pathParams.get(0);
        Assert.assertTrue(pp.isNullable);

        // form parameter x-nullable test
        Assert.assertEquals(op.formParams.size(), 2);
        CodegenParameter name = op.formParams.get(0);
        Assert.assertFalse(name.isNullable);
        CodegenParameter status = op.formParams.get(1);
        // TODO comment out the following until https://github.com/swagger-api/swagger-parser/issues/820 is solved
        //Assert.assertTrue(status.isNullable);
    }


    @Test(description = "test anyOf (OAS3)")
    public void anyOfTest() {
        final OpenAPI openAPI = new OpenAPIParser().readLocation("src/test/resources/3_0/anyOf.yaml", null, new ParseOptions()).getOpenAPI();
        final RubyClientCodegen codegen = new RubyClientCodegen();
        codegen.setModuleName("OnlinePetstore");

        final Schema schema = openAPI.getComponents().getSchemas().get("fruit");
        CodegenModel fruit = codegen.fromModel("Fruit", schema, openAPI.getComponents().getSchemas());

        Set<String> anyOf = new TreeSet<String>();
        anyOf.add("Apple");
        anyOf.add("Banana");
        Assert.assertEquals(fruit.anyOf, anyOf);
    }

    @Test(description = "test oneOf (OAS3)")
    public void oneOfTest() {
        final OpenAPI openAPI = new OpenAPIParser().readLocation("src/test/resources/3_0/oneOf.yaml", null, new ParseOptions()).getOpenAPI();
        final RubyClientCodegen codegen = new RubyClientCodegen();
        codegen.setModuleName("OnlinePetstore");

        final Schema schema = openAPI.getComponents().getSchemas().get("fruit");
        CodegenModel fruit = codegen.fromModel("Fruit", schema, openAPI.getComponents().getSchemas());

        Set<String> oneOf = new TreeSet<String>();
        oneOf.add("Apple");
        oneOf.add("Banana");
        Assert.assertEquals(fruit.oneOf, oneOf);
    }

    @Test(description = "test allOf (OAS3)")
    public void allOfTest() {
        final OpenAPI openAPI = new OpenAPIParser().readLocation("src/test/resources/3_0/allOf.yaml", null, new ParseOptions()).getOpenAPI();
        final RubyClientCodegen codegen = new RubyClientCodegen();
        codegen.setModuleName("OnlinePetstore");

        final Schema schema = openAPI.getComponents().getSchemas().get("Person");
        CodegenModel person = codegen.fromModel("Person", schema, openAPI.getComponents().getSchemas());
        Assert.assertNotNull(person);

        CodegenDiscriminator codegenDiscriminator = person.getDiscriminator();
        Set<CodegenDiscriminator.MappedModel> mappedModels = new LinkedHashSet<CodegenDiscriminator.MappedModel>();
        CodegenDiscriminator.MappedModel adult = new CodegenDiscriminator.MappedModel("a", "Adult");
        mappedModels.add(adult);
        CodegenDiscriminator.MappedModel child = new CodegenDiscriminator.MappedModel("c", "Child");
        mappedModels.add(child);
        Assert.assertEquals(codegenDiscriminator.getMappedModels(), mappedModels);
    }

    @Test(description = "test allOf with only allOf and duplicated properties(OAS3)")
    public void allOfDuplicatedPropertiesTest() {
        final OpenAPI openAPI = new OpenAPIParser().readLocation("src/test/resources/3_0/allOfDuplicatedProperties.yaml", null, new ParseOptions()).getOpenAPI();
        final RubyClientCodegen codegen = new RubyClientCodegen();
        codegen.setModuleName("OnlinePetstore");

        final Schema schema = openAPI.getComponents().getSchemas().get("ModelC");
        CodegenModel modelC = codegen.fromModel("ModelC", schema, openAPI.getComponents().getSchemas());
        Assert.assertNotNull(modelC);
        Assert.assertEquals(modelC.getVars().size(), 5);

        CodegenProperty cp0 = modelC.getVars().get(0);
        Assert.assertEquals(cp0.name, "foo");

        CodegenProperty cp1 = modelC.getVars().get(1);
        Assert.assertEquals(cp1.name, "duplicated_optional");

        CodegenProperty cp2 = modelC.getVars().get(2);
        Assert.assertEquals(cp2.name, "duplicated_required");

        CodegenProperty cp3 = modelC.getVars().get(3);
        Assert.assertEquals(cp3.name, "bar");

        CodegenProperty cp4 = modelC.getVars().get(4);
        Assert.assertEquals(cp4.name, "baz");
    }


    @Test(description = "test allOf with discriminator and duplicated properties(OAS3)")
    public void allOfMappingDuplicatedPropertiesTest() {
        final OpenAPI openAPI = new OpenAPIParser().readLocation("src/test/resources/3_0/allOfMappingDuplicatedProperties.yaml", null, new ParseOptions()).getOpenAPI();
        final RubyClientCodegen codegen = new RubyClientCodegen();
        codegen.setModuleName("OnlinePetstore");

        final Schema schema = openAPI.getComponents().getSchemas().get("Child");
        CodegenModel child = codegen.fromModel("Child", schema, openAPI.getComponents().getSchemas());
        Assert.assertNotNull(child);
        Assert.assertEquals(child.getVars().size(), 6);

        CodegenProperty cp0 = child.getVars().get(0);
        Assert.assertEquals(cp0.name, "age");

        CodegenProperty cp1 = child.getVars().get(1);
        Assert.assertEquals(cp1.name, "first_name");

        CodegenProperty cp2 = child.getVars().get(2);
        Assert.assertEquals(cp2.name, "_type");

        CodegenProperty cp3 = child.getVars().get(3);
        Assert.assertEquals(cp3.name, "last_name");

        CodegenProperty cp4 = child.getVars().get(4);
        Assert.assertEquals(cp4.name, "duplicated_optional");

        CodegenProperty cp5 = child.getVars().get(5);
        Assert.assertEquals(cp5.name, "duplicated_required");
    }
}
