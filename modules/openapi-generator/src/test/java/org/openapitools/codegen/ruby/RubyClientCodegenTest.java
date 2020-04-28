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

package org.openapitools.codegen.ruby;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.io.FileUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.RubyClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.TreeSet;

import static org.testng.Assert.assertTrue;
import static org.testng.Assert.fail;

/**
 * Tests for RubyClientCodegen-generated templates
 */
public class RubyClientCodegenTest {


    @Test
    public void testGenerateRubyClientWithHtmlEntity() throws Exception {
        final File output = Files.createTempDirectory("test").toFile();
        output.mkdirs();
        output.deleteOnExit();

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/pathWithHtmlEntity.yaml");
        CodegenConfig codegenConfig = new RubyClientCodegen();
        codegenConfig.setOutputDir(output.getAbsolutePath());

        ClientOptInput clientOptInput = new ClientOptInput().openAPI(openAPI).config(codegenConfig);

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
        final File output = Files.createTempDirectory("test").toFile();
        output.mkdirs();
        output.deleteOnExit();

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/npe1.yaml");
        CodegenConfig codegenConfig = new RubyClientCodegen();
        codegenConfig.setOutputDir(output.getAbsolutePath());

        ClientOptInput clientOptInput = new ClientOptInput().openAPI(openAPI).config(codegenConfig);

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
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml");
        final DefaultCodegen codegen = new RubyClientCodegen();
        codegen.setOpenAPI(openAPI);
        final String path = "/fake";
        final Operation p = openAPI.getPaths().get(path).getGet();
        final CodegenOperation op = codegen.fromOperation(path, "get", p, null);
        Assert.assertEquals(op.formParams.size(), 2);
        CodegenParameter fp = op.formParams.get(0);
        Assert.assertEquals(fp.dataType, "Array<String>");
        Assert.assertEquals(fp.datatypeWithEnum, "Array<ENUM_FORM_STRING_ARRAY>");
        Assert.assertEquals(fp.enumName, "ENUM_FORM_STRING_ARRAY");
    }

    @Test(description = "test example value for body parameter")
    public void bodyParameterTest() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml");
        final RubyClientCodegen codegen = new RubyClientCodegen();
        codegen.setModuleName("OnlinePetstore");
        codegen.setOpenAPI(openAPI);
        final String path = "/pet";
        final Operation p = openAPI.getPaths().get(path).getPost();
        final CodegenOperation op = codegen.fromOperation(path, "post", p, null);
        Assert.assertEquals(op.bodyParams.size(), 1);
        CodegenParameter bp = op.bodyParams.get(0);
        Assert.assertEquals(bp.example, "OnlinePetstore::Pet.new");
    }


    @Test(description = "test nullable for properties")
    public void nullablePropertyTest() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/petstore_oas3_test.yaml");
        final RubyClientCodegen codegen = new RubyClientCodegen();
        codegen.setModuleName("OnlinePetstore");
        final String path = "/pet";

        final Schema schema = openAPI.getComponents().getSchemas().get("NullablePet");
        codegen.setOpenAPI(openAPI);
        CodegenModel nullablePet = codegen.fromModel("NullablePet", schema);
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
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/petstore_oas3_test.yaml");
        final RubyClientCodegen codegen = new RubyClientCodegen();
        codegen.setModuleName("OnlinePetstore");
        final String path = "/pet";

        final Schema schema = openAPI.getComponents().getSchemas().get("Pet");
        codegen.setOpenAPI(openAPI);
        CodegenModel nullablePet = codegen.fromModel("Pet", schema);

        Assert.assertNotNull(nullablePet);
        Assert.assertEquals(nullablePet.getVars().size(), 6);
        CodegenProperty cp0 = nullablePet.getVars().get(0);
        Assert.assertFalse(cp0.isNullable);
        Assert.assertEquals(cp0.name, "id");

        CodegenProperty cp1 = nullablePet.getVars().get(1);
        Assert.assertFalse(cp1.isNullable);
        Assert.assertEquals(cp1.name, "category");

        CodegenProperty cp2 = nullablePet.getVars().get(2);
        Assert.assertFalse(cp2.isNullable);
        Assert.assertEquals(cp2.name, "name");

        CodegenProperty cp3 = nullablePet.getVars().get(3);
        Assert.assertFalse(cp3.isNullable);
        Assert.assertEquals(cp3.name, "photo_urls");

        CodegenProperty cp4 = nullablePet.getVars().get(4);
        Assert.assertFalse(cp4.isNullable);
        Assert.assertEquals(cp4.name, "tags");

        CodegenProperty cp5 = nullablePet.getVars().get(5);
        Assert.assertFalse(cp5.isNullable);
        Assert.assertEquals(cp5.name, "status");

        // test allVars
        Assert.assertEquals(nullablePet.getAllVars().size(), 6);
        cp0 = nullablePet.getVars().get(0);
        Assert.assertFalse(cp0.isNullable);
        Assert.assertEquals(cp0.name, "id");

        cp1 = nullablePet.getVars().get(1);
        Assert.assertFalse(cp1.isNullable);
        Assert.assertEquals(cp1.name, "category");

        cp2 = nullablePet.getVars().get(2);
        Assert.assertFalse(cp2.isNullable);
        Assert.assertEquals(cp2.name, "name");

        cp3 = nullablePet.getVars().get(3);
        Assert.assertFalse(cp3.isNullable);
        Assert.assertEquals(cp3.name, "photo_urls");

        cp4 = nullablePet.getVars().get(4);
        Assert.assertFalse(cp4.isNullable);
        Assert.assertEquals(cp4.name, "tags");

        cp5 = nullablePet.getVars().get(5);
        Assert.assertFalse(cp5.isNullable);
        Assert.assertEquals(cp5.name, "status");

        // test requiredVars
        Assert.assertEquals(nullablePet.getRequiredVars().size(), 2);
        cp0 = nullablePet.getRequiredVars().get(0);
        Assert.assertFalse(cp0.isNullable);
        Assert.assertEquals(cp0.name, "name");

        cp1 = nullablePet.getRequiredVars().get(1);
        Assert.assertFalse(cp1.isNullable);
        Assert.assertEquals(cp1.name, "photo_urls");

        // test mandatory
        Set<String> mandatory = new TreeSet<String>();
        mandatory.add("name");
        mandatory.add("photo_urls");
        Assert.assertEquals(nullablePet.getMandatory(), mandatory);

        // test allMandatory
        Set<String> allMandatory = new TreeSet<String>();
        allMandatory.add("name");
        allMandatory.add("photo_urls");
        Assert.assertEquals(nullablePet.getAllMandatory(), allMandatory);
    }

    @Test(description = "test nullable for parameters (OAS3)")
    public void nullableParameterOAS3Test() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/petstore_oas3_test.yaml");
        final RubyClientCodegen codegen = new RubyClientCodegen();
        codegen.setModuleName("OnlinePetstore");
        codegen.setOpenAPI(openAPI);
        final String path = "/pet/{petId}";

        final Operation p = openAPI.getPaths().get(path).getPost();
        final CodegenOperation op = codegen.fromOperation(path, "post", p, null);

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
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/petstore-nullable.yaml");
        final RubyClientCodegen codegen = new RubyClientCodegen();
        codegen.setModuleName("OnlinePetstore");
        codegen.setOpenAPI(openAPI);
        final String path = "/pet/{petId}";

        final Operation p = openAPI.getPaths().get(path).getPost();
        final CodegenOperation op = codegen.fromOperation(path, "post", p, null);

        // path parameter x-nullable test
        Assert.assertEquals(op.pathParams.size(), 1);
        CodegenParameter pp = op.pathParams.get(0);
        Assert.assertTrue(pp.isNullable);

        // form parameter x-nullable test
        Assert.assertEquals(op.formParams.size(), 2);
        CodegenParameter name = op.formParams.get(0);
        Assert.assertFalse(name.isNullable);
        CodegenParameter status = op.formParams.get(1);
        Assert.assertTrue(status.isNullable);
    }

    @Test(description = "test anyOf (OAS3)")
    public void anyOfTest() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/anyOf.yaml");
        final RubyClientCodegen codegen = new RubyClientCodegen();
        codegen.setModuleName("OnlinePetstore");

        final Schema schema = openAPI.getComponents().getSchemas().get("fruit");
        codegen.setOpenAPI(openAPI);
        CodegenModel fruit = codegen.fromModel("Fruit", schema);

        Set<String> anyOf = new TreeSet<String>();
        anyOf.add("Apple");
        anyOf.add("Banana");
        Assert.assertEquals(fruit.anyOf, anyOf);
    }

    @Test(description = "test oneOf (OAS3)")
    public void oneOfTest() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/oneOf.yaml");
        final RubyClientCodegen codegen = new RubyClientCodegen();
        codegen.setModuleName("OnlinePetstore");

        final Schema schema = openAPI.getComponents().getSchemas().get("fruit");
        codegen.setOpenAPI(openAPI);
        CodegenModel fruit = codegen.fromModel("Fruit", schema);

        Set<String> oneOf = new TreeSet<String>();
        oneOf.add("Apple");
        oneOf.add("Banana");
        Assert.assertEquals(fruit.oneOf, oneOf);
    }

    @Test(description = "test allOf (OAS3)")
    public void allOfTest() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/allOf.yaml");
        final RubyClientCodegen codegen = new RubyClientCodegen();
        codegen.setLegacyDiscriminatorBehavior(false);
        codegen.setModuleName("OnlinePetstore");

        final Schema schema = openAPI.getComponents().getSchemas().get("Person");
        codegen.setOpenAPI(openAPI);
        CodegenModel person = codegen.fromModel("Person", schema);
        Assert.assertNotNull(person);

        CodegenDiscriminator codegenDiscriminator = person.getDiscriminator();
        Set<CodegenDiscriminator.MappedModel> mappedModels = new LinkedHashSet<CodegenDiscriminator.MappedModel>();
        mappedModels.add(new CodegenDiscriminator.MappedModel("a", "Adult"));
        mappedModels.add(new CodegenDiscriminator.MappedModel("c", "Child"));
        mappedModels.add(new CodegenDiscriminator.MappedModel("Adult", "Adult"));
        mappedModels.add(new CodegenDiscriminator.MappedModel("Child", "Child"));
        Assert.assertEquals(codegenDiscriminator.getMappedModels(), mappedModels);
    }

    @Test(description = "test allOf (OAS3)")
    public void allOfTestLegacy() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/allOf.yaml");
        final RubyClientCodegen codegen = new RubyClientCodegen();
        // codegen.discriminatorExplicitMappingVerbose == false by default
        codegen.setModuleName("OnlinePetstore");

        final Schema schema = openAPI.getComponents().getSchemas().get("Person");
        codegen.setOpenAPI(openAPI);
        CodegenModel person = codegen.fromModel("Person", schema);
        Assert.assertNotNull(person);

        CodegenDiscriminator codegenDiscriminator = person.getDiscriminator();
        Set<CodegenDiscriminator.MappedModel> mappedModels = new LinkedHashSet<CodegenDiscriminator.MappedModel>();
        mappedModels.add(new CodegenDiscriminator.MappedModel("a", "Adult"));
        mappedModels.add(new CodegenDiscriminator.MappedModel("c", "Child"));
        Assert.assertEquals(codegenDiscriminator.getMappedModels(), mappedModels);
    }

    @Test(description = "test allOf with only allOf and duplicated properties(OAS3)")
    public void allOfDuplicatedPropertiesTest() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/allOfDuplicatedProperties.yaml");
        final RubyClientCodegen codegen = new RubyClientCodegen();
        codegen.setModuleName("OnlinePetstore");

        final Schema schema = openAPI.getComponents().getSchemas().get("ModelC");
        codegen.setOpenAPI(openAPI);
        CodegenModel modelC = codegen.fromModel("ModelC", schema);
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


    @Test(description = "test allOf with discriminator and duplicated properties(OAS3) for Child model")
    public void allOfMappingDuplicatedPropertiesTestForChild() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/allOfMappingDuplicatedProperties.yaml");
        final RubyClientCodegen codegen = new RubyClientCodegen();
        codegen.setModuleName("OnlinePetstore");

        final Schema schema = openAPI.getComponents().getSchemas().get("Child");
        codegen.setOpenAPI(openAPI);
        CodegenModel child = codegen.fromModel("Child", schema);
        Assert.assertNotNull(child);

        // to test allVars (without parent's properties)
        List<String> allVars  =
                child.getAllVars().stream()
                        .map(CodegenProperty::getName)
                        .collect(Collectors.toList());
        List<String> allVarsExpected = Arrays.asList(
                "age",
                "first_name",
                "_type",
                "last_name",
                "duplicated_optional",
                "duplicated_required",
                "person_required"
        );
        Assert.assertEquals(allVars.size(), allVarsExpected.size());
        Assert.assertTrue(allVars.containsAll(allVarsExpected));

        // to test vars (without parent's properties)
        List<String> vars  =
                child.getVars().stream()
                        .map(CodegenProperty::getName)
                        .collect(Collectors.toList());
        List<String> varsExpected = Arrays.asList(
                "age",
                "first_name"
        );
        Assert.assertEquals(vars.size(), varsExpected.size());
        Assert.assertTrue(vars.containsAll(varsExpected));

        // to test requiredVars
        List<String> requiredVars  =
                child.getRequiredVars().stream()
                        .map(CodegenProperty::getName)
                        .collect(Collectors.toList());
        List<String> requiredVarsExpected = Arrays.asList(
                "duplicated_required",
                "person_required"
                );
        Assert.assertEquals(vars.size(), requiredVarsExpected.size());
        Assert.assertTrue(requiredVars.containsAll(requiredVarsExpected));
    }

    @Test(description = "test allOf with discriminator and duplicated properties(OAS3) for Adult model")
    public void allOfMappingDuplicatedPropertiesTestForAdult() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/allOfMappingDuplicatedProperties.yaml");
        final RubyClientCodegen codegen = new RubyClientCodegen();
        codegen.setModuleName("OnlinePetstore");

        final Schema schema = openAPI.getComponents().getSchemas().get("Adult");
        codegen.setOpenAPI(openAPI);
        CodegenModel adult = codegen.fromModel("Adult", schema);
        Assert.assertNotNull(adult);

        // to test allVars (without parent's properties)
        Assert.assertEquals(adult.getAllVars().size(), 8);

        CodegenProperty cp0 = adult.getAllVars().get(0);
        Assert.assertEquals(cp0.name, "_type");

        CodegenProperty cp1 = adult.getAllVars().get(1);
        Assert.assertEquals(cp1.name, "last_name");

        CodegenProperty cp2 = adult.getAllVars().get(2);
        Assert.assertEquals(cp2.name, "first_name");

        CodegenProperty cp3 = adult.getAllVars().get(3);
        Assert.assertEquals(cp3.name, "duplicated_optional");

        CodegenProperty cp4 = adult.getAllVars().get(4);
        Assert.assertEquals(cp4.name, "duplicated_required");

        CodegenProperty cp5 = adult.getAllVars().get(5);
        Assert.assertEquals(cp5.name, "person_required");

        CodegenProperty cp6 = adult.getAllVars().get(6);
        Assert.assertEquals(cp6.name, "children");

        CodegenProperty cp7 = adult.getAllVars().get(7);
        Assert.assertEquals(cp7.name, "adult_required");

        // to test vars (without parent's properties)
        Assert.assertEquals(adult.getVars().size(), 4);

        cp0 = adult.getVars().get(0);
        Assert.assertEquals(cp0.name, "duplicated_optional");

        cp1 = adult.getVars().get(1);
        Assert.assertEquals(cp1.name, "duplicated_required");

        cp2 = adult.getVars().get(2);
        Assert.assertEquals(cp2.name, "children");

        // to test requiredVars
        Assert.assertEquals(adult.getRequiredVars().size(), 2);

        cp0 = adult.getRequiredVars().get(0);
        Assert.assertEquals(cp0.name, "duplicated_required");

        cp1 = adult.getRequiredVars().get(1);
        Assert.assertEquals(cp1.name, "person_required");
    }

    @Test(description = "test allOf composition")
    public void allOfCompositionTest() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/allOf_composition.yaml");
        final RubyClientCodegen codegen = new RubyClientCodegen();
        codegen.setModuleName("OnlinePetstore");

        final Schema schema = openAPI.getComponents().getSchemas().get("SuperMan");
        codegen.setOpenAPI(openAPI);
        CodegenModel superMan = codegen.fromModel("SuperMan", schema);
        Assert.assertNotNull(superMan);

        // to test all properties
        Assert.assertEquals(superMan.getVars().size(), 6);
        Assert.assertEquals(superMan.getAllVars().size(), 6);
        Assert.assertEquals(superMan.getMandatory().size(), 3);
        Assert.assertEquals(superMan.getAllMandatory().size(), 3);

        CodegenProperty cp0 = superMan.getVars().get(0);
        Assert.assertEquals(cp0.name, "id");
        Assert.assertTrue(cp0.required);

        CodegenProperty cp1 = superMan.getVars().get(1);
        Assert.assertEquals(cp1.name, "name");
        Assert.assertFalse(cp1.required);

        CodegenProperty cp2 = superMan.getVars().get(2);
        Assert.assertEquals(cp2.name, "reward");
        Assert.assertFalse(cp2.required);

        CodegenProperty cp3 = superMan.getVars().get(3);
        Assert.assertEquals(cp3.name, "origin");
        Assert.assertTrue(cp3.required);

        CodegenProperty cp4 = superMan.getVars().get(4);
        Assert.assertEquals(cp4.name, "category");
        Assert.assertFalse(cp4.required);

        CodegenProperty cp5 = superMan.getVars().get(5);
        Assert.assertEquals(cp5.name, "level");
        Assert.assertTrue(cp5.required);

        CodegenProperty cp6 = superMan.getAllVars().get(0);
        Assert.assertEquals(cp6.name, "id");
        Assert.assertTrue(cp6.required);

        CodegenProperty cp7 = superMan.getAllVars().get(1);
        Assert.assertEquals(cp7.name, "name");
        Assert.assertFalse(cp7.required);

        CodegenProperty cp8 = superMan.getAllVars().get(2);
        Assert.assertEquals(cp8.name, "reward");
        Assert.assertFalse(cp8.required);

        CodegenProperty cp9 = superMan.getAllVars().get(3);
        Assert.assertEquals(cp9.name, "origin");
        Assert.assertTrue(cp9.required);

        CodegenProperty cp10 = superMan.getAllVars().get(4);
        Assert.assertEquals(cp10.name, "category");
        Assert.assertFalse(cp10.required);

        CodegenProperty cp11 = superMan.getAllVars().get(5);
        Assert.assertEquals(cp11.name, "level");
        Assert.assertTrue(cp11.required);

    }


    @Test(description = "test example string imported from x-example parameterr (OAS2)")
    public void exampleStringFromExampleParameterOAS2Test() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/petstore-nullable.yaml");
        final RubyClientCodegen codegen = new RubyClientCodegen();
        codegen.setModuleName("OnlinePetstore");
        codegen.setOpenAPI(openAPI);
        final String path = "/store/order/{orderId}";

        final Operation p = openAPI.getPaths().get(path).getDelete();
        final CodegenOperation op = codegen.fromOperation(path, "delete", p, null);

        CodegenParameter pp = op.pathParams.get(0);
        Assert.assertEquals(pp.example, "'orderid123'");
    }

    @Test(description = "test example string imported from example in schema (OAS3)")
    public void exampleStringFromXExampleParameterOAS3Test() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/petstore_oas3_test.yaml");
        final RubyClientCodegen codegen = new RubyClientCodegen();
        codegen.setModuleName("OnlinePetstore");
        codegen.setOpenAPI(openAPI);
        final String path = "/store/order/{orderId}";

        final Operation p = openAPI.getPaths().get(path).getDelete();
        final CodegenOperation op = codegen.fromOperation(path, "delete", p, null);

        CodegenParameter pp = op.pathParams.get(0);
        Assert.assertEquals(pp.example, "'orderid123'");
    }

    /**
     * We want to make sure that all Regex patterns:
     * - Start with / so Ruby know this is a regex pattern
     * - Have a second / that may be added to end if only 1 exists at start
     * - If there are 2 / in pattern then don't add any more
     */
    @Test(description = "test regex patterns")
    public void exampleRegexParameterValidationOAS3Test() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/test_regex.yaml");
        final RubyClientCodegen codegen = new RubyClientCodegen();
        codegen.setOpenAPI(openAPI);
        final String path = "/ping";
        final Operation p = openAPI.getPaths().get(path).getGet();
        final CodegenOperation op = codegen.fromOperation(path, "get", p, null);
        // pattern_no_forward_slashes '^pattern$'
        Assert.assertEquals(op.allParams.get(0).pattern, "/^pattern$/");
        // pattern_two_slashes '/^pattern$/i'
        Assert.assertEquals(op.allParams.get(1).pattern, "/^pattern$/i");
        // pattern_dont_escape_backslash '/^pattern\d{3}$/i' NOTE: the double \ is to escape \ in string but is read as single \
        Assert.assertEquals(op.allParams.get(2).pattern, "/^pattern\\d{3}$/i");
    }
}
