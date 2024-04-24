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

import com.google.common.collect.ImmutableMap;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.io.FileUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.RubyClientCodegen;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.*;
import java.util.stream.Collectors;

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

        Assertions.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assertions.assertEquals(codegen.isHideGenerationTimestamp(), true);
        Assertions.assertEquals(codegen.modelPackage(), "models");
        Assertions.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), null);
        Assertions.assertEquals(codegen.apiPackage(), "api");
        Assertions.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), null);
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final RubyClientCodegen codegen = new RubyClientCodegen();
        codegen.setHideGenerationTimestamp(false);
        codegen.processOpts();

        Assertions.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assertions.assertEquals(codegen.isHideGenerationTimestamp(), false);
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final RubyClientCodegen codegen = new RubyClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, false);
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "ruby-models");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "ruby-api");
        codegen.processOpts();

        Assertions.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assertions.assertEquals(codegen.isHideGenerationTimestamp(), false);
        Assertions.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "ruby-models");
        Assertions.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "ruby-api");
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
        Assertions.assertEquals(op.formParams.size(), 2);
        CodegenParameter fp = op.formParams.get(0);
        Assertions.assertEquals(fp.dataType, "Array<String>");
        Assertions.assertEquals(fp.datatypeWithEnum, "Array<ENUM_FORM_STRING_ARRAY>");
        Assertions.assertEquals(fp.enumName, "ENUM_FORM_STRING_ARRAY");
    }

    @Test(description = "test example value for body parameter")
    public void bodyParameterTest() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml");
        final RubyClientCodegen codegen = new RubyClientCodegen();
        codegen.setModuleName("OnlinePetstore");
        codegen.setOpenAPI(openAPI);

        final String path = "/pet";
        final Operation p = openAPI.getPaths().get(path).getPost();
        Schema schema = openAPI.getComponents().getSchemas().get("Pet");
        CodegenModel model = codegen.fromModel("Pet", schema);
        ModelMap modelMap = new ModelMap();
        modelMap.setModel(model);
        final CodegenOperation op = codegen.fromOperation(path, "post", p, null);

        OperationMap operations = new OperationMap();
        operations.setOperation(op);
        OperationsMap objs = new OperationsMap();
        objs.setOperation(operations);
        objs.setImports(new ArrayList<>());
        objs = codegen.postProcessOperationsWithModels(objs, Collections.singletonList(modelMap));
        CodegenOperation postProcessedOp = objs.getOperations().getOperation().get(0);
        Assertions.assertEquals(postProcessedOp.bodyParams.size(), 1);
        CodegenParameter bp = postProcessedOp.bodyParams.get(0);
        Assertions.assertEquals(bp.vendorExtensions.get("x-ruby-example"), "OnlinePetstore::Pet.new({name: 'doggie', photo_urls: ['photo_urls_example']})");
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
        Assertions.assertTrue(cp0.isNullable);

        CodegenProperty cp1 = nullablePet.getVars().get(1);
        Assertions.assertFalse(cp1.isNullable);

        CodegenProperty cp2 = nullablePet.getVars().get(2);
        Assertions.assertTrue(cp2.isNullable);

        CodegenProperty cp3 = nullablePet.getVars().get(3);
        Assertions.assertTrue(cp3.isNullable);

        CodegenProperty cp4 = nullablePet.getVars().get(4);
        Assertions.assertFalse(cp4.isNullable);

        CodegenProperty cp5 = nullablePet.getVars().get(5);
        Assertions.assertTrue(cp5.isNullable);
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

        Assertions.assertNotNull(nullablePet);
        Assertions.assertEquals(nullablePet.getVars().size(), 6);
        CodegenProperty cp0 = nullablePet.getVars().get(0);
        Assertions.assertFalse(cp0.isNullable);
        Assertions.assertEquals(cp0.name, "id");

        CodegenProperty cp1 = nullablePet.getVars().get(1);
        Assertions.assertFalse(cp1.isNullable);
        Assertions.assertEquals(cp1.name, "category");

        CodegenProperty cp2 = nullablePet.getVars().get(2);
        Assertions.assertFalse(cp2.isNullable);
        Assertions.assertEquals(cp2.name, "name");

        CodegenProperty cp3 = nullablePet.getVars().get(3);
        Assertions.assertFalse(cp3.isNullable);
        Assertions.assertEquals(cp3.name, "photo_urls");

        CodegenProperty cp4 = nullablePet.getVars().get(4);
        Assertions.assertFalse(cp4.isNullable);
        Assertions.assertEquals(cp4.name, "tags");

        CodegenProperty cp5 = nullablePet.getVars().get(5);
        Assertions.assertFalse(cp5.isNullable);
        Assertions.assertEquals(cp5.name, "status");

        // test allVars
        Assertions.assertEquals(nullablePet.getAllVars().size(), 6);
        cp0 = nullablePet.getVars().get(0);
        Assertions.assertFalse(cp0.isNullable);
        Assertions.assertEquals(cp0.name, "id");

        cp1 = nullablePet.getVars().get(1);
        Assertions.assertFalse(cp1.isNullable);
        Assertions.assertEquals(cp1.name, "category");

        cp2 = nullablePet.getVars().get(2);
        Assertions.assertFalse(cp2.isNullable);
        Assertions.assertEquals(cp2.name, "name");

        cp3 = nullablePet.getVars().get(3);
        Assertions.assertFalse(cp3.isNullable);
        Assertions.assertEquals(cp3.name, "photo_urls");

        cp4 = nullablePet.getVars().get(4);
        Assertions.assertFalse(cp4.isNullable);
        Assertions.assertEquals(cp4.name, "tags");

        cp5 = nullablePet.getVars().get(5);
        Assertions.assertFalse(cp5.isNullable);
        Assertions.assertEquals(cp5.name, "status");

        // test requiredVars
        Assertions.assertEquals(nullablePet.getRequiredVars().size(), 2);
        cp0 = nullablePet.getRequiredVars().get(0);
        Assertions.assertFalse(cp0.isNullable);
        Assertions.assertEquals(cp0.name, "name");

        cp1 = nullablePet.getRequiredVars().get(1);
        Assertions.assertFalse(cp1.isNullable);
        Assertions.assertEquals(cp1.name, "photo_urls");

        // test mandatory
        Set<String> mandatory = new TreeSet<String>();
        mandatory.add("name");
        mandatory.add("photo_urls");
        Assertions.assertEquals(nullablePet.getMandatory(), mandatory);

        // test allMandatory
        Set<String> allMandatory = new TreeSet<String>();
        allMandatory.add("name");
        allMandatory.add("photo_urls");
        Assertions.assertEquals(nullablePet.getAllMandatory(), allMandatory);
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

        Assertions.assertEquals(op.pathParams.size(), 1);
        CodegenParameter pp = op.pathParams.get(0);
        Assertions.assertTrue(pp.isNullable);

        Assertions.assertEquals(op.formParams.size(), 2);
        CodegenParameter name = op.formParams.get(0);
        Assertions.assertFalse(name.isNullable);
        CodegenParameter status = op.formParams.get(1);
        Assertions.assertTrue(status.isNullable);
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
        Assertions.assertEquals(op.pathParams.size(), 1);
        CodegenParameter pp = op.pathParams.get(0);
        Assertions.assertTrue(pp.isNullable);

        // form parameter x-nullable test
        Assertions.assertEquals(op.formParams.size(), 2);
        CodegenParameter name = op.formParams.get(0);
        Assertions.assertFalse(name.isNullable);
        CodegenParameter status = op.formParams.get(1);
        Assertions.assertTrue(status.isNullable);
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
        Assertions.assertEquals(fruit.anyOf, anyOf);
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
        Assertions.assertEquals(fruit.oneOf, oneOf);
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
        Assertions.assertNotNull(person);

        CodegenDiscriminator codegenDiscriminator = person.getDiscriminator();
        Set<CodegenDiscriminator.MappedModel> mappedModels = new LinkedHashSet<CodegenDiscriminator.MappedModel>();
        mappedModels.add(new CodegenDiscriminator.MappedModel("a", "Adult", true));
        mappedModels.add(new CodegenDiscriminator.MappedModel("c", "Child", true));
        Assertions.assertEquals(codegenDiscriminator.getMappedModels(), mappedModels);
    }

    @Test(description = "test allOf (OAS3)")
    public void allOfTestLegacy() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/allOf.yaml");
        final RubyClientCodegen codegen = new RubyClientCodegen();
        // codegen.setLegacyDiscriminatorBehavior(false) by default
        codegen.setModuleName("OnlinePetstore");

        final Schema schema = openAPI.getComponents().getSchemas().get("Person");
        codegen.setOpenAPI(openAPI);
        CodegenModel person = codegen.fromModel("Person", schema);
        Assertions.assertNotNull(person);

        CodegenDiscriminator codegenDiscriminator = person.getDiscriminator();
        Set<CodegenDiscriminator.MappedModel> mappedModels = new LinkedHashSet<CodegenDiscriminator.MappedModel>();
        mappedModels.add(new CodegenDiscriminator.MappedModel("a", "Adult", true));
        mappedModels.add(new CodegenDiscriminator.MappedModel("c", "Child", true));
        Assertions.assertEquals(codegenDiscriminator.getMappedModels(), mappedModels);
    }

    @Test(description = "test allOf with only allOf and duplicated properties(OAS3)")
    public void allOfDuplicatedPropertiesTest() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/allOfDuplicatedProperties.yaml");
        final RubyClientCodegen codegen = new RubyClientCodegen();
        codegen.setModuleName("OnlinePetstore");

        final Schema schema = openAPI.getComponents().getSchemas().get("ModelC");
        codegen.setOpenAPI(openAPI);
        CodegenModel modelC = codegen.fromModel("ModelC", schema);
        Assertions.assertNotNull(modelC);
        Assertions.assertEquals(modelC.getVars().size(), 5);

        CodegenProperty cp0 = modelC.getVars().get(0);
        Assertions.assertEquals(cp0.name, "foo");

        CodegenProperty cp1 = modelC.getVars().get(1);
        Assertions.assertEquals(cp1.name, "duplicated_optional");

        CodegenProperty cp2 = modelC.getVars().get(2);
        Assertions.assertEquals(cp2.name, "duplicated_required");

        CodegenProperty cp3 = modelC.getVars().get(3);
        Assertions.assertEquals(cp3.name, "bar");

        CodegenProperty cp4 = modelC.getVars().get(4);
        Assertions.assertEquals(cp4.name, "baz");
    }


    @Test(description = "test allOf with discriminator and duplicated properties(OAS3) for Child model")
    public void allOfMappingDuplicatedPropertiesTestForChild() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/allOfMappingDuplicatedProperties.yaml");
        final RubyClientCodegen codegen = new RubyClientCodegen();
        codegen.setModuleName("OnlinePetstore");

        final Schema schema = openAPI.getComponents().getSchemas().get("Child");
        codegen.setOpenAPI(openAPI);
        CodegenModel child = codegen.fromModel("Child", schema);
        Assertions.assertNotNull(child);

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
        Assertions.assertEquals(allVars.size(), allVarsExpected.size());
        Assertions.assertTrue(allVars.containsAll(allVarsExpected));

        // to test vars (without parent's properties)
        List<String> vars  =
                child.getVars().stream()
                        .map(CodegenProperty::getName)
                        .collect(Collectors.toList());
        List<String> varsExpected = Arrays.asList(
                "age",
                "first_name"
        );
        Assertions.assertEquals(vars.size(), varsExpected.size());
        Assertions.assertTrue(vars.containsAll(varsExpected));

        // to test requiredVars
        List<String> requiredVars  =
                child.getRequiredVars().stream()
                        .map(CodegenProperty::getName)
                        .collect(Collectors.toList());
        List<String> requiredVarsExpected = Arrays.asList(
                "duplicated_required",
                "person_required"
                );
        Assertions.assertEquals(vars.size(), requiredVarsExpected.size());
        Assertions.assertTrue(requiredVars.containsAll(requiredVarsExpected));
    }

    @Test(description = "test allOf with discriminator and duplicated properties(OAS3) for Adult model")
    public void allOfMappingDuplicatedPropertiesTestForAdult() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/allOfMappingDuplicatedProperties.yaml");
        final RubyClientCodegen codegen = new RubyClientCodegen();
        codegen.setModuleName("OnlinePetstore");

        final Schema schema = openAPI.getComponents().getSchemas().get("Adult");
        codegen.setOpenAPI(openAPI);
        CodegenModel adult = codegen.fromModel("Adult", schema);
        Assertions.assertNotNull(adult);

        // to test allVars (without parent's properties)
        Assertions.assertEquals(adult.getAllVars().size(), 8);

        CodegenProperty cp0 = adult.getAllVars().get(0);
        Assertions.assertEquals(cp0.name, "_type");

        CodegenProperty cp1 = adult.getAllVars().get(1);
        Assertions.assertEquals(cp1.name, "last_name");

        CodegenProperty cp2 = adult.getAllVars().get(2);
        Assertions.assertEquals(cp2.name, "first_name");

        CodegenProperty cp3 = adult.getAllVars().get(3);
        Assertions.assertEquals(cp3.name, "duplicated_optional");

        CodegenProperty cp4 = adult.getAllVars().get(4);
        Assertions.assertEquals(cp4.name, "duplicated_required");

        CodegenProperty cp5 = adult.getAllVars().get(5);
        Assertions.assertEquals(cp5.name, "person_required");

        CodegenProperty cp6 = adult.getAllVars().get(6);
        Assertions.assertEquals(cp6.name, "children");

        CodegenProperty cp7 = adult.getAllVars().get(7);
        Assertions.assertEquals(cp7.name, "adult_required");

        // to test vars (without parent's properties)
        Assertions.assertEquals(adult.getVars().size(), 4);

        cp0 = adult.getVars().get(0);
        Assertions.assertEquals(cp0.name, "duplicated_optional");

        cp1 = adult.getVars().get(1);
        Assertions.assertEquals(cp1.name, "duplicated_required");

        cp2 = adult.getVars().get(2);
        Assertions.assertEquals(cp2.name, "children");

        // to test requiredVars
        Assertions.assertEquals(adult.getRequiredVars().size(), 2);

        cp0 = adult.getRequiredVars().get(0);
        Assertions.assertEquals(cp0.name, "duplicated_required");

        cp1 = adult.getRequiredVars().get(1);
        Assertions.assertEquals(cp1.name, "person_required");
    }

    @Test(description = "test allOf composition")
    public void allOfCompositionTest() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/allOf_composition.yaml");
        final RubyClientCodegen codegen = new RubyClientCodegen();
        codegen.setModuleName("OnlinePetstore");

        final Schema schema = openAPI.getComponents().getSchemas().get("SuperMan");
        codegen.setOpenAPI(openAPI);
        CodegenModel superMan = codegen.fromModel("SuperMan", schema);
        Assertions.assertNotNull(superMan);

        // to test all properties
        Assertions.assertEquals(superMan.getVars().size(), 6);
        Assertions.assertEquals(superMan.getAllVars().size(), 6);
        Assertions.assertEquals(superMan.getMandatory().size(), 3);
        Assertions.assertEquals(superMan.getAllMandatory().size(), 3);

        CodegenProperty cp0 = superMan.getVars().get(0);
        Assertions.assertEquals(cp0.name, "id");
        Assertions.assertTrue(cp0.required);

        CodegenProperty cp1 = superMan.getVars().get(1);
        Assertions.assertEquals(cp1.name, "name");
        Assertions.assertFalse(cp1.required);

        CodegenProperty cp2 = superMan.getVars().get(2);
        Assertions.assertEquals(cp2.name, "reward");
        Assertions.assertFalse(cp2.required);

        CodegenProperty cp3 = superMan.getVars().get(3);
        Assertions.assertEquals(cp3.name, "origin");
        Assertions.assertTrue(cp3.required);

        CodegenProperty cp4 = superMan.getVars().get(4);
        Assertions.assertEquals(cp4.name, "category");
        Assertions.assertFalse(cp4.required);

        CodegenProperty cp5 = superMan.getVars().get(5);
        Assertions.assertEquals(cp5.name, "level");
        Assertions.assertTrue(cp5.required);

        CodegenProperty cp6 = superMan.getAllVars().get(0);
        Assertions.assertEquals(cp6.name, "id");
        Assertions.assertTrue(cp6.required);

        CodegenProperty cp7 = superMan.getAllVars().get(1);
        Assertions.assertEquals(cp7.name, "name");
        Assertions.assertFalse(cp7.required);

        CodegenProperty cp8 = superMan.getAllVars().get(2);
        Assertions.assertEquals(cp8.name, "reward");
        Assertions.assertFalse(cp8.required);

        CodegenProperty cp9 = superMan.getAllVars().get(3);
        Assertions.assertEquals(cp9.name, "origin");
        Assertions.assertTrue(cp9.required);

        CodegenProperty cp10 = superMan.getAllVars().get(4);
        Assertions.assertEquals(cp10.name, "category");
        Assertions.assertFalse(cp10.required);

        CodegenProperty cp11 = superMan.getAllVars().get(5);
        Assertions.assertEquals(cp11.name, "level");
        Assertions.assertTrue(cp11.required);

    }


    @Test(description = "test example string imported from x-example parameter (OAS2)")
    public void exampleStringFromExampleParameterOAS2Test() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/petstore-nullable.yaml");
        final RubyClientCodegen codegen = new RubyClientCodegen();
        codegen.setModuleName("OnlinePetstore");
        codegen.setOpenAPI(openAPI);
        final String path = "/store/order/{orderId}";

        final Operation p = openAPI.getPaths().get(path).getDelete();
        final CodegenOperation op = codegen.fromOperation(path, "delete", p, null);

        OperationMap operations = new OperationMap();
        operations.setOperation(op);
        OperationsMap objs = new OperationsMap();
        objs.setOperation(operations);
        objs.setImports(new ArrayList<>());
        objs = codegen.postProcessOperationsWithModels(objs, Collections.emptyList());
        CodegenOperation postProcessedOp = objs.getOperations().getOperation().get(0);

        CodegenParameter pp = postProcessedOp.pathParams.get(0);
        Assertions.assertEquals(pp.vendorExtensions.get("x-ruby-example"), "'orderid123'");
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

        OperationMap operations = new OperationMap();
        operations.setOperation(op);
        OperationsMap objs = new OperationsMap();
        objs.setOperation(operations);
        objs.setImports(new ArrayList<>());
        objs = codegen.postProcessOperationsWithModels(objs, Collections.emptyList());
        CodegenOperation postProcessedOp = objs.getOperations().getOperation().get(0);

        CodegenParameter pp = postProcessedOp.pathParams.get(0);
        Assertions.assertEquals(pp.vendorExtensions.get("x-ruby-example"), "'orderid123'");
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
        Assertions.assertEquals(op.allParams.get(0).pattern, "/^pattern$/");
        // pattern_two_slashes '/^pattern$/i'
        Assertions.assertEquals(op.allParams.get(1).pattern, "/^pattern$/i");
        // pattern_dont_escape_backslash '/^pattern\d{3}$/i' NOTE: the double \ is to escape \ in string but is read as single \
        Assertions.assertEquals(op.allParams.get(2).pattern, "/^pattern\\d{3}$/i");
    }

    /**
     * We want to make sure that the type mapping works as expect
     */
    @Test(description = "test type mapping to handle special format, e.g. string+special")
    public void typeMappingTest() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/type_mapping_test.yaml");
        final RubyClientCodegen codegen = new RubyClientCodegen();
        codegen.typeMapping().put("string+special", "VerySpecialStringInRuby");

        codegen.setOpenAPI(openAPI);
        final String path = "/animals";
        final Operation p = openAPI.getPaths().get(path).getGet();
        final CodegenOperation op = codegen.fromOperation(path, "get", p, null);
        Assertions.assertEquals(op.allParams.get(0).dataType, "VerySpecialStringInRuby");

        final Schema schema = openAPI.getComponents().getSchemas().get("Animal");
        codegen.setOpenAPI(openAPI);
        CodegenModel animal = codegen.fromModel("Animal", schema);
        Assertions.assertNotNull(animal);
        CodegenProperty cp2 = animal.getVars().get(2);
        Assertions.assertEquals(cp2.name, "mapping_test");
        Assertions.assertFalse(cp2.required);
        Assertions.assertEquals(cp2.dataType, "VerySpecialStringInRuby");
    }

    @Test(description = "test regex patterns")
    public void testRegularExpressionOpenAPISchemaVersion3() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/issue_1517.yaml");
        final RubyClientCodegen codegen = new RubyClientCodegen();
        codegen.setOpenAPI(openAPI);
        final String path = "/ping";
        final Operation p = openAPI.getPaths().get(path).getGet();
        final CodegenOperation op = codegen.fromOperation(path, "get", p, null);
        // pattern_no_forward_slashes '^pattern$'
        Assertions.assertEquals(op.allParams.get(0).pattern, "/^pattern$/");
        // pattern_two_slashes '/^pattern$/'
        Assertions.assertEquals(op.allParams.get(1).pattern, "/^pattern$/");
        // pattern_dont_escape_backslash '/^pattern\d{3}$/'
        Assertions.assertEquals(op.allParams.get(2).pattern, "/^pattern\\d{3}$/");
        // pattern_dont_escape_escaped_forward_slash '/^pattern\/\d{3}$/'
        Assertions.assertEquals(op.allParams.get(3).pattern, "/^pattern\\/\\d{3}$/");
        // pattern_escape_unescaped_forward_slash '^pattern/\d{3}$'
        Assertions.assertEquals(op.allParams.get(4).pattern, "/^pattern\\/\\d{3}$/");
        // pattern_with_modifiers '/^pattern\d{3}$/i
        Assertions.assertEquals(op.allParams.get(5).pattern, "/^pattern\\d{3}$/i");
        // not testing pattern_with_backslash_after_bracket '/^[\pattern\d{3}$/i'
        // as "/^[\\pattern\\d{3}$/i" is invalid regex because [ is not escaped and there is no closing ]
        // Assertions.assertEquals(op.allParams.get(6).pattern, "/^[\\pattern\\d{3}$/i");
        // alternation_with_forward_slash '/ax$|/bx$'
        Assertions.assertEquals(op.allParams.get(7).pattern, "/ax$|/bx$");
        // patten_starts_ends_with_slash '/root/'
        Assertions.assertEquals(op.allParams.get(8).pattern, "/root/");
    }
}
