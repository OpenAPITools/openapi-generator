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

package org.openapitools.codegen.go;

import com.google.common.collect.Sets;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.parser.util.SchemaTypeUtil;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.InlineModelResolver;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.GoClientCodegen;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;

@SuppressWarnings("static-method")
public class GoModelTest {

    @Test(description = "convert a simple Go model")
    public void simpleModelTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperty("id", new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperty("name", new StringSchema())
                .addProperty("createdAt", new DateTimeSchema())
                .addRequiredItem("id")
                .addRequiredItem("name");
        final DefaultCodegen codegen = new GoClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 3);
        Assert.assertEquals(cm.imports.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "id");
        Assert.assertEquals(property1.dataType, "int64");
        Assert.assertEquals(property1.name, "Id");
        Assert.assertNull(property1.defaultValue);
        Assert.assertEquals(property1.baseType, "int64");
        Assert.assertTrue(property1.required);
        Assert.assertTrue(property1.isPrimitiveType);

        final CodegenProperty property2 = cm.vars.get(1);
        Assert.assertEquals(property2.baseName, "name");
        Assert.assertEquals(property2.dataType, "string");
        Assert.assertEquals(property2.name, "Name");
        Assert.assertNull(property2.defaultValue);
        Assert.assertEquals(property2.baseType, "string");
        Assert.assertTrue(property2.required);
        Assert.assertTrue(property2.isPrimitiveType);

        final CodegenProperty property3 = cm.vars.get(2);
        Assert.assertEquals(property3.baseName, "createdAt");
        Assert.assertEquals(property3.complexType, "time.Time");
        Assert.assertEquals(property3.dataType, "time.Time");
        Assert.assertEquals(property3.name, "CreatedAt");
        Assert.assertNull(property3.defaultValue);
        Assert.assertEquals(property3.baseType, "time.Time");
        Assert.assertFalse(property3.required);
    }

    @Test(description = "convert a model with list property")
    public void listPropertyTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperty("id", new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperty("urls", new ArraySchema()
                        .items(new StringSchema()))
                .addRequiredItem("id");
        final DefaultCodegen codegen = new GoClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 2);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "id");
        Assert.assertEquals(property1.dataType, "int64");
        Assert.assertEquals(property1.name, "Id");
        Assert.assertNull(property1.defaultValue);
        Assert.assertEquals(property1.baseType, "int64");
        Assert.assertTrue(property1.required);
        Assert.assertTrue(property1.isPrimitiveType);

        final CodegenProperty property2 = cm.vars.get(1);
        Assert.assertEquals(property2.baseName, "urls");
        Assert.assertEquals(property2.dataType, "[]string");
        Assert.assertEquals(property2.name, "Urls");
        Assert.assertEquals(property2.baseType, "array");
        Assert.assertEquals(property2.containerType, "array");
        Assert.assertFalse(property2.required);
        Assert.assertTrue(property2.isPrimitiveType);
    }

    @Test(description = "convert a model with a map property")
    public void mapPropertyTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperty("translations", new MapSchema()
                        .additionalProperties(new StringSchema()))
                .addRequiredItem("id");
        final DefaultCodegen codegen = new GoClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "translations");
        Assert.assertEquals(property1.dataType, "map[string]string");
        Assert.assertEquals(property1.name, "Translations");
        Assert.assertEquals(property1.baseType, "map");
        Assert.assertEquals(property1.containerType, "map");
        Assert.assertFalse(property1.required);
        Assert.assertTrue(property1.isContainer);
        Assert.assertTrue(property1.isPrimitiveType);
    }

    @Test(description = "convert a model with complex property")
    public void complexPropertyTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperty("children", new Schema().$ref("#/definitions/Children"));
        final DefaultCodegen codegen = new GoClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "children");
        Assert.assertEquals(property1.dataType, "Children");
        Assert.assertEquals(property1.name, "Children");
        Assert.assertEquals(property1.baseType, "Children");
        Assert.assertFalse(property1.required);
    }

    @Test(description = "convert a model with complex list property")
    public void complexListProperty() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperty("children", new ArraySchema()
                        .items(new Schema().$ref("#/definitions/Children")));
        final DefaultCodegen codegen = new GoClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "children");
        Assert.assertEquals(property1.dataType, "[]Children");
        Assert.assertEquals(property1.name, "Children");
        Assert.assertEquals(property1.baseType, "array");
        Assert.assertEquals(property1.containerType, "array");
        Assert.assertFalse(property1.required);
        Assert.assertTrue(property1.isContainer);
    }

    @Test(description = "convert a model with complex map property")
    public void complexMapProperty() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperty("children", new MapSchema()
                        .additionalProperties(new Schema().$ref("#/definitions/Children")));
        final DefaultCodegen codegen = new GoClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);
        Assert.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("Children")).size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "children");
        Assert.assertEquals(property1.complexType, "Children");
        Assert.assertEquals(property1.dataType, "map[string]Children");
        Assert.assertEquals(property1.name, "Children");
        Assert.assertEquals(property1.baseType, "map");
        Assert.assertEquals(property1.containerType, "map");
        Assert.assertFalse(property1.required);
        Assert.assertTrue(property1.isContainer);
    }

    @Test(description = "convert an array model")
    public void arrayModelTest() {
        final Schema model = new ArraySchema()
                .items(new Schema().$ref("#/definitions/Children"))
                .description("an array model");
        final DefaultCodegen codegen = new GoClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "an array model");
        Assert.assertEquals(cm.vars.size(), 0);
        Assert.assertEquals(cm.imports.size(), 1);
    }

    @Test(description = "convert a map model")
    public void mapModelTest() {
        final Schema model = new Schema()
                .additionalProperties(new Schema().$ref("#/definitions/Children"))
                .description("a map model");
        final DefaultCodegen codegen = new GoClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a map model");
        Assert.assertEquals(cm.vars.size(), 0);
        Assert.assertEquals(cm.imports.size(), 1);
        Assert.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("Children")).size(), 1);
    }

    @Test(description = "convert file type and file schema models")
    public void filePropertyTest() {
        final DefaultCodegen codegen = new GoClientCodegen();
        final Schema model1 = new Schema().type("file");
        Assert.assertEquals(codegen.getSchemaType(model1), "*os.File");
        Assert.assertEquals(codegen.getTypeDeclaration(model1), "*os.File");

        final Schema model2 = new Schema().$ref("#/definitions/File");
        Assert.assertEquals(codegen.getSchemaType(model2), "File");
        Assert.assertEquals(codegen.getTypeDeclaration(model2), "File");

        final Schema model3 = new Schema().$ref("#/components/schemas/File");
        Assert.assertEquals(codegen.getSchemaType(model3), "File");
        Assert.assertEquals(codegen.getTypeDeclaration(model3), "File");
    }

    @DataProvider(name = "modelNames")
    public static Object[][] primeNumbers() {
        return new Object[][]{
                {"sample", "Sample"},
                {"sample_name", "SampleName"},
                {"sample__name", "SampleName"},
                {"/sample", "Sample"},
                {"\\sample", "Sample"},
                {"sample.name", "SampleName"},
                {"_sample", "Sample"},
        };
    }

    @Test(dataProvider = "modelNames", description = "avoid inner class")
    public void modelNameTest(String name, String expectedName) {
        final Schema model = new Schema();
        final DefaultCodegen codegen = new GoClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema(name, model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel(name, model);

        Assert.assertEquals(cm.name, name);
        Assert.assertEquals(cm.classname, expectedName);
    }

    @DataProvider(name = "modelMappedNames")
    public static Object[][] mappedNames() {
        return new Object[][]{
                {"mapped", "Remapped", "model_remapped.go"},
                {"mapped_underscore", "RemappedUnderscore", "model_remapped_underscore.go"},
        };
    }

    @Test(dataProvider = "modelMappedNames", description = "map model names")
    public void modelNameMappingsTest(String name, String expectedName, String expectedFilename) {
        final Schema model = new Schema();
        final DefaultCodegen codegen = new GoClientCodegen();
        codegen.modelNameMapping().put(name, expectedName);
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema(name, model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel(name, model);

        final String fn = codegen.modelFilename("model.mustache", name, "");
        Assert.assertEquals(fn, File.separator + expectedFilename);

        Assert.assertEquals(cm.name, name);
        Assert.assertEquals(cm.classname, expectedName);
    }

    @Test(description = "test that direct $ref usage does NOT create aliases")
    public void directRefNoAliasTest() {
        final Schema phoneNumberSchema = new Schema()
                .type("object")
                .addProperty("countryCode", new StringSchema())
                .addProperty("number", new StringSchema())
                .addRequiredItem("number");
        
        final Schema personSchema = new Schema()
                .type("object")
                .addProperty("name", new StringSchema())
                .addProperty("mobile", new Schema().$ref("#/components/schemas/PhoneNumber"))
                .addProperty("home", new Schema().$ref("#/components/schemas/PhoneNumber"))
                .addRequiredItem("name");
        
        final DefaultCodegen codegen = new GoClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPI();
        openAPI.getComponents().addSchemas("PhoneNumber", phoneNumberSchema);
        openAPI.getComponents().addSchemas("Person", personSchema);
        codegen.setOpenAPI(openAPI);
        
        final CodegenModel personModel = codegen.fromModel("Person", personSchema);
        Assert.assertEquals(personModel.name, "Person");
        Assert.assertEquals(personModel.vars.size(), 3);
        
        // Direct $refs should not have aliases (only deduplicated inline schemas get aliases)
        final CodegenProperty mobileProperty = personModel.vars.stream()
                .filter(v -> v.baseName.equals("mobile"))
                .findFirst()
                .orElse(null);
        Assert.assertNotNull(mobileProperty);
        Assert.assertNull(mobileProperty.dataTypeAlias);
        Assert.assertEquals(mobileProperty.dataType, "PhoneNumber");
        
        final CodegenProperty homeProperty = personModel.vars.stream()
                .filter(v -> v.baseName.equals("home"))
                .findFirst()
                .orElse(null);
        Assert.assertNotNull(homeProperty);
        Assert.assertNull(homeProperty.dataTypeAlias);
        Assert.assertEquals(homeProperty.dataType, "PhoneNumber");
    }

    @Test(description = "test type aliases for deduplicated inline schemas")
    public void typeAliasForDeduplicatedInlineSchemasTest() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/inline-deduplicated-schemas.yaml");
        final GoClientCodegen codegen = new GoClientCodegen();
        codegen.setOpenAPI(openAPI);

        Schema demoResponseSchema = openAPI.getComponents().getSchemas().get("DemoResponse");
        CodegenModel demoModel = codegen.fromModel("DemoResponse", demoResponseSchema);
        
        // Call postProcessModels to trigger Go-specific dataType/dataTypeAlias swapping
        ModelsMap modelsMap = new ModelsMap();
        ModelMap modelMap = new ModelMap();
        modelMap.setModel(demoModel);
        modelsMap.setModels(Arrays.asList(modelMap));
        modelsMap.setImports(new ArrayList<>());
        modelsMap = codegen.postProcessModels(modelsMap);
        demoModel = modelsMap.getModels().get(0).getModel();

        Assert.assertEquals(demoModel.name, "DemoResponse");

        // inlinePhone1 is the original (not deduplicated)
        final CodegenProperty inlinePhone1 = demoModel.vars.stream()
                .filter(v -> v.baseName.equals("inlinePhone1"))
                .findFirst()
                .orElse(null);
        Assert.assertNotNull(inlinePhone1);
        Assert.assertNull(inlinePhone1.dataTypeAlias);
        Assert.assertEquals(inlinePhone1.dataType, "DemoResponseInlinePhone1");

        // inlinePhone2 is deduplicated with inlinePhone1
        final CodegenProperty inlinePhone2 = demoModel.vars.stream()
                .filter(v -> v.baseName.equals("inlinePhone2"))
                .findFirst()
                .orElse(null);
        Assert.assertNotNull(inlinePhone2);
        Assert.assertNotNull(inlinePhone2.dataTypeAlias);
        Assert.assertEquals(inlinePhone2.dataType, "DemoResponseInlinePhone2");
        Assert.assertEquals(inlinePhone2.dataTypeAlias, "DemoResponseInlinePhone1");

        // nestedPhones array items are deduplicated
        final CodegenProperty nestedPhones = demoModel.vars.stream()
                .filter(v -> v.baseName.equals("nestedPhones"))
                .findFirst()
                .orElse(null);
        Assert.assertNotNull(nestedPhones);
        Assert.assertTrue(nestedPhones.isContainer);
        Assert.assertNotNull(nestedPhones.items);
        Assert.assertNotNull(nestedPhones.items.dataTypeAlias);
        Assert.assertEquals(nestedPhones.items.dataType, "DemoResponseNestedPhonesInner");
        Assert.assertEquals(nestedPhones.items.dataTypeAlias, "DemoResponseInlinePhone1");
        Assert.assertEquals(nestedPhones.dataType, "[]DemoResponseNestedPhonesInner");

        // phoneHistory array items are deduplicated
        final CodegenProperty phoneHistory = demoModel.vars.stream()
                .filter(v -> v.baseName.equals("phoneHistory"))
                .findFirst()
                .orElse(null);
        Assert.assertNotNull(phoneHistory);
        Assert.assertTrue(phoneHistory.isContainer);
        Assert.assertNotNull(phoneHistory.items);
        Assert.assertNotNull(phoneHistory.items.dataTypeAlias);
        Assert.assertEquals(phoneHistory.items.dataType, "DemoResponsePhoneHistoryInner");
        Assert.assertEquals(phoneHistory.items.dataTypeAlias, "DemoResponseInlinePhone1");
        Assert.assertEquals(phoneHistory.dataType, "[]DemoResponsePhoneHistoryInner");

        // optionalNumber has different structure (no required fields)
        final CodegenProperty optionalNumber = demoModel.vars.stream()
                .filter(v -> v.baseName.equals("optionalNumber"))
                .findFirst()
                .orElse(null);
        Assert.assertNotNull(optionalNumber);
        Assert.assertNull(optionalNumber.dataTypeAlias);
        Assert.assertEquals(optionalNumber.dataType, "DemoResponseOptionalNumber");

        // requiredNumber is deduplicated with inlinePhone1
        final CodegenProperty requiredNumber = demoModel.vars.stream()
                .filter(v -> v.baseName.equals("requiredNumber"))
                .findFirst()
                .orElse(null);
        Assert.assertNotNull(requiredNumber);
        Assert.assertNotNull(requiredNumber.dataTypeAlias);
        Assert.assertEquals(requiredNumber.dataType, "DemoResponseRequiredNumber");
        Assert.assertEquals(requiredNumber.dataTypeAlias, "DemoResponseInlinePhone1");

        // transactOptions array items are deduplicated (MapSchema with additionalProperties: true)
        final CodegenProperty transactOptions = demoModel.vars.stream()
                .filter(v -> v.baseName.equals("transactOptions"))
                .findFirst()
                .orElse(null);
        Assert.assertNotNull(transactOptions);
        Assert.assertTrue(transactOptions.isContainer);
        Assert.assertNotNull(transactOptions.items);
        // transactOptions is the ORIGINAL, so items should NOT have dataTypeAlias
        Assert.assertNull(transactOptions.items.dataTypeAlias);
        Assert.assertEquals(transactOptions.items.dataType, "DemoResponseTransactOptionsInner");
        Assert.assertEquals(transactOptions.dataType, "[]DemoResponseTransactOptionsInner");

        // otherOptions array items are DEDUPLICATED with transactOptions (MapSchema with additionalProperties: true)
        final CodegenProperty otherOptions = demoModel.vars.stream()
                .filter(v -> v.baseName.equals("otherOptions"))
                .findFirst()
                .orElse(null);
        Assert.assertNotNull(otherOptions);
        Assert.assertTrue(otherOptions.isContainer);
        Assert.assertNotNull(otherOptions.items);
        Assert.assertNotNull(otherOptions.items.dataTypeAlias);
        Assert.assertEquals(otherOptions.items.dataType, "DemoResponseOtherOptionsInner");
        Assert.assertEquals(otherOptions.items.dataTypeAlias, "DemoResponseTransactOptionsInner");
        Assert.assertEquals(otherOptions.dataType, "[]DemoResponseOtherOptionsInner");

        // directMapObject is DEDUPLICATED with transactOptions items (MapSchema with additionalProperties: true)
        final CodegenProperty directMapObject = demoModel.vars.stream()
                .filter(v -> v.baseName.equals("directMapObject"))
                .findFirst()
                .orElse(null);
        Assert.assertNotNull(directMapObject);
        Assert.assertNotNull(directMapObject.dataTypeAlias);
        Assert.assertEquals(directMapObject.dataType, "DemoResponseDirectMapObject");
        Assert.assertEquals(directMapObject.dataTypeAlias, "DemoResponseTransactOptionsInner");
    }
}
