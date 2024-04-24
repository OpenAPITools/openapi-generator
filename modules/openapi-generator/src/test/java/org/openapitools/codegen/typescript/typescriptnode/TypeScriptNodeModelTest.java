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

package org.openapitools.codegen.typescript.typescriptnode;

import com.google.common.collect.Sets;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.parser.util.SchemaTypeUtil;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.TypeScriptFetchClientCodegen;
import org.openapitools.codegen.languages.TypeScriptNodeClientCodegen;
import org.openapitools.codegen.typescript.TypeScriptGroups;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.Date;
import java.util.Locale;

@Test(groups = {TypeScriptGroups.TYPESCRIPT, TypeScriptGroups.TYPESCRIPT_NODE})
@SuppressWarnings("static-method")
public class TypeScriptNodeModelTest {

    @Test(description = "convert a simple TypeScript Node model")
    public void simpleModelTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("id", new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperties("name", new StringSchema())
                .addProperties("createdAt", new DateTimeSchema())
                .addProperties("birthDate", new DateSchema())
                .addProperties("active", new BooleanSchema())
                .addRequiredItem("id")
                .addRequiredItem("name");
        final DefaultCodegen codegen = new TypeScriptNodeClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 5);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "id");
        Assertions.assertEquals(property1.dataType, "number");
        Assertions.assertEquals(property1.name, "id");
        Assertions.assertEquals(property1.defaultValue, null);
        Assertions.assertEquals(property1.baseType, "number");
        Assertions.assertTrue(property1.required);

        final CodegenProperty property2 = cm.vars.get(1);
        Assertions.assertEquals(property2.baseName, "name");
        Assertions.assertEquals(property2.dataType, "string");
        Assertions.assertEquals(property2.name, "name");
        Assertions.assertEquals(property2.defaultValue, null);
        Assertions.assertEquals(property2.baseType, "string");
        Assertions.assertTrue(property2.required);

        final CodegenProperty property3 = cm.vars.get(2);
        Assertions.assertEquals(property3.baseName, "createdAt");
        Assertions.assertEquals(property3.complexType, null);
        Assertions.assertEquals(property3.dataType, "Date");
        Assertions.assertEquals(property3.name, "createdAt");
        Assertions.assertEquals(property3.defaultValue, null);
        Assertions.assertFalse(property3.required);

        final CodegenProperty property4 = cm.vars.get(3);
        Assertions.assertEquals(property4.baseName, "birthDate");
        Assertions.assertEquals(property4.complexType, null);
        Assertions.assertEquals(property4.dataType, "string");
        Assertions.assertEquals(property4.name, "birthDate");
        Assertions.assertEquals(property4.defaultValue, null);
        Assertions.assertFalse(property4.required);

        final CodegenProperty property5 = cm.vars.get(4);
        Assertions.assertEquals(property5.baseName, "active");
        Assertions.assertEquals(property5.complexType, null);
        Assertions.assertEquals(property5.dataType, "boolean");
        Assertions.assertEquals(property5.name, "active");
        Assertions.assertEquals(property5.defaultValue, null);
        Assertions.assertFalse(property5.required);
        Assertions.assertFalse(property5.isContainer);
    }

    @Test(description = "convert and check default values for a simple TypeScript Angular model")
    public void simpleModelDefaultValuesTest() throws ParseException {
        IntegerSchema integerSchema = new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT);
        integerSchema.setDefault(1234);

        StringSchema stringSchema = new StringSchema();
        stringSchema.setDefault("Jack");

        OffsetDateTime testOffsetDateTime = OffsetDateTime.of(LocalDateTime.of(2020, 1, 1, 12, 0), ZoneOffset.UTC);
        DateTimeSchema dateTimeSchema = new DateTimeSchema();
        dateTimeSchema.setDefault(testOffsetDateTime);

        Date testDate = Date.from(testOffsetDateTime.toInstant());
        DateSchema dateSchema = new DateSchema();
        dateSchema.setDefault(testDate);

        BooleanSchema booleanSchema = new BooleanSchema();
        booleanSchema.setDefault(true);

        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("id", integerSchema)
                .addProperties("name", stringSchema)
                .addProperties("createdAt", dateTimeSchema)
                .addProperties("birthDate", dateSchema)
                .addProperties("active", booleanSchema)
                .addRequiredItem("id")
                .addRequiredItem("name");

        final DefaultCodegen codegen = new TypeScriptFetchClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 5);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "id");
        Assertions.assertEquals(property1.defaultValue, "1234");

        final CodegenProperty property2 = cm.vars.get(1);
        Assertions.assertEquals(property2.baseName, "name");
        Assertions.assertEquals(property2.defaultValue, "'Jack'");

        final CodegenProperty property3 = cm.vars.get(2);
        Assertions.assertEquals(property3.baseName, "createdAt");
        Assertions.assertEquals(OffsetDateTime.parse(property3.defaultValue), testOffsetDateTime);

        final CodegenProperty property4 = cm.vars.get(3);
        Assertions.assertEquals(property4.baseName, "birthDate");
        Assertions.assertEquals(new SimpleDateFormat("EEE MMM dd HH:mm:ss z yyyy", Locale.ENGLISH).parse(property4.defaultValue), testDate);

        final CodegenProperty property5 = cm.vars.get(4);
        Assertions.assertEquals(property5.baseName, "active");
        Assertions.assertEquals(property5.defaultValue, "true");
    }

    @Test(description = "convert a model with list property")
    public void listPropertyTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("id", new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperties("urls", new ArraySchema().items(new StringSchema()))
                .addRequiredItem("id");
        final DefaultCodegen codegen = new TypeScriptNodeClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 2);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "id");
        Assertions.assertEquals(property1.dataType, "number");
        Assertions.assertEquals(property1.name, "id");
        Assertions.assertEquals(property1.defaultValue, null);
        Assertions.assertEquals(property1.baseType, "number");
        Assertions.assertTrue(property1.required);

        final CodegenProperty property2 = cm.vars.get(1);
        Assertions.assertEquals(property2.baseName, "urls");
        Assertions.assertEquals(property2.dataType, "Array<string>");
        Assertions.assertEquals(property2.name, "urls");
        Assertions.assertEquals(property2.baseType, "Array");
        Assertions.assertFalse(property2.required);
    }

    @Test(description = "convert a model with complex property")
    public void complexPropertyTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("children", new Schema().$ref("#/definitions/Children"));
        final DefaultCodegen codegen = new TypeScriptNodeClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "children");
        Assertions.assertEquals(property1.dataType, "Children");
        Assertions.assertEquals(property1.name, "children");
        Assertions.assertEquals(property1.baseType, "Children");
        Assertions.assertFalse(property1.required);
    }

    @Test(description = "convert a model with complex list property")
    public void complexListPropertyTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("children", new ArraySchema()
                        .items(new Schema().$ref("#/definitions/Children")));
        final DefaultCodegen codegen = new TypeScriptNodeClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "children");
        Assertions.assertEquals(property1.complexType, "Children");
        Assertions.assertEquals(property1.dataType, "Array<Children>");
        Assertions.assertEquals(property1.name, "children");
        Assertions.assertEquals(property1.baseType, "Array");
        Assertions.assertFalse(property1.required);
    }

    @Test(description = "convert an array model")
    public void arrayModelTest() {
        final Schema schema = new ArraySchema()
                .items(new Schema().$ref("#/definitions/Children"))
                .description("an array model");
        final DefaultCodegen codegen = new TypeScriptNodeClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "an array model");
        Assertions.assertEquals(cm.vars.size(), 0);
    }

    @Test(description = "convert a map model")
    public void mapModelTest() {
        final Schema schema = new Schema()
                .description("a map model")
                .additionalProperties(new Schema().$ref("#/definitions/Children"));
        final DefaultCodegen codegen = new TypeScriptNodeClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a map model");
        Assertions.assertEquals(cm.vars.size(), 0);
        Assertions.assertEquals(cm.imports.size(), 1);
        Assertions.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("Children")).size(), 1);
    }

    @Test(description = "convert an array additional properties model")
    public void arrayModelAdditionalPropertiesArrayTest() {
        final Schema schema = new Schema()
                .description("a map model")
                .additionalProperties(new ArraySchema().type("array").items(new Schema().type("string")));
        final DefaultCodegen codegen = new TypeScriptNodeClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a map model");
        Assertions.assertEquals(cm.additionalPropertiesType, "Array<string>");
    }

    @Test(description = "convert a string additional properties model")
    public void arrayModelAdditionalPropertiesStringTest() {
        final Schema schema = new Schema()
                .description("a map model")
                .additionalProperties(new Schema().type("string"));
        final DefaultCodegen codegen = new TypeScriptNodeClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a map model");
        Assertions.assertEquals(cm.additionalPropertiesType, "string");
    }

    @Test(description = "convert a complex additional properties model")
    public void arrayModelAdditionalPropertiesComplexTest() {
        final Schema schema = new Schema()
                .description("a map model")
                .additionalProperties(new Schema().type("object").$ref("#/definitions/Children"));
        final DefaultCodegen codegen = new TypeScriptNodeClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a map model");
        Assertions.assertEquals(cm.additionalPropertiesType, "Children");
    }

    @Test(description = "prepend imports with ./ by default")
    public void defaultFromModelTest() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/petstore.yaml");
        final DefaultCodegen codegen = new TypeScriptNodeClientCodegen();
        codegen.setOpenAPI(openAPI);
        final Schema categorySchema = openAPI.getComponents().getSchemas().get("ApiResponse");
        final CodegenModel cm = codegen.fromModel("ApiResponse", categorySchema);

        Assertions.assertEquals(cm.name, "ApiResponse");
        Assertions.assertEquals(cm.classFilename, "./apiResponse");
    }

    @Test(description = "use mapped imports for type")
    public void mappedFromModelTest() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/petstore.yaml");
        final DefaultCodegen codegen = new TypeScriptNodeClientCodegen();
        final String mappedName = "@namespace/dir/response";
        codegen.importMapping().put("ApiResponse", mappedName);
        codegen.setOpenAPI(openAPI);
        final Schema categorySchema = openAPI.getComponents().getSchemas().get("ApiResponse");
        final CodegenModel cm = codegen.fromModel("ApiResponse", categorySchema);

        Assertions.assertEquals(cm.name, "ApiResponse");
        Assertions.assertEquals(cm.classFilename, mappedName);
    }
}
