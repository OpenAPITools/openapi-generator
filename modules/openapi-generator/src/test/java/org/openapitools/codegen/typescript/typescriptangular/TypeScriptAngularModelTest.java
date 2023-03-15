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

package org.openapitools.codegen.typescript.typescriptangular;

import com.google.common.collect.Sets;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.parser.util.SchemaTypeUtil;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.TypeScriptAngularClientCodegen;
import org.openapitools.codegen.languages.TypeScriptFetchClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.Date;
import java.util.Locale;

@SuppressWarnings("static-method")
public class TypeScriptAngularModelTest {

    @Test(description = "convert a simple TypeScript Angular model")
    public void simpleModelTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("id", new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperties("name", new StringSchema())
                .addProperties("createdAt", new DateTimeSchema())
                .addProperties("birthDate", new DateSchema())
                .addProperties("active", new BooleanSchema())
                .addRequiredItem("id")
                .addRequiredItem("name");
        final DefaultCodegen codegen = new TypeScriptAngularClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 5);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "id");
        Assert.assertEquals(property1.dataType, "number");
        Assert.assertEquals(property1.name, "id");
        Assert.assertEquals(property1.defaultValue, "undefined");
        Assert.assertEquals(property1.baseType, "number");
        Assert.assertTrue(property1.required);
        Assert.assertFalse(property1.isContainer);

        final CodegenProperty property2 = cm.vars.get(1);
        Assert.assertEquals(property2.baseName, "name");
        Assert.assertEquals(property2.dataType, "string");
        Assert.assertEquals(property2.name, "name");
        Assert.assertEquals(property2.defaultValue, "undefined");
        Assert.assertEquals(property2.baseType, "string");
        Assert.assertTrue(property2.required);
        Assert.assertFalse(property2.isContainer);

        final CodegenProperty property3 = cm.vars.get(2);
        Assert.assertEquals(property3.baseName, "createdAt");
        Assert.assertEquals(property3.complexType, null);
        Assert.assertEquals(property3.dataType, "string");
        Assert.assertEquals(property3.name, "createdAt");
        Assert.assertEquals(property3.baseType, "string");
        Assert.assertEquals(property3.defaultValue, "undefined");
        Assert.assertFalse(property3.required);
        Assert.assertFalse(property3.isContainer);

        final CodegenProperty property4 = cm.vars.get(3);
        Assert.assertEquals(property4.baseName, "birthDate");
        Assert.assertEquals(property4.complexType, null);
        Assert.assertEquals(property4.dataType, "string");
        Assert.assertEquals(property4.name, "birthDate");
        Assert.assertEquals(property4.baseType, "string");
        Assert.assertEquals(property4.defaultValue, "undefined");
        Assert.assertFalse(property4.required);
        Assert.assertFalse(property4.isContainer);

        final CodegenProperty property5 = cm.vars.get(4);
        Assert.assertEquals(property5.baseName, "active");
        Assert.assertEquals(property5.complexType, null);
        Assert.assertEquals(property5.dataType, "boolean");
        Assert.assertEquals(property5.name, "active");
        Assert.assertEquals(property5.defaultValue, "undefined");
        Assert.assertFalse(property5.required);
        Assert.assertFalse(property5.isContainer);
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

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 5);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "id");
        Assert.assertEquals(property1.defaultValue, "1234");

        final CodegenProperty property2 = cm.vars.get(1);
        Assert.assertEquals(property2.baseName, "name");
        Assert.assertEquals(property2.defaultValue, "'Jack'");

        final CodegenProperty property3 = cm.vars.get(2);
        Assert.assertEquals(property3.baseName, "createdAt");
        Assert.assertEquals(OffsetDateTime.parse(property3.defaultValue), testOffsetDateTime);

        final CodegenProperty property4 = cm.vars.get(3);
        Assert.assertEquals(property4.baseName, "birthDate");
        Assert.assertEquals(new SimpleDateFormat("EEE MMM dd HH:mm:ss z yyyy", Locale.ENGLISH).parse(property4.defaultValue), testDate);

        final CodegenProperty property5 = cm.vars.get(4);
        Assert.assertEquals(property5.baseName, "active");
        Assert.assertEquals(property5.defaultValue, "true");
    }

    @Test(description = "convert a model with list property")
    public void listPropertyTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("id", new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperties("urls", new ArraySchema().items(new StringSchema()))
                .addRequiredItem("id");
        final DefaultCodegen codegen = new TypeScriptAngularClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 2);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "id");
        Assert.assertEquals(property1.dataType, "number");
        Assert.assertEquals(property1.name, "id");
        Assert.assertEquals(property1.defaultValue, "undefined");
        Assert.assertEquals(property1.baseType, "number");
        Assert.assertTrue(property1.required);
        Assert.assertFalse(property1.isContainer);

        final CodegenProperty property2 = cm.vars.get(1);
        Assert.assertEquals(property2.baseName, "urls");
        Assert.assertEquals(property2.dataType, "Array<string>");
        Assert.assertEquals(property2.name, "urls");
        Assert.assertEquals(property2.baseType, "Array");
        Assert.assertFalse(property2.required);
    }

    @Test(description = "convert a model with complex property")
    public void complexPropertyTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("children", new Schema().$ref("#/definitions/Children"));
        final DefaultCodegen codegen = new TypeScriptAngularClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "children");
        Assert.assertEquals(property1.dataType, "Children");
        Assert.assertEquals(property1.name, "children");
        Assert.assertEquals(property1.defaultValue, "undefined");
        Assert.assertEquals(property1.baseType, "Children");
        Assert.assertFalse(property1.required);
    }

    @Test(description = "convert a model with complex list property")
    public void complexListPropertyTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("children", new ArraySchema()
                        .items(new Schema().$ref("#/definitions/Children")));
        final DefaultCodegen codegen = new TypeScriptAngularClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "children");
        Assert.assertEquals(property1.complexType, "Children");
        Assert.assertEquals(property1.dataType, "Array<Children>");
        Assert.assertEquals(property1.name, "children");
        Assert.assertEquals(property1.baseType, "Array");
        Assert.assertFalse(property1.required);
    }

    @Test(description = "convert an array model")
    public void arrayModelTest() {
        final Schema schema = new ArraySchema()
                .items(new Schema().$ref("#/definitions/Children"))
                .description("an array model");
        final DefaultCodegen codegen = new TypeScriptAngularClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "an array model");
        Assert.assertEquals(cm.vars.size(), 0);
    }

    @Test(description = "convert an array oneof model")
    public void arrayOneOfModelTest() {
        final Schema schema = new ArraySchema()
                .items(new ComposedSchema()
                        .addOneOfItem(new StringSchema())
                        .addOneOfItem(new IntegerSchema().format("int64")))
                .description("an array oneof model");
        final DefaultCodegen codegen = new TypeScriptAngularClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);


        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "an array oneof model");
        Assert.assertEquals(cm.arrayModelType, "string | number");
        Assert.assertEquals(cm.vars.size(), 0);
    }

    @Test(description = "convert an any of with array oneof model")
    public void objectPropertyAnyOfWithArrayOneOfModelTest() {
        final Schema schema = new ObjectSchema().addProperties("value",
                new ComposedSchema().addAnyOfItem(new StringSchema()).addAnyOfItem(new ArraySchema()
                        .items(new ComposedSchema()
                                .addOneOfItem(new StringSchema())
                                .addOneOfItem(new IntegerSchema().format("int64")))))
                .description("an any of with array oneof model");
        final DefaultCodegen codegen = new TypeScriptAngularClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        String s = codegen.getSchemaType((Schema)schema.getProperties().get("value"));


        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "an any of with array oneof model");
        Assert.assertEquals(cm.vars.size(), 1);
        Assert.assertEquals(s, "string | Array<string | number>");
    }

    @Test(description = "import a typemapping")
    public void importTypeMappingModelTest() {
        final Schema schema = new ArraySchema()
                .items(new Schema().$ref("Children"))
                .description("a typemapping array model");
        final DefaultCodegen codegen = new TypeScriptAngularClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        codegen.typeMapping().put("Children", "Test");
        codegen.importMapping().put("Test", "@myTest/package");
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a typemapping array model");
        Assert.assertEquals(cm.vars.size(), 0);
        Assert.assertEquals(cm.imports.size(), 1);
        Assert.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("Test")).size(), 1);
    }

    @Test(description = "convert a map model")
    public void mapModelTest() {
        final Schema schema = new Schema()
                .description("a map model")
                .additionalProperties(new Schema().$ref("#/definitions/Children"));
        final DefaultCodegen codegen = new TypeScriptAngularClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a map model");
        Assert.assertEquals(cm.vars.size(), 0);
        Assert.assertEquals(cm.imports.size(), 1);
        Assert.assertEquals(cm.additionalPropertiesType, "Children");
        Assert.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("Children")).size(), 1);
    }

    @Test(description = "convert a model with a name starting with decimal")
    public void beginDecimalNameTest() {
        final Schema schema = new Schema()
                .description("a model with a name starting with decimal")
                .addProperties("1list", new StringSchema())
                .addRequiredItem("1list");
        final DefaultCodegen codegen = new TypeScriptAngularClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property = cm.vars.get(0);
        Assert.assertEquals(property.baseName, "1list");
        Assert.assertEquals(property.dataType, "string");
        Assert.assertEquals(property.name, "_1list");
        Assert.assertEquals(property.defaultValue, "undefined");
        Assert.assertEquals(property.baseType, "string");
        Assert.assertTrue(property.required);
        Assert.assertFalse(property.isContainer);
    }

    @Test(description = "convert an inline model that originally had a name prefixed with an underscore")
    public void inlineModelWithUnderscoreNameTest() {
        // Originally parent model "FooResponse" with inline model called "_links". The InlineModelResolver resolves
        // that to "FooResponse__links" (double underscore)
        final Schema schema = new Schema()
            .description("an inline model with name previously prefixed with underscore")
            .addRequiredItem("self")
            .addProperties("self", new StringSchema());

        TypeScriptAngularClientCodegen codegen = new TypeScriptAngularClientCodegen();
        codegen.additionalProperties().put(TypeScriptAngularClientCodegen.FILE_NAMING, "kebab-case");
        codegen.processOpts();

        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);

        final CodegenModel cm = codegen.fromModel("FooResponse__links", schema);
        Assert.assertEquals(cm.getClassFilename(), "./foo-response-links", "The generated filename should not have a double hyphen.");
    }
}
