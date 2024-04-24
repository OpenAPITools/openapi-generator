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

package org.openapitools.codegen.csharpnetcore;

import com.google.common.collect.Sets;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.parser.util.SchemaTypeUtil;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.AbstractCSharpCodegen;
import org.openapitools.codegen.languages.AspNetServerCodegen;
import org.openapitools.codegen.languages.CSharpClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

@SuppressWarnings("static-method")
public class CSharpModelTest {

    @Test
    public void assertOuterEnumIsString() {
        // this issue has not been found yet in version 2
        // Assertions.assertEquals(outerEnumVarsIsString(new AspNetServerCodegen(), 2, false), true);
        // Assertions.assertEquals(outerEnumVarsIsString(new AspNetServerCodegen(), 2, true), true);
        Assertions.assertEquals(outerEnumVarsIsString(new AspNetServerCodegen(), 3, false), true);
        Assertions.assertEquals(outerEnumVarsIsString(new AspNetServerCodegen(), 3, true), true);

        // this issue has not been found yet in version 2
        // Assertions.assertEquals(outerEnumVarsIsString(new CSharpClientCodegen(), 2, false), true);
        // Assertions.assertEquals(outerEnumVarsIsString(new CSharpClientCodegen(), 2, true), true);
        Assertions.assertEquals(outerEnumVarsIsString(new CSharpClientCodegen(), 3, false), true);
        Assertions.assertEquals(outerEnumVarsIsString(new CSharpClientCodegen(), 3, true), true);
    }

    public boolean outerEnumVarsIsString(final AbstractCSharpCodegen codegen, final int openApiVersion, final Boolean nullableReferenceTypes){
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/" + openApiVersion + "_0/petstore-with-fake-endpoints-models-for-testing-with-http-signature.yaml");
        codegen.setNullableReferenceTypes(nullableReferenceTypes);
        codegen.setOpenAPI(openAPI);
        codegen.processOpts();
        Schema schema = openAPI.getComponents().getSchemas().get("Enum_Test");
        final CodegenModel generated = codegen.fromModel("OuterEnum", schema);

        CodegenProperty cp0 = generated.getVars().get(0);
        return cp0.isString;
    }

    @Test(description = "convert a model with array property to default List<T>")
    public void arrayPropertyTest() {
        final Schema schema = getArrayTestSchema();

        final DefaultCodegen codegen = new CSharpClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        codegen.processOpts();
        final CodegenModel generated = codegen.fromModel("sample", schema);

        Assertions.assertEquals(generated.name, "sample");
        Assertions.assertEquals(generated.classname, "Sample");
        Assertions.assertEquals(generated.description, "a sample model");
        Assertions.assertEquals(generated.vars.size(), 2);

        final CodegenProperty property = generated.vars.get(1);
        Assertions.assertEquals(property.baseName, "examples");
        Assertions.assertEquals(property.getter, "getExamples");
        Assertions.assertEquals(property.setter, "setExamples");
        Assertions.assertEquals(property.dataType, "List<string>");
        Assertions.assertEquals(property.name, "Examples");
        Assertions.assertNull(property.defaultValue);
        Assertions.assertEquals(property.baseType, "List");
        Assertions.assertEquals(property.containerType, "array");
        Assertions.assertFalse(property.required);
        Assertions.assertTrue(property.isContainer);
    }

    @Test(description = "convert a model with array property to Collection<T>")
    public void arrayPropertyCollectionOptionTest() {
        final Schema schema = getArrayTestSchema();

        final CSharpClientCodegen codegen = new CSharpClientCodegen();

        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        codegen.processOpts();
        codegen.setUseCollection(true);
        final CodegenModel generated = codegen.fromModel("sample", schema);

        Assertions.assertEquals(generated.name, "sample");
        Assertions.assertEquals(generated.vars.size(), 2);

        final CodegenProperty property = generated.vars.get(1);
        Assertions.assertEquals(property.baseName, "examples");
        Assertions.assertEquals(property.name, "Examples");
        Assertions.assertNull(property.defaultValue);
        Assertions.assertEquals(property.dataType, "Collection<string>");
        Assertions.assertEquals(property.baseType, "Collection");
        Assertions.assertEquals(property.containerType, "array");
        Assertions.assertFalse(property.required);
        Assertions.assertTrue(property.isContainer);
    }

    @Test(description = "convert a model with array property to Collection<T>")
    public void arrayPropertyICollectionOptionTest() {
        final Schema schema = getArrayTestSchema();

        final CSharpClientCodegen codegen = new CSharpClientCodegen();

        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        codegen.processOpts();
        codegen.setUseCollection(true);
        codegen.setReturnICollection(true);

        final CodegenModel generated = codegen.fromModel("sample", schema);

        Assertions.assertEquals(generated.name, "sample");
        Assertions.assertEquals(generated.vars.size(), 2);

        final CodegenProperty property = generated.vars.get(1);
        Assertions.assertEquals(property.baseName, "examples");
        Assertions.assertEquals(property.name, "Examples");
        Assertions.assertEquals(property.dataType, "Collection<string>",
                "returnICollection option should not modify property datatype");
        Assertions.assertNull(property.defaultValue);
        Assertions.assertEquals(property.baseType, "Collection",
                "returnICollection option should not modify property baseType");
        Assertions.assertEquals(property.containerType, "array");
        Assertions.assertFalse(property.required);
        Assertions.assertTrue(property.isContainer);
    }

    private Schema getArrayTestSchema() {
        return new Schema()
                .description("a sample model")
                .addProperties("id", new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperties("examples", new ArraySchema().items(new StringSchema()))
                .addRequiredItem("id");
    }

    @Test(description = "convert a simple model")
    public void simpleModelTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("id", new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperties("name", new StringSchema())
                .addProperties("createdAt", new DateTimeSchema())
                .addRequiredItem("id")
                .addRequiredItem("name");
        final DefaultCodegen codegen = new CSharpClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        codegen.processOpts();
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 3);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "id");
        Assertions.assertEquals(property1.dataType, "long");
        Assertions.assertEquals(property1.name, "Id");
        Assertions.assertNull(property1.defaultValue);
        Assertions.assertEquals(property1.baseType, "long");
        Assertions.assertTrue(property1.required);
        Assertions.assertTrue(property1.isPrimitiveType);

        final CodegenProperty property2 = cm.vars.get(1);
        Assertions.assertEquals(property2.baseName, "name");
        Assertions.assertEquals(property2.dataType, "string");
        Assertions.assertEquals(property2.name, "Name");
        Assertions.assertNull(property2.defaultValue);
        Assertions.assertEquals(property2.baseType, "string");
        Assertions.assertTrue(property2.required);
        Assertions.assertTrue(property2.isPrimitiveType);

        final CodegenProperty property3 = cm.vars.get(2);
        Assertions.assertEquals(property3.baseName, "createdAt");
        Assertions.assertEquals(property3.dataType, "DateTime");
        Assertions.assertEquals(property3.name, "CreatedAt");
        Assertions.assertNull(property3.defaultValue);
        Assertions.assertEquals(property3.baseType, "DateTime");
        Assertions.assertFalse(property3.required);
    }

    @Test(description = "convert a model with a non-nullable property")
    public void nonNullablePropertyTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("id",  new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT).nullable(false))
                .addProperties("urls", new ArraySchema()
                        .items(new StringSchema()))
                .addProperties("name", new StringSchema().nullable(true))
                .addRequiredItem("id");
        final DefaultCodegen codegen = new CSharpClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        codegen.processOpts();
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 3);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "id");
        Assertions.assertEquals(property1.dataType, "long");
        Assertions.assertEquals(property1.name, "Id");
        Assertions.assertNull(property1.defaultValue);
        Assertions.assertEquals(property1.baseType, "long");
        Assertions.assertTrue(property1.required);
        Assertions.assertTrue(property1.isPrimitiveType);

        final CodegenProperty property2 = cm.vars.get(1);
        Assertions.assertEquals(property2.baseName, "urls");
        Assertions.assertEquals(property2.dataType, "List<string>");
        Assertions.assertEquals(property2.name, "Urls");
        Assertions.assertNull(property2.defaultValue);
        Assertions.assertEquals(property2.baseType, "List");
        Assertions.assertEquals(property2.containerType, "array");
        Assertions.assertFalse(property2.required);
        Assertions.assertTrue(property2.isPrimitiveType);
        Assertions.assertTrue(property2.isContainer);

        final CodegenProperty property3 = cm.vars.get(2);
        Assertions.assertEquals(property3.baseName, "name");
        Assertions.assertEquals(property3.dataType, "string");
        Assertions.assertEquals(property3.name, "Name");
        Assertions.assertNull(property3.defaultValue);
        Assertions.assertEquals(property3.baseType, "string");
        Assertions.assertFalse(property3.required);
        Assertions.assertTrue(property3.isPrimitiveType);
    }

    @Test(description = "convert a model with a nullable property")
    public void nullablePropertyTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("id",  new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT).nullable(true))
                .addProperties("urls", new ArraySchema()
                        .items(new StringSchema()))
                .addProperties("name", new StringSchema().nullable(true))
                .addRequiredItem("id");
        final DefaultCodegen codegen = new CSharpClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        codegen.processOpts();
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 3);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "id");
        Assertions.assertEquals(property1.dataType, "long?");
        Assertions.assertEquals(property1.name, "Id");
        Assertions.assertNull(property1.defaultValue);
        Assertions.assertEquals(property1.baseType, "long?");
        Assertions.assertTrue(property1.required);
        Assertions.assertTrue(property1.isPrimitiveType);

        final CodegenProperty property2 = cm.vars.get(1);
        Assertions.assertEquals(property2.baseName, "urls");
        Assertions.assertEquals(property2.dataType, "List<string>");
        Assertions.assertEquals(property2.name, "Urls");
        Assertions.assertNull(property2.defaultValue);
        Assertions.assertEquals(property2.baseType, "List");
        Assertions.assertEquals(property2.containerType, "array");
        Assertions.assertFalse(property2.required);
        Assertions.assertTrue(property2.isPrimitiveType);
        Assertions.assertTrue(property2.isContainer);

        final CodegenProperty property3 = cm.vars.get(2);
        Assertions.assertEquals(property3.baseName, "name");
        Assertions.assertEquals(property3.dataType, "string");
        Assertions.assertEquals(property3.name, "Name");
        Assertions.assertNull(property3.defaultValue);
        Assertions.assertEquals(property3.baseType, "string");
        Assertions.assertFalse(property3.required);
        Assertions.assertTrue(property3.isPrimitiveType);
    }

    @Test(description = "convert a model with a nullable property without nullable annotation")
    public void nullablePropertyWithoutNullableReferenceTypesTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("id",  new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT).nullable(true))
                .addProperties("urls", new ArraySchema()
                        .items(new StringSchema()).nullable(true))
                .addProperties("name", new StringSchema().nullable(true))
                .addProperties("subObject",  new Schema().addProperties("name", new StringSchema()).nullable(true))
                .addRequiredItem("id");
        final DefaultCodegen codegen = new AspNetServerCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        codegen.processOpts();
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 4);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "id");
        Assertions.assertEquals(property1.dataType, "long?");
        Assertions.assertEquals(property1.name, "Id");
        Assertions.assertNull(property1.defaultValue);
        Assertions.assertEquals(property1.baseType, "long?");
        Assertions.assertTrue(property1.required);
        Assertions.assertTrue(property1.isPrimitiveType);

        final CodegenProperty property2 = cm.vars.get(1);
        Assertions.assertEquals(property2.baseName, "urls");
        Assertions.assertEquals(property2.dataType, "List<string>");
        Assertions.assertEquals(property2.name, "Urls");
        Assertions.assertNull(property2.defaultValue);
        Assertions.assertEquals(property2.baseType, "List");
        Assertions.assertEquals(property2.containerType, "array");
        Assertions.assertFalse(property2.required);
        Assertions.assertTrue(property2.isPrimitiveType);
        Assertions.assertTrue(property2.isContainer);

        final CodegenProperty property3 = cm.vars.get(2);
        Assertions.assertEquals(property3.baseName, "name");
        Assertions.assertEquals(property3.dataType, "string");
        Assertions.assertEquals(property3.name, "Name");
        Assertions.assertNull(property3.defaultValue);
        Assertions.assertEquals(property3.baseType, "string");
        Assertions.assertFalse(property3.required);
        Assertions.assertTrue(property3.isPrimitiveType);

        final CodegenProperty property4 = cm.vars.get(3);
        Assertions.assertEquals(property4.baseName, "subObject");
        Assertions.assertEquals(property4.dataType, "Object");
        Assertions.assertEquals(property4.name, "SubObject");
        Assertions.assertNull(property4.defaultValue);
        Assertions.assertEquals(property4.baseType, "Object");
        Assertions.assertFalse(property4.required);
        Assertions.assertTrue(property4.isPrimitiveType);
    }

    @Test(description = "convert a model with a nullable property using nullable annotation")
    public void nullablePropertyWithNullableReferenceTypesTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("id",  new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT).nullable(true))
                .addProperties("urls", new ArraySchema()
                        .items(new StringSchema()).nullable(true))
                .addProperties("name", new StringSchema().nullable(true))
                .addProperties("subObject",  new Schema().addProperties("name", new StringSchema()).nullable(true))
                .addRequiredItem("id");
        final DefaultCodegen codegen = new AspNetServerCodegen();
        codegen.processOpts();
        codegen.additionalProperties().put(CodegenConstants.NULLABLE_REFERENCE_TYPES, true);
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        codegen.processOpts();
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 4);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "id");
        Assertions.assertEquals(property1.dataType, "long?");
        Assertions.assertEquals(property1.name, "Id");
        Assertions.assertNull(property1.defaultValue);
        Assertions.assertEquals(property1.baseType, "long?");
        Assertions.assertTrue(property1.required);
        Assertions.assertTrue(property1.isPrimitiveType);

        final CodegenProperty property2 = cm.vars.get(1);
        Assertions.assertEquals(property2.baseName, "urls");
        Assertions.assertEquals(property2.dataType, "List<string>");
        Assertions.assertEquals(property2.name, "Urls");
        Assertions.assertNull(property2.defaultValue);
        Assertions.assertEquals(property2.baseType, "List?");
        Assertions.assertEquals(property2.containerType, "array");
        Assertions.assertFalse(property2.required);
        Assertions.assertTrue(property2.isPrimitiveType);
        Assertions.assertTrue(property2.isContainer);

        final CodegenProperty property3 = cm.vars.get(2);
        Assertions.assertEquals(property3.baseName, "name");
        Assertions.assertEquals(property3.dataType, "string?");
        Assertions.assertEquals(property3.name, "Name");
        Assertions.assertNull(property3.defaultValue);
        Assertions.assertEquals(property3.baseType, "string?");
        Assertions.assertFalse(property3.required);
        Assertions.assertFalse(property3.isPrimitiveType);

        final CodegenProperty property4 = cm.vars.get(3);
        Assertions.assertEquals(property4.baseName, "subObject");
        Assertions.assertEquals(property4.dataType, "Object?");
        Assertions.assertEquals(property4.name, "SubObject");
        Assertions.assertNull(property4.defaultValue);
        Assertions.assertEquals(property4.baseType, "Object?");
        Assertions.assertFalse(property4.required);
        Assertions.assertFalse(property4.isPrimitiveType);
    }

    @Test(description = "convert a model with list property")
    public void listPropertyTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("id",  new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperties("urls", new ArraySchema()
                        .items(new StringSchema()))
                .addRequiredItem("id");
        final DefaultCodegen codegen = new CSharpClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        codegen.processOpts();
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 2);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "id");
        Assertions.assertEquals(property1.dataType, "long");
        Assertions.assertEquals(property1.name, "Id");
        Assertions.assertNull(property1.defaultValue);
        Assertions.assertEquals(property1.baseType, "long");
        Assertions.assertTrue(property1.required);
        Assertions.assertTrue(property1.isPrimitiveType);

        final CodegenProperty property2 = cm.vars.get(1);
        Assertions.assertEquals(property2.baseName, "urls");
        Assertions.assertEquals(property2.dataType, "List<string>");
        Assertions.assertEquals(property2.name, "Urls");
        Assertions.assertNull(property2.defaultValue);
        Assertions.assertEquals(property2.baseType, "List");
        Assertions.assertEquals(property2.containerType, "array");
        Assertions.assertFalse(property2.required);
        Assertions.assertTrue(property2.isPrimitiveType);
        Assertions.assertTrue(property2.isContainer);
    }

    @Test(description = "convert a model with a map property")
    public void mapPropertyTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("translations", new MapSchema()
                        .additionalProperties(new StringSchema()))
                .addRequiredItem("id");
        final DefaultCodegen codegen = new CSharpClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        codegen.processOpts();
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "translations");
        Assertions.assertEquals(property1.dataType, "Dictionary<string, string>");
        Assertions.assertEquals(property1.name, "Translations");
        Assertions.assertEquals(property1.baseType, "Dictionary");
        Assertions.assertEquals(property1.containerType, "map");
        Assertions.assertFalse(property1.required);
        Assertions.assertTrue(property1.isContainer);
        Assertions.assertTrue(property1.isPrimitiveType);
    }

    @Test(description = "convert a model with complex property")
    public void complexPropertyTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("children", new Schema().$ref("#/components/schemas/Children"));
        final DefaultCodegen codegen = new CSharpClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        codegen.processOpts();
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "children");
        Assertions.assertEquals(property1.dataType, "Children");
        Assertions.assertEquals(property1.name, "Children");
        Assertions.assertEquals(property1.baseType, "Children");
        Assertions.assertFalse(property1.required);
    }

    @Test(description = "convert a model with complex list property")
    public void complexListPropertyTest() {
        OpenAPI openAPI = TestUtils.createOpenAPI();
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("children", new ArraySchema()
                        .items(new Schema().$ref("#/components/schemas/Children")));
        final DefaultCodegen codegen = new CSharpClientCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.processOpts();
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "children");
        Assertions.assertEquals(property1.complexType, "Children");
        Assertions.assertEquals(property1.dataType, "List<Children>");
        Assertions.assertEquals(property1.name, "Children");
        Assertions.assertEquals(property1.baseType, "List");
        Assertions.assertEquals(property1.containerType, "array");
        Assertions.assertFalse(property1.required);
        Assertions.assertTrue(property1.isContainer);
    }

    @Test(description = "convert a model with complex map property")
    public void complexMapPropertyTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("children", new MapSchema()
                        .additionalProperties(new Schema().$ref("#/components/schemas/Children")));
        final DefaultCodegen codegen = new CSharpClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        codegen.processOpts();
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 1);
        Assertions.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("Children")).size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "children");
        Assertions.assertEquals(property1.complexType, "Children");
        Assertions.assertEquals(property1.dataType, "Dictionary<string, Children>");
        Assertions.assertEquals(property1.name, "Children");
        Assertions.assertEquals(property1.baseType, "Dictionary");
        Assertions.assertEquals(property1.containerType, "map");
        Assertions.assertFalse(property1.required);
        Assertions.assertTrue(property1.isContainer);
    }

    @Test(description = "convert an array model")
    public void arrayModelTest() {
        final Schema schema = new ArraySchema()
                .items(new Schema().$ref("#/components/schemas/Children"))
                .description("an array model");
        final DefaultCodegen codegen = new CSharpClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        codegen.processOpts();
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "an array model");
        Assertions.assertEquals(cm.vars.size(), 0);
        // TODO: additional properties should be of type Dictionary<string, List<Children>>
        Assertions.assertEquals(cm.imports.size(), 1);
        Assertions.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("Children")).size(), 1);
    }

    @Test(description = "convert a map model")
    public void mapModelTest() {
        final Schema schema = new Schema()
                .description("a map model")
                .additionalProperties(new Schema().$ref("#/components/schemas/Children"));
        final DefaultCodegen codegen = new CSharpClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        codegen.processOpts();
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a map model");
        Assertions.assertEquals(cm.vars.size(), 0);
        // TODO: additional properties should be of type Dictionary<string, Children>
        Assertions.assertEquals(cm.imports.size(), 1);
        Assertions.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("Children")).size(), 1);
    }
}
