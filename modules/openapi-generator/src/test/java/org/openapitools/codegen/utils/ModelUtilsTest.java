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

package org.openapitools.codegen.utils;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import org.openapitools.codegen.TestUtils;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.testng.Assert.*;

public class ModelUtilsTest {

    @Test
    public void testGetAllUsedSchemas() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/unusedSchemas.yaml");
        List<String> allUsedSchemas = ModelUtils.getAllUsedSchemas(openAPI);
        List<String> expectedAllUsedSchemas = Arrays.asList(
                "SomeObj1",
                "SomeObj2",
                "SomeObj3",
                "SomeObjShared",
                "SomeObj6",
                "SomeObj7",
                "SomeObj8",
                "SomeObj9A",
                "SomeObj9B",
                "SomeObj10A",
                "SomeObj10B",
                "SomeObj11",
                "SomeArrayObj12",
                "ArrayItem12",
                "SomeArrayObj13",
                "ArrayItem13",
                "SomeObj14",
                "PropertyObj14",
                "SomeObj15",
                "SomeMapObj16",
                "MapItem16",
                "p17_200_response",
                "SomeObj17",
                "SomeObj18",
                "Common18",
                "_some_p19_patch_request",
                "Obj19ByAge",
                "Obj19ByType",
                "SomeObj20",
                "OtherObj20",
                "PingDataInput21",
                "PingDataOutput21",
                "SInput22",
                "SOutput22",
                "SomeHeader23",
                "SomeHeader24",
                "SomeObj25",
                "SomeObj26",
                "Param27",
                "Param28",
                "Parent30",
                "AChild30",
                "BChild30"
        );
        Assert.assertEquals(allUsedSchemas, expectedAllUsedSchemas);
        Assert.assertTrue(allUsedSchemas.containsAll(expectedAllUsedSchemas));
    }

    @Test
    public void testGetUnusedSchemas() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/unusedSchemas.yaml");
        List<String> unusedSchemas = ModelUtils.getUnusedSchemas(openAPI);
        List<String> expectedUnusedSchemas = Arrays.asList(
                "UnusedObj1",
                "UnusedObj2",
                "UnusedObj3",
                "UnusedObj4",
                "Parent29",
                "AChild29",
                "BChild29"
        );
        Assert.assertEquals(unusedSchemas.size(), expectedUnusedSchemas.size());
        Assert.assertTrue(unusedSchemas.containsAll(expectedUnusedSchemas));
    }

    @Test
    public void testSchemasUsedOnlyInFormParam() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/unusedSchemas.yaml");
        List<String> unusedSchemas = ModelUtils.getSchemasUsedOnlyInFormParam(openAPI);
        Assert.assertEquals(unusedSchemas.size(), 3);
        //SomeObj2 is only used in an 'application/x-www-form-urlencoded' request
        Assert.assertTrue(unusedSchemas.contains("SomeObj2"), "contains 'SomeObj2'");
        //SomeObj3 is only used in a 'multipart/form-data' request
        Assert.assertTrue(unusedSchemas.contains("SomeObj3"), "contains 'SomeObj3'");
        //SomeObj7 is only used in an 'application/x-www-form-urlencoded' request (with referenced request body)
        Assert.assertTrue(unusedSchemas.contains("SomeObj7"), "contains 'SomeObj7'");
    }

    @Test
    public void testNestedFormParameter() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/nestedFormParameter.yaml");
        List<String> unusedSchemas = ModelUtils.getSchemasUsedOnlyInFormParam(openAPI);
        Assert.assertEquals(unusedSchemas.size(), 1);
        Assert.assertTrue(unusedSchemas.contains("OuterObject"), "contains 'OuterObject'");
    }

    @Test
    public void testNoComponentsSection() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/ping.yaml");
        List<String> unusedSchemas = ModelUtils.getSchemasUsedOnlyInFormParam(openAPI);
        Assert.assertEquals(unusedSchemas.size(), 0);
    }

    @Test
    public void testGlobalProducesConsumes() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/globalProducesConsumesTest.yaml");
        List<String> unusedSchemas = ModelUtils.getSchemasUsedOnlyInFormParam(openAPI);
        Assert.assertEquals(unusedSchemas.size(), 0);
    }

    @Test
    public void testIsModelAllowsEmptyBaseModel() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/emptyBaseModel.yaml");
        Schema commandSchema = ModelUtils.getSchema(openAPI, "Command");

        Assert.assertTrue(ModelUtils.isModel(commandSchema));
        Assert.assertFalse(ModelUtils.isFreeFormObject(commandSchema, openAPI));
    }

    @Test
    public void testReferencedSchema() {
        Schema otherObj = new ObjectSchema().addProperties("sprop", new StringSchema()).addProperties("iprop", new IntegerSchema());

        OpenAPI openAPI = TestUtils.createOpenAPI();
        openAPI.getComponents().addSchemas("OtherObj", otherObj);

        Schema notExistingReferencedSchema = new Schema().$ref("NotExisting");
        Schema result1 = ModelUtils.getReferencedSchema(openAPI, notExistingReferencedSchema);
        Assert.assertEquals(result1, notExistingReferencedSchema);

        Schema result2 = ModelUtils.getReferencedSchema(openAPI, new Schema().$ref("#/components/schemas/OtherObj"));
        Assert.assertEquals(result2, otherObj);
    }

    @Test
    public void testReferencedRequestBody() {
        RequestBody otherRequestBody = new RequestBody().description("Some Description");

        OpenAPI openAPI = TestUtils.createOpenAPI();
        openAPI.getComponents().addRequestBodies("OtherRequestBody", otherRequestBody);

        RequestBody notExistingRequestBody = new RequestBody().$ref("NotExisting");
        RequestBody result1 = ModelUtils.getReferencedRequestBody(openAPI, notExistingRequestBody);
        Assert.assertEquals(result1, notExistingRequestBody);

        RequestBody result2 = ModelUtils.getReferencedRequestBody(openAPI, new RequestBody().$ref("#/components/requestBodies/OtherRequestBody"));
        Assert.assertEquals(result2, otherRequestBody);
    }

    @Test
    public void testReferencedApiResponse() {
        ApiResponse otherApiResponse = new ApiResponse().description("Some Description");

        OpenAPI openAPI = TestUtils.createOpenAPI();
        openAPI.getComponents().addResponses("OtherApiResponse", otherApiResponse);

        ApiResponse notExistingApiResponse = new ApiResponse().$ref("NotExisting");
        ApiResponse result1 = ModelUtils.getReferencedApiResponse(openAPI, notExistingApiResponse);
        Assert.assertEquals(result1, notExistingApiResponse);

        ApiResponse result2 = ModelUtils.getReferencedApiResponse(openAPI, new ApiResponse().$ref("#/components/responses/OtherApiResponse"));
        Assert.assertEquals(result2, otherApiResponse);
    }

    @Test
    public void testReferencedParameter() {
        Parameter otherParameter = new Parameter().description("Some Description");

        OpenAPI openAPI = TestUtils.createOpenAPI();
        openAPI.getComponents().addParameters("OtherParameter", otherParameter);

        Parameter notExistingParameter = new Parameter().$ref("NotExisting");
        Parameter result1 = ModelUtils.getReferencedParameter(openAPI, notExistingParameter);
        Assert.assertEquals(result1, notExistingParameter);

        Parameter result2 = ModelUtils.getReferencedParameter(openAPI, new Parameter().$ref("#/components/parameters/OtherParameter"));
        Assert.assertEquals(result2, otherParameter);
    }

    /**
     * Issue https://github.com/OpenAPITools/openapi-generator/issues/582.
     * Composed schemas should not get unaliased when generating model properties, in order to properly
     * generate the property data type name.
     */
    @Test
    public void testComposedSchemasAreNotUnaliased() {
        ComposedSchema composedSchema = new ComposedSchema();
        composedSchema.allOf(Arrays.asList(
                new Schema<>().$ref("#/components/schemas/SomeSchema"),
                new ObjectSchema()
        ));
        Schema refToComposedSchema = new Schema().$ref("#/components/schemas/SomeComposedSchema");

        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("SomeComposedSchema", composedSchema);

        Assert.assertEquals(refToComposedSchema, ModelUtils.unaliasSchema(openAPI, refToComposedSchema, new HashMap<>()));
    }

    @Test
    public void testAliasedTypeIsNotUnaliasedIfUsedForImportMapping() {
        Schema emailSchema = new Schema().$ref("#/components/schemas/Email").type("string");
        StringSchema stringSchema = new StringSchema();
        HashMap<String, String> schemaMappings = new HashMap<>();
        schemaMappings.put("Email", "foo.bar.Email");

        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("Email", stringSchema);

        Assert.assertEquals(emailSchema, ModelUtils.unaliasSchema(openAPI, emailSchema, schemaMappings));
        Assert.assertEquals(stringSchema, ModelUtils.unaliasSchema(openAPI, emailSchema, new HashMap<>()));
    }

    /**
     * Issue https://github.com/OpenAPITools/openapi-generator/issues/1624.
     * ModelUtils.isFreeFormObject() should not throw an NPE when passed an empty
     * object schema that has additionalProperties defined as an empty object schema.
     */
    @Test
    public void testIsFreeFormObject() {
        OpenAPI openAPI = new OpenAPI().openapi("3.0.0");
        // Create initial "empty" object schema.
        ObjectSchema objSchema = new ObjectSchema();
        Assert.assertTrue(ModelUtils.isFreeFormObject(objSchema, openAPI));

        // Set additionalProperties to an empty ObjectSchema.
        objSchema.setAdditionalProperties(new ObjectSchema());
        Assert.assertTrue(ModelUtils.isFreeFormObject(objSchema, openAPI));

        // Add a single property to the schema (no longer a free-form object).
        Map<String, Schema> props = new HashMap<>();
        props.put("prop1", new StringSchema());
        objSchema.setProperties(props);
        Assert.assertFalse(ModelUtils.isFreeFormObject(objSchema, openAPI));

        // Test a non-object schema
        Assert.assertFalse(ModelUtils.isFreeFormObject(new StringSchema(), openAPI));

        // Test a null schema
        Assert.assertFalse(ModelUtils.isFreeFormObject(null, openAPI));
    }

    @Test
    public void testIsSetForValidSet() {
        ArraySchema as = new ArraySchema()
                .items(new StringSchema());
        as.setUniqueItems(true);

        Assert.assertTrue(ModelUtils.isSet(as));
    }

    @Test
    public void testIsSetFalseForInvalidSet() {
        ArraySchema as = new ArraySchema()
                .items(new StringSchema());
        as.setUniqueItems(false);

        Assert.assertFalse(ModelUtils.isSet(as));
    }

    @Test
    public void testIsSetFailsForNullSchema() {
        ArraySchema as = null;
        Assert.assertFalse(ModelUtils.isSet(as));
    }

    @Test
    public void testSimpleRefDecoding() {
        String decoded = ModelUtils.getSimpleRef("#/components/~01%20Hallo~1Welt");
        Assert.assertEquals(decoded, "~1 Hallo/Welt");
    }

    @Test
    public void testRefToSchemaProperties() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/petstore.yaml");

        Schema category = ModelUtils.getSchemaFromRefToSchemaWithProperties(openAPI, "#/components/schemas/Pet/properties/category");
        Assert.assertEquals(category.get$ref(), "#/components/schemas/Category");

        Schema name = ModelUtils.getSchemaFromRefToSchemaWithProperties(openAPI, "#/components/schemas/Pet/properties/name");
        Assert.assertEquals(name.getType(), "string");

        Schema id = ModelUtils.getSchemaFromRefToSchemaWithProperties(openAPI, "#/components/schemas/Pet/properties/id");
        Assert.assertEquals(id.getType(), "integer");
        Assert.assertEquals(id.getFormat(), "int64");

        Assert.assertEquals(null, ModelUtils.getSchemaFromRefToSchemaWithProperties(openAPI, "#/components/schemas/Pet/prop/category"));
        Assert.assertEquals(null, ModelUtils.getSchemaFromRefToSchemaWithProperties(openAPI, "#/components/schemas/Pet/properties/categoryyyy"));
        Assert.assertEquals(null, ModelUtils.getSchemaFromRefToSchemaWithProperties(openAPI, "#/components/schemas/Pet"));
    }


    // 3.0 spec tests
    @Test
    public void test30Schemas() {
        final OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/schema.yaml");
        Schema misc = ModelUtils.getSchema(openAPI, "Misc");

        // test map
        Assert.assertTrue(ModelUtils.isMapSchema((Schema) misc.getProperties().get("map1")));

        // test free form object
        Assert.assertTrue(ModelUtils.isFreeFormObject((Schema) misc.getProperties().get("free_form_object_1"), openAPI));
        Assert.assertTrue(ModelUtils.isFreeFormObject((Schema) misc.getProperties().get("free_form_object_2"), openAPI));
        Assert.assertTrue(ModelUtils.isFreeFormObject((Schema) misc.getProperties().get("free_form_object_3"), openAPI));

        // test oneOf
        Assert.assertTrue(ModelUtils.isOneOf((Schema) misc.getProperties().get("oneof1")));

        // test anyOf model
        Schema anyof1 = ModelUtils.getSchema(openAPI, "anyof1");
        Assert.assertNotNull(anyof1);
        Assert.assertNull(anyof1.getTypes());
        Assert.assertNull(anyof1.getType());
        Assert.assertTrue(ModelUtils.hasAnyOf(anyof1));
        Assert.assertTrue(ModelUtils.isAnyOf(anyof1));

        // test anyOf in properties
        Schema anyof1Property = (Schema) misc.getProperties().get("anyof1");
        Assert.assertNotNull(anyof1Property);
        Assert.assertNull(anyof1Property.getTypes());
        Assert.assertNull(anyof1Property.getType());
        Assert.assertTrue(ModelUtils.hasAnyOf(anyof1Property));
        Assert.assertTrue(ModelUtils.isAnyOf(anyof1Property));

        Schema objectSchema = ModelUtils.getSchema(openAPI, "ObjectSchema");
        Assert.assertFalse(ModelUtils.isMapSchema(objectSchema));
    }

    // 3.1 spec tests
    @Test
    public void test31Schemas() {
        final OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_1/schema.yaml");
        Schema misc = ModelUtils.getSchema(openAPI, "Misc");

        // test map
        Assert.assertTrue(ModelUtils.isMapSchema((Schema) misc.getProperties().get("map1")));

        // test free form object
        Assert.assertTrue(ModelUtils.isFreeFormObject((Schema) misc.getProperties().get("free_form_object_1"), openAPI));
        Assert.assertTrue(ModelUtils.isFreeFormObject((Schema) misc.getProperties().get("free_form_object_2"), openAPI));
        Assert.assertTrue(ModelUtils.isFreeFormObject((Schema) misc.getProperties().get("free_form_object_3"), openAPI));

        // test oneOf property
        Assert.assertTrue(ModelUtils.isOneOf((Schema) misc.getProperties().get("oneof1")));

        // test anyOf property
        Schema anyof1 = (Schema) misc.getProperties().get("anyof1");
        Assert.assertNotNull(anyof1);
        Assert.assertNull(anyof1.getTypes());
        Assert.assertNull(anyof1.getType());
        Assert.assertNotNull(anyof1.getAnyOf());
        Assert.assertFalse(anyof1.getAnyOf().isEmpty());
        Assert.assertTrue(ModelUtils.hasAnyOf(anyof1));
        Assert.assertTrue(ModelUtils.isAnyOf(anyof1));

        Schema anyof2 = (Schema) misc.getProperties().get("anyof2");
        Assert.assertNotNull(anyof2);
        Assert.assertNull(anyof2.getTypes());
        Assert.assertNull(anyof2.getType());
        Assert.assertNotNull(anyof2.getAnyOf());
        Assert.assertFalse(anyof2.getAnyOf().isEmpty());
        Assert.assertTrue(ModelUtils.hasAnyOf(anyof2));
        Assert.assertTrue(ModelUtils.isAnyOf(anyof2));

        Schema objectSchema = ModelUtils.getSchema(openAPI, "ObjectSchema");
        Assert.assertFalse(ModelUtils.isMapSchema(objectSchema));

        Schema complexComposedSchema = ModelUtils.getSchema(openAPI, "ComplexComposedSchema");
        Assert.assertTrue(ModelUtils.isComplexComposedSchema(complexComposedSchema));
    }

    @Test
    public void testCloneNumberSchema() {
        Schema schema = new NumberSchema()
                .name("test-schema")
                .minimum(new BigDecimal(100));

        Schema deepCopy = ModelUtils.cloneSchema(schema, false);

        Assert.assertEquals(deepCopy, schema);
        Assert.assertNotSame(deepCopy, schema);
    }

    @Test
    public void testCloneCustomSchema() {
        Schema schema = new ObjectSchema().type("money");

        Schema deepCopy = ModelUtils.cloneSchema(schema, false);

        Assert.assertEquals(deepCopy, schema);
        Assert.assertNotSame(deepCopy, schema);
    }

    @Test
    public void testCloneComposedSchema() {
        Schema base1 = new ObjectSchema()
                .name("Base1")
                .addProperty("foo", new StringSchema());
        Schema base2 = new ObjectSchema()
                .name("Base2")
                .addProperty("bar", new StringSchema());
        Schema composedSchema = new ComposedSchema()
                .name("Composed")
                .allOf(List.of(base1, base2))
                .addProperty("baz", new StringSchema());

        Schema deepCopy = ModelUtils.cloneSchema(composedSchema, false);

        Assert.assertEquals(deepCopy, composedSchema);
        Assert.assertNotSame(deepCopy, composedSchema);
    }

    @Test
    public void testCloneArrayOfEnumsSchema() {
        Schema schema = new ArraySchema()
                .name("ArrayType")
                .type("array")
                .items(new StringSchema()
                        .type("string")
                        ._enum(List.of("SUCCESS", "FAILURE", "SKIPPED"))
                )
                ._default(List.of("SUCCESS", "FAILURE"));

        Schema deepCopy = ModelUtils.cloneSchema(schema, false);

        Assert.assertEquals(deepCopy, schema);
        Assert.assertNotSame(deepCopy, schema);
    }

    @Test
    public void testCloneDateTimeSchemaWithExample() {
        Schema schema = new DateTimeSchema()
                .example("2020-02-02T20:20:20.000222Z");

        Schema deepCopy = ModelUtils.cloneSchema(schema, false);

        Assert.assertEquals(deepCopy, schema);
        Assert.assertNotSame(deepCopy, schema);
    }

    @Test
    public void testGetSchemaItemsWith31Spec() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_1/issue_18291.yaml");
        Schema arrayWithPrefixItems = ModelUtils.getSchema(openAPI, "ArrayWithPrefixItems");
        Assert.assertNotNull(ModelUtils.getSchemaItems((Schema) arrayWithPrefixItems.getProperties().get("with_prefixitems")));
        Assert.assertNotNull(ModelUtils.getSchemaItems((Schema) arrayWithPrefixItems.getProperties().get("without_items")));
    }

    @Test
    public void simplifyOneOfAnyOfWithOnlyOneNonNullSubSchema() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/simplifyOneOfAnyOf_test.yaml");
        Schema schema;
        List<Schema> subSchemas;

        Schema anyOfWithSeveralSubSchemasButSingleNonNull = ModelUtils.getSchema(openAPI, "AnyOfTest");
        subSchemas = anyOfWithSeveralSubSchemasButSingleNonNull.getAnyOf();
        schema = ModelUtils.simplifyOneOfAnyOfWithOnlyOneNonNullSubSchema(openAPI, anyOfWithSeveralSubSchemasButSingleNonNull, subSchemas);
        assertNull(schema.getOneOf());
        assertNull(schema.getAnyOf());
        assertTrue(schema.getNullable());
        assertEquals("string", schema.getType());

        Schema anyOfWithSingleNonNullSubSchema = ModelUtils.getSchema(openAPI, "Parent");
        subSchemas = ((Schema) anyOfWithSingleNonNullSubSchema.getProperties().get("number")).getAnyOf();
        schema = ModelUtils.simplifyOneOfAnyOfWithOnlyOneNonNullSubSchema(openAPI, anyOfWithSingleNonNullSubSchema, subSchemas);
        assertNull(schema.getOneOf());
        assertNull(schema.getAnyOf());
        assertNull(schema.getNullable());
        assertEquals(schema.get$ref(), "#/components/schemas/Number");

        Schema oneOfWithSeveralSubSchemasButSingleNonNull = ModelUtils.getSchema(openAPI, "OneOfTest");
        subSchemas = oneOfWithSeveralSubSchemasButSingleNonNull.getOneOf();
        schema = ModelUtils.simplifyOneOfAnyOfWithOnlyOneNonNullSubSchema(openAPI, oneOfWithSeveralSubSchemasButSingleNonNull, subSchemas);
        assertNull(schema.getOneOf());
        assertNull(schema.getAnyOf());
        assertTrue(schema.getNullable());
        assertEquals("integer", schema.getType());

        Schema oneOfWithSingleNonNullSubSchema = ModelUtils.getSchema(openAPI, "ParentWithOneOfProperty");
        subSchemas = ((Schema) oneOfWithSingleNonNullSubSchema.getProperties().get("number")).getOneOf();
        schema = ModelUtils.simplifyOneOfAnyOfWithOnlyOneNonNullSubSchema(openAPI, oneOfWithSingleNonNullSubSchema, subSchemas);
        assertNull(schema.getOneOf());
        assertNull(schema.getAnyOf());
        assertNull(schema.getNullable());
        assertEquals(schema.get$ref(), "#/components/schemas/Number");

        Schema oneOfWithSeveralSubSchemas = ModelUtils.getSchema(openAPI, "ParentWithPluralOneOfProperty");
        subSchemas = ((Schema) oneOfWithSeveralSubSchemas.getProperties().get("number")).getOneOf();
        schema = ModelUtils.simplifyOneOfAnyOfWithOnlyOneNonNullSubSchema(openAPI, oneOfWithSeveralSubSchemas, subSchemas);
        assertNull(schema.getOneOf());
        assertNotNull(oneOfWithSeveralSubSchemas.getProperties().get("number"));
        assertNull(schema.getAnyOf());
        assertNull(schema.getNullable());
        assertEquals(((Schema) oneOfWithSeveralSubSchemas.getProperties().get("number")).getOneOf().size(), 2);
    }

    @Test
    public void simplifyOneOfWithOnlyOneNonNullSubSchemaKeepsReadOnlyWriteOnlyAttribute() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/simplifyOneOfAnyOf_test.yaml");
        Schema schema;
        List<Schema> subSchemas;

        Schema oneOfWithNullAndRefSubSchema = ModelUtils.getSchema(openAPI, "OneOfParentRefTest");
        Schema numberPropertySchema = ((Schema) oneOfWithNullAndRefSubSchema.getProperties().get("number"));
        subSchemas = numberPropertySchema.getOneOf();
        schema = ModelUtils.simplifyOneOfAnyOfWithOnlyOneNonNullSubSchema(openAPI, numberPropertySchema, subSchemas);
        assertNull(schema.getOneOf());
        assertNull(schema.getAnyOf());
        assertTrue(schema.getNullable());
        assertNull(schema.getReadOnly());
        assertTrue(schema.getWriteOnly());
        assertEquals(schema.get$ref(), "#/components/schemas/IntegerRef");

        Schema number2PropertySchema = ((Schema) oneOfWithNullAndRefSubSchema.getProperties().get("number2"));
        subSchemas = number2PropertySchema.getOneOf();
        schema = ModelUtils.simplifyOneOfAnyOfWithOnlyOneNonNullSubSchema(openAPI, number2PropertySchema, subSchemas);
        assertNull(schema.getOneOf());
        assertNull(schema.getAnyOf());
        assertTrue(schema.getNullable());
        assertTrue(schema.getReadOnly());
        assertNull(schema.getWriteOnly());
        assertEquals(schema.get$ref(), "#/components/schemas/IntegerRef");
    }

    @Test
    public void simplifyAnyOfWithOnlyOneNonNullSubSchemaKeepsReadOnlyWriteOnlyAttribute() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/simplifyOneOfAnyOf_test.yaml");
        Schema schema;
        List<Schema> subSchemas;

        Schema anyOfWithNullAndRefSubSchema = ModelUtils.getSchema(openAPI, "AnyOfParentRefTest");
        Schema numberPropertySchema = ((Schema) anyOfWithNullAndRefSubSchema.getProperties().get("number"));
        subSchemas = numberPropertySchema.getAnyOf();
        schema = ModelUtils.simplifyOneOfAnyOfWithOnlyOneNonNullSubSchema(openAPI, numberPropertySchema, subSchemas);
        assertNull(schema.getOneOf());
        assertNull(schema.getAnyOf());
        assertTrue(schema.getNullable());
        assertNull(schema.getReadOnly());
        assertTrue(schema.getWriteOnly());
        assertEquals(schema.get$ref(), "#/components/schemas/IntegerRef");

        Schema number2PropertySchema = ((Schema) anyOfWithNullAndRefSubSchema.getProperties().get("number2"));
        subSchemas = number2PropertySchema.getAnyOf();
        schema = ModelUtils.simplifyOneOfAnyOfWithOnlyOneNonNullSubSchema(openAPI, number2PropertySchema, subSchemas);
        assertNull(schema.getOneOf());
        assertNull(schema.getAnyOf());
        assertTrue(schema.getNullable());
        assertTrue(schema.getReadOnly());
        assertNull(schema.getWriteOnly());
        assertEquals(schema.get$ref(), "#/components/schemas/IntegerRef");
    }

    @Test
    public void isNullTypeSchemaTest() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/null_schema_test.yaml");
        Map<String, String> options = new HashMap<>();
        Schema schema = openAPI.getComponents().getSchemas().get("AnyOfStringArrayOfString");
        assertFalse(ModelUtils.isNullTypeSchema(openAPI, schema));

        schema = openAPI.getComponents().getSchemas().get("IntegerRef");
        assertFalse(ModelUtils.isNullTypeSchema(openAPI, schema));

        schema = openAPI.getComponents().getSchemas().get("OneOfAnyType");
        assertFalse(ModelUtils.isNullTypeSchema(openAPI, schema));

        schema = openAPI.getComponents().getSchemas().get("AnyOfAnyType");
        assertFalse(ModelUtils.isNullTypeSchema(openAPI, schema));

        schema = openAPI.getComponents().getSchemas().get("Parent");
        assertFalse(ModelUtils.isNullTypeSchema(openAPI, schema));
        // the dummy property is a ref to integer
        assertFalse(ModelUtils.isNullTypeSchema(openAPI, (Schema) schema.getProperties().get("dummy")));

        schema = openAPI.getComponents().getSchemas().get("AnyOfTest");
        assertFalse(ModelUtils.isNullTypeSchema(openAPI, schema));
        // first element (getAnyOf().get(0)) is a string. no need to test
        assertTrue(ModelUtils.isNullTypeSchema(openAPI, (Schema) schema.getAnyOf().get(1)));
        assertTrue(ModelUtils.isNullTypeSchema(openAPI, (Schema) schema.getAnyOf().get(2)));
        assertTrue(ModelUtils.isNullTypeSchema(openAPI, (Schema) schema.getAnyOf().get(3)));

        schema = openAPI.getComponents().getSchemas().get("OneOfRef");
        assertFalse(ModelUtils.isNullTypeSchema(openAPI, schema));
        assertFalse(ModelUtils.isNullTypeSchema(openAPI, (Schema) schema.getOneOf().get(0)));

        schema = openAPI.getComponents().getSchemas().get("OneOfMultiRef");
        assertFalse(ModelUtils.isNullTypeSchema(openAPI, schema));
        assertFalse(ModelUtils.isNullTypeSchema(openAPI, (Schema) schema.getOneOf().get(0)));
        assertFalse(ModelUtils.isNullTypeSchema(openAPI, (Schema) schema.getOneOf().get(1)));

        schema = openAPI.getComponents().getSchemas().get("JustDescription");
        assertFalse(ModelUtils.isNullTypeSchema(openAPI, schema));
    }

    @Test
    public void isNullTypeSchemaTestWith31Spec() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_1/null_schema_test.yaml");
        Map<String, String> options = new HashMap<>();
        Schema schema = openAPI.getComponents().getSchemas().get("AnyOfStringArrayOfString");
        assertFalse(ModelUtils.isNullTypeSchema(openAPI, schema));

        schema = openAPI.getComponents().getSchemas().get("IntegerRef");
        assertFalse(ModelUtils.isNullTypeSchema(openAPI, schema));

        schema = openAPI.getComponents().getSchemas().get("OneOfAnyType");
        assertFalse(ModelUtils.isNullTypeSchema(openAPI, schema));

        schema = openAPI.getComponents().getSchemas().get("AnyOfAnyType");
        assertFalse(ModelUtils.isNullTypeSchema(openAPI, schema));

        schema = openAPI.getComponents().getSchemas().get("Parent");
        assertFalse(ModelUtils.isNullTypeSchema(openAPI, schema));
        // the dummy property is a ref to integer
        assertFalse(ModelUtils.isNullTypeSchema(openAPI, (Schema) schema.getProperties().get("dummy")));

        schema = openAPI.getComponents().getSchemas().get("AnyOfTest");
        assertFalse(ModelUtils.isNullTypeSchema(openAPI, schema));
        // first element (getAnyOf().get(0)) is a string. no need to test
        assertTrue(ModelUtils.isNullTypeSchema(openAPI, (Schema) schema.getAnyOf().get(1)));
        assertTrue(ModelUtils.isNullTypeSchema(openAPI, (Schema) schema.getAnyOf().get(2)));
        assertTrue(ModelUtils.isNullTypeSchema(openAPI, (Schema) schema.getAnyOf().get(3)));

        schema = openAPI.getComponents().getSchemas().get("OneOfRef");
        assertFalse(ModelUtils.isNullTypeSchema(openAPI, schema));
        assertFalse(ModelUtils.isNullTypeSchema(openAPI, (Schema) schema.getOneOf().get(0)));

        schema = openAPI.getComponents().getSchemas().get("OneOfMultiRef");
        assertFalse(ModelUtils.isNullTypeSchema(openAPI, schema));
        assertFalse(ModelUtils.isNullTypeSchema(openAPI, (Schema) schema.getOneOf().get(0)));
        assertFalse(ModelUtils.isNullTypeSchema(openAPI, (Schema) schema.getOneOf().get(1)));

        schema = openAPI.getComponents().getSchemas().get("JustDescription");
        assertFalse(ModelUtils.isNullTypeSchema(openAPI, schema));
    }

    @Test
    public void isUnsupportedSchemaTest() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_1/unsupported_schema_test.yaml");
        Map<String, String> options = new HashMap<>();
        Schema schema = openAPI.getComponents().getSchemas().get("Dummy");
        Schema property1 = (Schema) schema.getProperties().get("property1");
        Schema property2 = (Schema) schema.getProperties().get("property2");

        // a test of string type with allOf (2 patterns)
        assertTrue(ModelUtils.isUnsupportedSchema(openAPI, (Schema) property1.getAllOf().get(0)));
        assertTrue(ModelUtils.isUnsupportedSchema(openAPI, (Schema) property1.getAllOf().get(1)));

        // if, then test
        assertTrue(ModelUtils.isUnsupportedSchema(openAPI, (Schema) property2.getAllOf().get(0)));
        assertTrue(ModelUtils.isUnsupportedSchema(openAPI, (Schema) property2.getAllOf().get(1)));

        // typical schemas, e.g. boolean, string, array of string (enum)
        assertFalse(ModelUtils.isUnsupportedSchema(openAPI, (Schema) property2.getProperties().get("aBooleanCheck")));
        assertFalse(ModelUtils.isUnsupportedSchema(openAPI, (Schema) property2.getProperties().get("condition")));
        assertFalse(ModelUtils.isUnsupportedSchema(openAPI, (Schema) property2.getProperties().get("purpose")));
    }

    @Test
    public void testModelWithPropertiesOnly() {
        // Schema with no properties
        Schema testSchema = new ObjectSchema().properties(null);
        assertFalse(ModelUtils.isModelWithPropertiesOnly(testSchema));

        // Schema with properties
        testSchema.setProperties(Map.of("foo", new Schema()));
        assertTrue(ModelUtils.isModelWithPropertiesOnly(testSchema));

        // Explicitly no additional properties
        testSchema.setAdditionalProperties(false);
        assertTrue(ModelUtils.isModelWithPropertiesOnly(testSchema));

        // With additional properties
        testSchema.setAdditionalProperties(true);
        assertFalse(ModelUtils.isModelWithPropertiesOnly(testSchema));

        // Additional properties is a schema set to false
        testSchema.setAdditionalProperties(new Schema().booleanSchemaValue(false));
        assertTrue(ModelUtils.isModelWithPropertiesOnly(testSchema));

        // Additional properties is a schema set to true
        testSchema.setAdditionalProperties(new Schema().booleanSchemaValue(true));
        assertFalse(ModelUtils.isModelWithPropertiesOnly(testSchema));

        // Additional properties is a custom schema
        testSchema.setAdditionalProperties(new Schema().type("string"));
        assertFalse(ModelUtils.isModelWithPropertiesOnly(testSchema));
    }

    @Test
    public void getParentNameMultipleInterfacesTest() {
        OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/oneOf_innerModel.yaml");
        Map<String, Schema> allSchemas = openAPI.getComponents().getSchemas();
        Schema composedSchema = allSchemas.get("RandomAnimalsResponse_animals_inner");
        assertNull(ModelUtils.getParentName(composedSchema, allSchemas));
    }
}
