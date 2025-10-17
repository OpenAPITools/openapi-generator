/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

package org.openapitools.codegen;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.security.SecurityScheme;
import org.openapitools.codegen.utils.ModelUtils;
import org.testng.annotations.Test;

import java.util.*;

import static org.testng.Assert.*;

public class OpenAPINormalizerTest {

    private static final String REF_AS_PARENT_IN_ALLOF = "REF_AS_PARENT_IN_ALLOF";
    private static final String X_PARENT = "x-parent";
    private static final String X_INTERNAL = "x-internal";

    @Test
    public void testOpenAPINormalizerRefAsParentInAllOf() {
        // to test the rule REF_AS_PARENT_IN_ALLOF
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/allOf_extension_parent.yaml");

        Schema<?> anotherPerson = openAPI.getComponents().getSchemas().get("AnotherPerson");
        assertNull(anotherPerson.getExtensions());

        Schema<?>person = openAPI.getComponents().getSchemas().get("Person");
        assertEquals(person.getExtensions().get(X_PARENT), "abstract");

        Schema<?> preNormPersonA = openAPI.getComponents().getSchemas().get("PersonA");
        assertNull(preNormPersonA.getExtensions());
        Schema<?> preNormPersonB = openAPI.getComponents().getSchemas().get("PersonB");
        assertNull(preNormPersonB.getExtensions());

        Map<String, String> options = new HashMap<>();
        options.put(REF_AS_PARENT_IN_ALLOF, "true");
        OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, options);
        openAPINormalizer.normalize();

        Schema<?>schema3 = openAPI.getComponents().getSchemas().get("AnotherPerson");
        assertEquals(schema3.getExtensions().get(X_PARENT), true);

        Schema<?>schema4 = openAPI.getComponents().getSchemas().get("AnotherParent");
        assertEquals(schema4.getExtensions().get(X_PARENT), true);

        Schema<?>schema5 = openAPI.getComponents().getSchemas().get("Person");
        assertEquals(schema5.getExtensions().get(X_PARENT), "abstract");

        // Verify that all allOf refs gets marked as parents
        Schema<?>schemaWithTwoParents = openAPI.getComponents().getSchemas().get("SchemaWithTwoAllOfRefs");
        assertNull(schemaWithTwoParents.getExtensions());
        Schema<?>personA = openAPI.getComponents().getSchemas().get("PersonA");
        assertEquals(personA.getExtensions().get(X_PARENT), true);
        Schema<?>personB = openAPI.getComponents().getSchemas().get("PersonB");
        assertEquals(personB.getExtensions().get(X_PARENT), true);
    }

    @Test
    public void testOpenAPINormalizerRefAsParentInAllOfAndRefactorAllOfWithProperties() {
        // to test the both REF_AS_PARENT_IN_ALLOF and REFACTOR_ALLOF_WITH_PROPERTIES_ONLY
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/allOf_extension_parent.yaml");

        Schema<?> schema = openAPI.getComponents().getSchemas().get("Child");
        assertNull(schema.getExtensions());

        Schema<?> schema2 = openAPI.getComponents().getSchemas().get("Ancestor");
        assertNull(schema2.getExtensions());

        Map<String, String> options = new HashMap<>();
        options.put(REF_AS_PARENT_IN_ALLOF, "true");
        options.put("REFACTOR_ALLOF_WITH_PROPERTIES_ONLY", "true");
        OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, options);
        openAPINormalizer.normalize();

        Schema<?> schema3 = openAPI.getComponents().getSchemas().get("Ancestor");
        assertEquals(schema3.getExtensions().get(X_PARENT), true);

        Schema<?> schema4 = openAPI.getComponents().getSchemas().get("Child");
        assertNull(schema4.getExtensions());
    }

    @Test
    public void testOpenAPINormalizerRefactorAllofWithMetadataOnlySchemas() {
        // to test the rule REF_AS_PARENT_IN_ALLOF
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/allof_with_metadata_only_schemas.yaml");

        Schema schema = openAPI.getComponents().getSchemas().get("ReferenceNumber");
        assertEquals(schema.getAllOf().size(), 3);
        assertEquals(((Schema) schema.getAllOf().get(2)).getExample(), "IEAN1234");

        Map<String, String> options = new HashMap<>();
        OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, options);
        openAPINormalizer.normalize();

        Schema schema2 = openAPI.getComponents().getSchemas().get("ReferenceNumber");
        assertEquals(schema2.getAllOf().size(), 1);
        assertEquals(schema2.getExample(), "IEAN1234");
        assertEquals(((Schema) schema2.getAllOf().get(0)).get$ref(), "#/components/schemas/IEAN8");
    }

    @Test
    public void testOpenAPINormalizerEnableKeepOnlyFirstTagInOperation() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/enableKeepOnlyFirstTagInOperation_test.yaml");

        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getGet().getTags().size(), 2);
        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getDelete().getTags().size(), 1);

        Map<String, String> options = new HashMap<>();
        options.put("KEEP_ONLY_FIRST_TAG_IN_OPERATION", "true");
        OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, options);
        openAPINormalizer.normalize();

        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getGet().getTags().size(), 1);
        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getDelete().getTags().size(), 1);
        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getGet().getTags().get(0), "person");
    }

    @Test
    public void testOpenAPINormalizerRemoveAnyOfOneOfAndKeepPropertiesOnly() {
        // to test the rule REMOVE_ANYOF_ONEOF_AND_KEEP_PROPERTIES_ONLY
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/removeAnyOfOneOfAndKeepPropertiesOnly_test.yaml");

        Schema schema = openAPI.getComponents().getSchemas().get("Person");
        assertEquals(schema.getAnyOf().size(), 2);

        Map<String, String> options = new HashMap<>();
        options.put("REMOVE_ANYOF_ONEOF_AND_KEEP_PROPERTIES_ONLY", "true");
        OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, options);
        openAPINormalizer.normalize();

        Schema schema3 = openAPI.getComponents().getSchemas().get("Person");
        assertNull(schema.getAnyOf());
    }


    @Test
    public void testOpenAPINormalizerSimplifyOneOfAnyOfStringAndEnumString() {
        // to test the rule SIMPLIFY_ONEOF_ANYOF_STRING_AND_ENUM_STRING
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/simplifyAnyOfStringAndEnumString_test.yaml");

        Schema schema = openAPI.getComponents().getSchemas().get("AnyOfTest");
        assertEquals(schema.getAnyOf().size(), 2);

        Map<String, String> options = new HashMap<>();
        options.put("SIMPLIFY_ANYOF_STRING_AND_ENUM_STRING", "true");
        OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, options);
        openAPINormalizer.normalize();

        Schema schema3 = openAPI.getComponents().getSchemas().get("AnyOfTest");
        assertNull(schema3.getAnyOf());
        assertTrue(schema3 instanceof StringSchema);
        assertTrue(schema3.getEnum().size() > 0);
    }

    @Test
    public void testSimplifyOneOfAnyOfEnum() throws Exception {
        // Load OpenAPI spec from external YAML file
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/simplifyOneOfWithEnums_test.yaml");

        // Test with rule enabled (default)
        Map<String, String> options = new HashMap<>();
        options.put("SIMPLIFY_ONEOF_ANYOF_ENUM", "true");
        OpenAPINormalizer normalizer = new OpenAPINormalizer(openAPI, options);
        normalizer.normalize();

        // Verify component schema was simplified
        Schema colorSchema = openAPI.getComponents().getSchemas().get("ColorEnum");
        assertNull(colorSchema.getOneOf());
        assertEquals(colorSchema.getType(), "string");
        assertEquals(colorSchema.getEnum(), Arrays.asList("red", "green", "blue", "yellow", "purple"));

        Schema statusSchema = openAPI.getComponents().getSchemas().get("StatusEnum");
        assertNull(statusSchema.getOneOf());
        assertEquals(statusSchema.getType(), "number");
        assertEquals(statusSchema.getEnum(), Arrays.asList(1, 2, 3));

        // Verify parameter schema was simplified
        Parameter param = openAPI.getPaths().get("/test").getGet().getParameters().get(0);
        assertNull(param.getSchema().getOneOf());
        assertEquals(param.getSchema().getType(), "string");
        assertEquals(param.getSchema().getEnum(), Arrays.asList("option1", "option2"));

        // Verify parameter schema was simplified
        Parameter anyOfParam = openAPI.getPaths().get("/test").getGet().getParameters().get(1);
        assertNull(anyOfParam.getSchema().getAnyOf());
        assertEquals(anyOfParam.getSchema().getType(), "string");
        assertEquals(anyOfParam.getSchema().getEnum(), Arrays.asList("anyof 1", "anyof 2"));
        assertEquals(anyOfParam.getSchema().getExtensions().get("x-enum-descriptions"), Arrays.asList("title 1", "title 2"));

        Schema combinedRefsEnum = openAPI.getComponents().getSchemas().get("combinedRefsEnum");

        assertEquals(anyOfParam.getSchema().getType(), "string");
        assertNull(combinedRefsEnum.get$ref());
        assertEquals(combinedRefsEnum.getEnum(), Arrays.asList("A", "B", "C", "D"));
        assertNull(combinedRefsEnum.getOneOf());

        // Test with rule disabled
        OpenAPI openAPI2 = TestUtils.parseSpec("src/test/resources/3_0/simplifyOneOfWithEnums_test.yaml");
        Map<String, String> options2 = new HashMap<>();
        options2.put("SIMPLIFY_ONEOF_ANYOF_ENUM", "false");
        OpenAPINormalizer normalizer2 = new OpenAPINormalizer(openAPI2, options2);
        normalizer2.normalize();

        // oneOf will be removed, as they are in this normalizer if a primitive type has a oneOf
        Schema colorSchema2 = openAPI2.getComponents().getSchemas().get("ColorEnum");
        assertNull(colorSchema2.getOneOf());
        assertNull(colorSchema2.getEnum());

        //If you put string on every subscheme of oneOf, it does not remove it. This might need a fix at some other time
        Parameter param2 = openAPI2.getPaths().get("/test").getGet().getParameters().get(0);
        assertNotNull(param2.getSchema().getOneOf());
        assertNull(param2.getSchema().getEnum());

        //but here it does
        Parameter anyOfParam2 = openAPI2.getPaths().get("/test").getGet().getParameters().get(1);
        assertNull(anyOfParam2.getSchema().getOneOf());
        assertNull(anyOfParam2.getSchema().getEnum());

    }

    @Test
    public void testOpenAPINormalizerSimplifyOneOfAnyOf() {
        // to test the rule SIMPLIFY_ONEOF_ANYOF
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/simplifyOneOfAnyOf_test.yaml");

        Schema schema = openAPI.getComponents().getSchemas().get("AnyOfTest");
        assertEquals(schema.getAnyOf().size(), 4);
        assertNull(schema.getNullable());

        Schema schema2 = openAPI.getComponents().getSchemas().get("OneOfTest");
        assertEquals(schema2.getOneOf().size(), 4);
        assertNull(schema2.getNullable());

        Schema schema2b = openAPI.getComponents().getSchemas().get("OneOfTest2");
        assertEquals(schema2b.getOneOf().size(), 2);
        assertNull(schema2b.getNullable());

        Schema schema5 = openAPI.getComponents().getSchemas().get("OneOfNullableTest");
        assertEquals(schema5.getOneOf().size(), 3);
        assertNull(schema5.getNullable());

        Schema schema7 = openAPI.getComponents().getSchemas().get("Parent");
        assertEquals(((Schema) schema7.getProperties().get("number")).getAnyOf().size(), 1);

        Schema schema9 = openAPI.getComponents().getSchemas().get("AnyOfStringArrayOfString");
        assertEquals(schema9.getAnyOf().size(), 2);

        Schema schema11 = openAPI.getComponents().getSchemas().get("AnyOfAnyType");
        assertEquals(schema11.getAnyOf().size(), 6);

        Schema schema13 = openAPI.getComponents().getSchemas().get("OneOfAnyType");
        assertEquals(schema13.getOneOf().size(), 6);

        Schema schema15 = openAPI.getComponents().getSchemas().get("AnyOfAnyTypeWithRef");
        assertEquals(schema15.getAnyOf().size(), 6);

        Schema schema17 = openAPI.getComponents().getSchemas().get("ParentWithOneOfProperty");
        assertEquals(((Schema) schema17.getProperties().get("number")).getOneOf().size(), 1);

        Schema schema19 = openAPI.getComponents().getSchemas().get("SingleAnyOfTest");
        assertEquals(schema19.getAnyOf().size(), 1);

        Map<String, String> options = new HashMap<>();
        options.put("SIMPLIFY_ONEOF_ANYOF", "true");
        OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, options);
        openAPINormalizer.normalize();

        Schema schema3 = openAPI.getComponents().getSchemas().get("AnyOfTest");
        assertNull(schema3.getAnyOf());
        assertTrue(schema3 instanceof StringSchema);
        assertTrue(schema3.getNullable());

        Schema schema4 = openAPI.getComponents().getSchemas().get("OneOfTest");
        assertNull(schema4.getOneOf());
        assertTrue(schema4 instanceof IntegerSchema);
        assertTrue(schema4.getNullable());

        Schema schema4b = openAPI.getComponents().getSchemas().get("OneOfTest2");
        assertNull(schema4b.getOneOf());
        assertTrue(schema4b instanceof StringSchema);
        assertTrue(schema4b.getNullable());

        Schema schema6 = openAPI.getComponents().getSchemas().get("OneOfNullableTest");
        assertEquals(schema6.getOneOf().size(), 2);
        assertTrue(schema6.getNullable());

        Schema schema8 = openAPI.getComponents().getSchemas().get("Parent");
        assertEquals(((Schema) schema8.getProperties().get("number")).get$ref(), "#/components/schemas/Number");

        Schema schema10 = openAPI.getComponents().getSchemas().get("AnyOfStringArrayOfString");
        assertEquals(schema10.getAnyOf().size(), 2);

        Schema schema12 = openAPI.getComponents().getSchemas().get("AnyOfAnyType");
        assertEquals(schema12.getAnyOf(), null);
        assertEquals(schema12.getType(), null);

        Schema schema14 = openAPI.getComponents().getSchemas().get("OneOfAnyType");
        assertEquals(schema14.getOneOf(), null);
        assertEquals(schema14.getType(), null);

        Schema schema16 = openAPI.getComponents().getSchemas().get("AnyOfAnyTypeWithRef");
        assertEquals(schema16.getAnyOf(), null);
        assertEquals(schema16.getType(), null);

        Schema schema18 = openAPI.getComponents().getSchemas().get("ParentWithOneOfProperty");
        assertEquals(((Schema) schema18.getProperties().get("number")).get$ref(), "#/components/schemas/Number");

        Schema schema20 = openAPI.getComponents().getSchemas().get("SingleAnyOfTest");
        assertEquals(schema20.getAnyOf(), null);
        assertEquals(schema20.getType(), "string");
        assertEquals(schema20.getEnum().size(), 2);
    }

    @Test
    public void testOpenAPINormalizerSimplifyOneOfWithSingleRef() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/simplifyOneOfAnyOf_test.yaml");

        Schema oneOfWithSingleRef = openAPI.getComponents().getSchemas().get("ParentWithOneOfProperty");
        assertEquals(((Schema) oneOfWithSingleRef.getProperties().get("number")).getOneOf().size(), 1);

        Map<String, String> options = new HashMap<>();
        options.put("SIMPLIFY_ONEOF_ANYOF", "true");
        OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, options);
        openAPINormalizer.normalize();

        oneOfWithSingleRef = openAPI.getComponents().getSchemas().get("ParentWithOneOfProperty");
        assertEquals(((Schema) oneOfWithSingleRef.getProperties().get("number")).get$ref(), "#/components/schemas/Number");
    }

    @Test
    public void testOpenAPINormalizerSimplifyBooleanEnum() {
        // to test the rule SIMPLIFY_BOOLEAN_ENUM
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/simplifyBooleanEnum_test.yaml");

        Schema schema = openAPI.getComponents().getSchemas().get("BooleanEnumTest");
        assertEquals(schema.getProperties().size(), 3);
        assertTrue(schema.getProperties().get("boolean_enum") instanceof BooleanSchema);
        BooleanSchema bs = (BooleanSchema) schema.getProperties().get("boolean_enum");
        assertEquals(bs.getEnum().size(), 2);

        Map<String, String> options = new HashMap<>();
        options.put("SIMPLIFY_BOOLEAN_ENUM", "true");
        OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, options);
        openAPINormalizer.normalize();

        Schema schema3 = openAPI.getComponents().getSchemas().get("BooleanEnumTest");
        assertEquals(schema.getProperties().size(), 3);
        assertTrue(schema.getProperties().get("boolean_enum") instanceof BooleanSchema);
        BooleanSchema bs2 = (BooleanSchema) schema.getProperties().get("boolean_enum");
        assertNull(bs2.getEnum()); //ensure the enum has been erased
    }

    @Test
    public void testOpenAPINormalizerSimplifyBooleanEnumWithComposedSchema() {
        // to test the rule SIMPLIFY_BOOLEAN_ENUM
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/simplifyBooleanEnum_test.yaml");

        Schema schema = openAPI.getComponents().getSchemas().get("ComposedSchemaBooleanEnumTest");
        assertEquals(schema.getProperties().size(), 3);
        assertTrue(schema.getProperties().get("boolean_enum") instanceof BooleanSchema);
        BooleanSchema bs = (BooleanSchema) schema.getProperties().get("boolean_enum");
        assertEquals(bs.getEnum().size(), 2);

        Map<String, String> options = new HashMap<>();
        options.put("SIMPLIFY_BOOLEAN_ENUM", "true");
        OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, options);
        openAPINormalizer.normalize();

        Schema schema3 = openAPI.getComponents().getSchemas().get("ComposedSchemaBooleanEnumTest");
        assertEquals(schema.getProperties().size(), 3);
        assertTrue(schema.getProperties().get("boolean_enum") instanceof BooleanSchema);
        BooleanSchema bs2 = (BooleanSchema) schema.getProperties().get("boolean_enum");
        assertNull(bs2.getEnum()); //ensure the enum has been erased
    }

    @Test
    public void testOpenAPINormalizerSetTagsInAllOperations() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/enableKeepOnlyFirstTagInOperation_test.yaml");

        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getGet().getTags().size(), 2);
        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getDelete().getTags().size(), 1);

        Map<String, String> options = new HashMap<>();
        options.put("SET_TAGS_FOR_ALL_OPERATIONS", "core");
        OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, options);
        openAPINormalizer.normalize();

        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getGet().getTags().size(), 1);
        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getDelete().getTags().size(), 1);
        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getGet().getTags().get(0), "core");
        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getDelete().getTags().get(0), "core");
    }

    @Test
    public void testOpenAPINormalizerSetTagsToOperationId() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/enableKeepOnlyFirstTagInOperation_test.yaml");

        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getGet().getTags().size(), 2);
        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getDelete().getTags().size(), 1);

        Map<String, String> options = new HashMap<>();
        options.put("SET_TAGS_TO_OPERATIONID", "true");
        OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, options);
        openAPINormalizer.normalize();

        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getGet().getTags().size(), 1);
        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getDelete().getTags().size(), 1);
        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getGet().getTags().get(0), "list");
        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getDelete().getTags().get(0), "delete");
    }

    @Test
    public void testOpenAPINormalizerSetTagsToVendorExtension() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/enableSetTagsToVendorExtension_test.yaml");

        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getGet().getTags().size(), 2);
        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getGet().getExtensions().size(), 1);

        Map<String, String> options = new HashMap<>();
        options.put("SET_TAGS_TO_VENDOR_EXTENSION", "x-tags");
        OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, options);
        openAPINormalizer.normalize();

        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getGet().getTags().size(), 1);
    }

    @Test
    public void testAddUnsignedToIntegerWithInvalidMaxValue() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/addUnsignedToIntegerWithInvalidMaxValue_test.yaml");

        Schema person = openAPI.getComponents().getSchemas().get("Person");
        assertNull(((Schema) person.getProperties().get("integer")).getExtensions());
        assertNull(((Schema) person.getProperties().get("int32")).getExtensions());
        assertNull(((Schema) person.getProperties().get("int64")).getExtensions());
        assertNull(((Schema) person.getProperties().get("integer_max")).getExtensions());
        assertNull(((Schema) person.getProperties().get("int32_max")).getExtensions());
        assertNull(((Schema) person.getProperties().get("int64_max")).getExtensions());

        Map<String, String> options = new HashMap<>();
        options.put("ADD_UNSIGNED_TO_INTEGER_WITH_INVALID_MAX_VALUE", "true");
        OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, options);
        openAPINormalizer.normalize();

        Schema person2 = openAPI.getComponents().getSchemas().get("Person");
        assertNull(((Schema) person2.getProperties().get("integer")).getExtensions());
        assertNull(((Schema) person2.getProperties().get("int32")).getExtensions());
        assertNull(((Schema) person2.getProperties().get("int64")).getExtensions());
        assertTrue((Boolean) ((Schema) person2.getProperties().get("integer_max")).getExtensions().get("x-unsigned"));
        assertTrue((Boolean) ((Schema) person2.getProperties().get("int32_max")).getExtensions().get("x-unsigned"));
        assertTrue((Boolean) ((Schema) person2.getProperties().get("int64_max")).getExtensions().get("x-unsigned"));
    }

    @Test
    public void testOpenAPINormalizerConvertEnumNullToNullable() {
        // to test the rule SIMPLIFY_ONEOF_ANYOF, which now also covers CONVERT_ENUM_NULL_TO_NULLABLE (removed)
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/convertEnumNullToNullable_test.yaml");

        Schema schema = openAPI.getComponents().getSchemas().get("AnyOfTest");
        assertEquals(schema.getAnyOf().size(), 4);
        assertNull(schema.getNullable());

        Map<String, String> options = new HashMap<>();
        options.put("SIMPLIFY_ONEOF_ANYOF", "true");
        OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, options);
        openAPINormalizer.normalize();

        Schema schema3 = openAPI.getComponents().getSchemas().get("AnyOfTest");
        assertEquals(schema3.getAnyOf().size(), 2);
        assertTrue(schema3.getNullable());
    }

    @Test
    public void testOpenAPINormalizerDefaultRules() {
        // to test the rule SIMPLIFY_ONEOF_ANYOF, which now also covers CONVERT_ENUM_NULL_TO_NULLABLE (removed)
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/convertEnumNullToNullable_test.yaml");

        Schema schema = openAPI.getComponents().getSchemas().get("AnyOfTest");
        assertEquals(schema.getAnyOf().size(), 4);
        assertNull(schema.getNullable());

        Map<String, String> options = new HashMap<>();
        // SIMPLIFY_ONEOF_ANYOF is switched on by default as part of v7.0.0 release
        //options.put("SIMPLIFY_ONEOF_ANYOF", "true");
        OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, options);
        openAPINormalizer.normalize();

        Schema schema3 = openAPI.getComponents().getSchemas().get("AnyOfTest");
        assertEquals(schema3.getAnyOf().size(), 2);
        assertTrue(schema3.getNullable());
    }

    @Test
    public void testOpenAPINormalizerDisableAll() {
        // to test the rule SIMPLIFY_ONEOF_ANYOF, which now also covers CONVERT_ENUM_NULL_TO_NULLABLE (removed)
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/convertEnumNullToNullable_test.yaml");

        // before test
        Schema schema = openAPI.getComponents().getSchemas().get("AnyOfTest");
        assertEquals(schema.getAnyOf().size(), 4);
        assertNull(schema.getNullable());

        Map<String, String> options = new HashMap<>();
        options.put("DISABLE_ALL", "true");
        OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, options);
        openAPINormalizer.normalize();

        // checks should be the same after test
        Schema schema3 = openAPI.getComponents().getSchemas().get("AnyOfTest");
        assertEquals(schema3.getAnyOf().size(), 4);
        assertNull(schema3.getNullable());
    }

    @Test
    public void testOpenAPINormalizerRefactorAllOfWithPropertiesOnly() {
        // to test the rule REFACTOR_ALLOF_WITH_PROPERTIES_ONLY
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/allOf_extension_parent.yaml");

        ComposedSchema schema = (ComposedSchema) openAPI.getComponents().getSchemas().get("allOfWithProperties");
        assertEquals(schema.getAllOf().size(), 1);
        assertEquals(schema.getProperties().size(), 2);
        assertEquals(((Schema) schema.getProperties().get("isParent")).getType(), "boolean");
        assertEquals(((Schema) schema.getProperties().get("mum_or_dad")).getType(), "string");

        Map<String, String> options = new HashMap<>();
        options.put("REFACTOR_ALLOF_WITH_PROPERTIES_ONLY", "true");
        OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, options);
        openAPINormalizer.normalize();

        Schema schema2 = openAPI.getComponents().getSchemas().get("allOfWithProperties");
        assertEquals(schema2.getAllOf().size(), 2);
        assertNull(schema2.getProperties());

        Schema newSchema = (Schema) (schema2.getAllOf().get(1));
        assertEquals(((Schema) newSchema.getProperties().get("isParent")).getType(), "boolean");
        assertEquals(((Schema) newSchema.getProperties().get("mum_or_dad")).getType(), "string");
        assertEquals(newSchema.getRequired().get(0), "isParent");
    }

    @Test
    public void testNormalize31Schema() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_1/common-parameters.yaml");

        Map<String, String> inputRules = Map.of(
                "NORMALIZE_31SPEC", "true"
        );
        OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, inputRules);
        openAPINormalizer.normalize();

        Schema pet = openAPI.getComponents().getSchemas().get("Pet");
        // verify schema for property id
        Schema petSchema = (Schema) pet.getProperties().get("id");
        // both type and types are defined
        assertNotNull(petSchema.getType());
        assertNotNull(petSchema.getTypes());
    }

    @Test
    public void testNormalize31Parameters() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_1/common-parameters.yaml");

        Map<String, String> inputRules = Map.of(
                "NORMALIZE_31SPEC", "true"
        );
        OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, inputRules);
        openAPINormalizer.normalize();

        PathItem pathItem = openAPI.getPaths().get("/pet/{petId}");
        assertNotNull(pathItem);

        // check common parameters
        assertEquals(pathItem.getParameters().size(), 1);
        assertNotNull(pathItem.getParameters().get(0).getSchema().getType());
        assertNotNull(pathItem.getParameters().get(0).getSchema().getTypes());

        // check operation (delete) parameters
        assertEquals(pathItem.getDelete().getParameters().size(), 1);
        assertNotNull(pathItem.getDelete().getParameters().get(0).getSchema().getType());
        assertNotNull(pathItem.getDelete().getParameters().get(0).getSchema().getTypes());
    }

    @Test
    public void testRemoveXInternal() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/enableKeepOnlyFirstTagInOperation_test.yaml");
        Schema s = openAPI.getComponents().getSchemas().get("Dummy");

        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getGet().getExtensions(), null);
        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getDelete().getExtensions().get(X_INTERNAL), true);
        assertEquals(s.getExtensions().get(X_INTERNAL), true);

        Map<String, String> options = new HashMap<>();
        options.put("REMOVE_X_INTERNAL", "true");
        OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, options);
        openAPINormalizer.normalize();

        Schema s2 = openAPI.getComponents().getSchemas().get("Dummy");
        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getGet().getExtensions(), null);
        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getDelete().getExtensions().get(X_INTERNAL), null);
        assertEquals(s2.getExtensions().get(X_INTERNAL), null);
    }

    @Test
    public void testOperationIdFilter() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/enableKeepOnlyFirstTagInOperation_test.yaml");

        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getGet().getExtensions(), null);
        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getDelete().getExtensions().get(X_INTERNAL), true);
        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getPut().getExtensions(), null);

        Map<String, String> options = new HashMap<>();
        options.put("FILTER", "operationId:delete|list");
        OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, options);
        openAPINormalizer.normalize();

        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getGet().getExtensions().get(X_INTERNAL), false);
        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getDelete().getExtensions().get(X_INTERNAL), false);
        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getPut().getExtensions().get(X_INTERNAL), true);
    }

    @Test
    public void testOperationIdFilterWithTrim() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/enableKeepOnlyFirstTagInOperation_test.yaml");

        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getGet().getExtensions(), null);
        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getDelete().getExtensions().get(X_INTERNAL), true);
        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getPut().getExtensions(), null);

        Map<String, String> options = new HashMap<>();
        options.put("FILTER", "operationId:\n\t\t\t\tdelete|\n\t\tlist");
        OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, options);
        openAPINormalizer.normalize();

        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getGet().getExtensions().get(X_INTERNAL), false);
        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getDelete().getExtensions().get(X_INTERNAL), false);
        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getPut().getExtensions().get(X_INTERNAL), true);
    }

    @Test
    public void testFilterWithMethod() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/enableKeepOnlyFirstTagInOperation_test.yaml");

        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getGet().getExtensions(), null);
        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getDelete().getExtensions().get(X_INTERNAL), true);
        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getPut().getExtensions(), null);

        Map<String, String> options = new HashMap<>();
        options.put("FILTER", "method:get");
        OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, options);
        openAPINormalizer.normalize();

        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getGet().getExtensions().get(X_INTERNAL), false);
        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getDelete().getExtensions().get(X_INTERNAL), true);
        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getPut().getExtensions().get(X_INTERNAL), true);
    }
    @Test
    public void testFilterWithMethodWithTrim() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/enableKeepOnlyFirstTagInOperation_test.yaml");

        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getGet().getExtensions(), null);
        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getDelete().getExtensions().get(X_INTERNAL), true);
        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getPut().getExtensions(), null);

        Map<String, String> options = new HashMap<>();
        options.put("FILTER", "method:\n\t\t\t\tget");
        OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, options);
        openAPINormalizer.normalize();

        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getGet().getExtensions().get(X_INTERNAL), false);
        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getDelete().getExtensions().get(X_INTERNAL), true);
        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getPut().getExtensions().get(X_INTERNAL), true);
    }

    @Test
    public void testFilterWithTag() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/enableKeepOnlyFirstTagInOperation_test.yaml");

        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getGet().getExtensions(), null);
        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getDelete().getExtensions().get(X_INTERNAL), true);
        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getPut().getExtensions(), null);

        Map<String, String> options = new HashMap<>();
        options.put("FILTER", "tag:basic");
        OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, options);
        openAPINormalizer.normalize();

        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getGet().getExtensions().get(X_INTERNAL), false);
        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getDelete().getExtensions().get(X_INTERNAL), true);
        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getPut().getExtensions().get(X_INTERNAL), true);
    }
    @Test
    public void testFilterWithTagWithTrim() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/enableKeepOnlyFirstTagInOperation_test.yaml");

        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getGet().getExtensions(), null);
        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getDelete().getExtensions().get(X_INTERNAL), true);
        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getPut().getExtensions(), null);

        Map<String, String> options = new HashMap<>();
        options.put("FILTER", "tag:basic");
        OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, options);
        openAPINormalizer.normalize();

        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getGet().getExtensions().get(X_INTERNAL), false);
        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getDelete().getExtensions().get(X_INTERNAL), true);
        assertEquals(openAPI.getPaths().get("/person/display/{personId}").getPut().getExtensions().get(X_INTERNAL), true);
    }

    @Test
    public void testComposedSchemaDoesNotThrow() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_1/composed-schema.yaml");

        OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, Collections.emptyMap());
        openAPINormalizer.normalize();
    }

    @Test
    public void testSetContainerToNullable() {
        // test `array|map`
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/setContainerToNullable_test.yaml");

        Schema schema = openAPI.getComponents().getSchemas().get("Person");
        assertEquals(((Schema) schema.getProperties().get("array_property")).getNullable(), null);
        assertEquals(((Schema) schema.getProperties().get("set_property")).getNullable(), null);
        assertEquals(((Schema) schema.getProperties().get("map_property")).getNullable(), null);

        Map<String, String> options = new HashMap<>();
        options.put("SET_CONTAINER_TO_NULLABLE", "array|map");
        OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, options);
        openAPINormalizer.normalize();

        Schema schema2 = openAPI.getComponents().getSchemas().get("Person");
        assertEquals(((Schema) schema2.getProperties().get("array_property")).getNullable(), true);
        assertEquals(((Schema) schema2.getProperties().get("set_property")).getNullable(), null);
        assertEquals(((Schema) schema2.getProperties().get("map_property")).getNullable(), true);

        // test `set`
        OpenAPI openAPI2 = TestUtils.parseSpec("src/test/resources/3_0/setContainerToNullable_test.yaml");

        Schema schema3 = openAPI2.getComponents().getSchemas().get("Person");
        assertEquals(((Schema) schema3.getProperties().get("array_property")).getNullable(), null);
        assertEquals(((Schema) schema3.getProperties().get("set_property")).getNullable(), null);
        assertEquals(((Schema) schema3.getProperties().get("map_property")).getNullable(), null);

        options.put("SET_CONTAINER_TO_NULLABLE", "set");
        OpenAPINormalizer openAPINormalizer2 = new OpenAPINormalizer(openAPI2, options);
        openAPINormalizer2.normalize();

        Schema schema4 = openAPI2.getComponents().getSchemas().get("Person");
        assertEquals(((Schema) schema4.getProperties().get("array_property")).getNullable(), null);
        assertEquals(((Schema) schema4.getProperties().get("set_property")).getNullable(), true);
        assertEquals(((Schema) schema4.getProperties().get("map_property")).getNullable(), null);
    }

    @Test
    public void testSetPrimitiveTypesToNullable() {
        // test `string|integer|number|boolean`
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/setPrimitiveTypesToNullable_test.yaml");

        Schema schema = openAPI.getComponents().getSchemas().get("Person");
        assertEquals(((Schema) schema.getProperties().get("lastName")).getNullable(), null);
        assertEquals(((Schema) schema.getProperties().get("first_integer")).getNullable(), null);
        assertEquals(((Schema) schema.getProperties().get("first_number")).getNullable(), null);
        assertEquals(((Schema) schema.getProperties().get("first_boolean")).getNullable(), null);

        Map<String, String> options = new HashMap<>();
        options.put("SET_PRIMITIVE_TYPES_TO_NULLABLE", "string|integer|number|boolean");
        OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, options);
        openAPINormalizer.normalize();

        Schema schema2 = openAPI.getComponents().getSchemas().get("Person");
        assertEquals(((Schema) schema2.getProperties().get("lastName")).getNullable(), true);
        assertEquals(((Schema) schema2.getProperties().get("first_integer")).getNullable(), true);
        assertEquals(((Schema) schema2.getProperties().get("first_number")).getNullable(), true);
        assertEquals(((Schema) schema2.getProperties().get("first_boolean")).getNullable(), true);

        // test `number` only
        OpenAPI openAPI2 = TestUtils.parseSpec("src/test/resources/3_0/setPrimitiveTypesToNullable_test.yaml");

        Schema schema3 = openAPI2.getComponents().getSchemas().get("Person");
        assertEquals(((Schema) schema3.getProperties().get("lastName")).getNullable(), null);
        assertEquals(((Schema) schema3.getProperties().get("first_integer")).getNullable(), null);
        assertEquals(((Schema) schema3.getProperties().get("first_number")).getNullable(), null);
        assertEquals(((Schema) schema3.getProperties().get("first_boolean")).getNullable(), null);

        options.put("SET_PRIMITIVE_TYPES_TO_NULLABLE", "number");
        OpenAPINormalizer openAPINormalizer2 = new OpenAPINormalizer(openAPI2, options);
        openAPINormalizer2.normalize();

        Schema schema4 = openAPI2.getComponents().getSchemas().get("Person");
        assertEquals(((Schema) schema4.getProperties().get("lastName")).getNullable(), null);
        assertEquals(((Schema) schema4.getProperties().get("first_integer")).getNullable(), null);
        assertEquals(((Schema) schema4.getProperties().get("first_number")).getNullable(), true);
        assertEquals(((Schema) schema4.getProperties().get("first_boolean")).getNullable(), null);
    }

    @Test
    public void testOpenAPINormalizerSimplifyOneOfAnyOf31SpecForIssue18184() {
        // to test the rule SIMPLIFY_ONEOF_ANYOF in 3.1 spec
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_1/issue_18184.yaml");
        // test spec contains anyOf with a ref to enum and another scheme type is null

        Schema schema = openAPI.getComponents().getSchemas().get("Item");
        assertEquals(((Schema) schema.getProperties().get("my_enum")).getAnyOf().size(), 2);

        Map<String, String> options = new HashMap<>();
        options.put("SIMPLIFY_ANYOF_STRING_AND_ENUM_STRING", "true");
        OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, options);
        openAPINormalizer.normalize();

        Schema schema2 = openAPI.getComponents().getSchemas().get("Item");
        assertEquals(((Schema) schema2.getProperties().get("my_enum")).getAnyOf(), null);
        assertEquals(((Schema) schema2.getProperties().get("my_enum")).get$ref(), "#/components/schemas/MyEnum");
    }

    @Test
    public void testOpenAPINormalizerProcessingArraySchema31Spec() {
        // to test array schema processing in 3.1 spec
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_1/issue_18291.yaml");

        Schema schema = openAPI.getComponents().getSchemas().get("Foo");
        assertEquals(((Schema) schema.getProperties().get("arrayOfStrings")).getTypes().size(), 1);
        assertEquals(((Schema) schema.getProperties().get("arrayOfStrings")).getTypes().contains("array"), true);
        assertEquals(((Schema) schema.getProperties().get("arrayOfStrings")).getType(), null);
        assertEquals(ModelUtils.isArraySchema((Schema) schema.getProperties().get("arrayOfStrings")), true);
        assertEquals(((Schema) schema.getProperties().get("arrayOfStrings")).getItems().getType(), null);
        assertEquals(((Schema) schema.getProperties().get("arrayOfStrings")).getItems().getTypes().contains("string"), true);

        Schema schema3 = openAPI.getComponents().getSchemas().get("Bar");
        assertEquals(((Schema) schema3.getAllOf().get(0)).get$ref(), "#/components/schemas/Foo");

        Schema schema5 = ModelUtils.getSchema(openAPI, ModelUtils.getSimpleRef(((Schema) schema3.getAllOf().get(0)).get$ref()));
        assertEquals(((Schema) schema5.getProperties().get("arrayOfStrings")).getTypes().size(), 1);
        assertEquals(((Schema) schema5.getProperties().get("arrayOfStrings")).getTypes().contains("array"), true);
        assertEquals(((Schema) schema5.getProperties().get("arrayOfStrings")).getType(), null);
        assertEquals(ModelUtils.isArraySchema((Schema) schema5.getProperties().get("arrayOfStrings")), true);
        assertEquals(((Schema) schema5.getProperties().get("arrayOfStrings")).getItems().getType(), null);
        assertEquals(((Schema) schema5.getProperties().get("arrayOfStrings")).getItems().getTypes().contains("string"), true);

        Schema schema7 = openAPI.getComponents().getSchemas().get("ArrayWithPrefixItems");
        assertEquals(((Schema) schema7.getProperties().get("with_prefixitems")).getItems(), null);
        assertNotEquals(((Schema) schema7.getProperties().get("with_prefixitems")).getPrefixItems(), null);
        assertEquals(((Schema) schema7.getProperties().get("without_items")).getItems(), null);

        Schema schema9 = openAPI.getComponents().getSchemas().get("AnyOfArrayWithPrefixItems");
        assertEquals(((Schema) schema9.getAnyOf().get(0)).getItems(), null);
        assertNotEquals(((Schema) schema9.getAnyOf().get(0)).getPrefixItems(), null);
        assertEquals(((Schema) schema9.getAnyOf().get(1)).getItems(), null);

        Schema schema11 = openAPI.getComponents().getSchemas().get("OneOfArrayWithPrefixItems");
        assertEquals(((Schema) schema11.getOneOf().get(0)).getItems(), null);
        assertNotEquals(((Schema) schema11.getOneOf().get(0)).getPrefixItems(), null);
        assertEquals(((Schema) schema11.getOneOf().get(1)).getItems(), null);

        Map<String, String> inputRules = Map.of("NORMALIZE_31SPEC", "true");
        OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, inputRules);
        openAPINormalizer.normalize();

        Schema schema2 = openAPI.getComponents().getSchemas().get("Foo");
        assertEquals(((Schema) schema2.getProperties().get("arrayOfStrings")).getTypes().size(), 1);
        assertEquals(((Schema) schema2.getProperties().get("arrayOfStrings")).getTypes().contains("array"), true);
        assertEquals(ModelUtils.isArraySchema((Schema) schema2.getProperties().get("arrayOfStrings")), true);
        assertEquals(((Schema) schema2.getProperties().get("arrayOfStrings")).getItems().getTypes().contains("string"), true);
        assertEquals(((Schema) schema2.getProperties().get("arrayOfStrings")).getItems().getType(), "string");
        assertEquals(((Schema) schema2.getProperties().get("arrayOfStrings")).getType(), "array");

        Schema schema4 = openAPI.getComponents().getSchemas().get("Bar");
        assertEquals(((Schema) schema4.getAllOf().get(0)).get$ref(), "#/components/schemas/Foo");

        Schema schema6 = ModelUtils.getSchema(openAPI, ModelUtils.getSimpleRef(((Schema) schema4.getAllOf().get(0)).get$ref()));
        assertEquals(((Schema) schema6.getProperties().get("arrayOfStrings")).getTypes().size(), 1);
        assertEquals(((Schema) schema6.getProperties().get("arrayOfStrings")).getTypes().contains("array"), true);
        assertEquals(ModelUtils.isArraySchema((Schema) schema6.getProperties().get("arrayOfStrings")), true);
        assertEquals(((Schema) schema6.getProperties().get("arrayOfStrings")).getItems().getTypes().contains("string"), true);
        assertEquals(((Schema) schema6.getProperties().get("arrayOfStrings")).getItems().getType(), "string");
        assertEquals(((Schema) schema6.getProperties().get("arrayOfStrings")).getType(), "array");

        Schema schema8 = openAPI.getComponents().getSchemas().get("ArrayWithPrefixItems");
        assertNotEquals(((Schema) schema8.getProperties().get("with_prefixitems")).getItems(), null);
        assertEquals(((Schema) schema8.getProperties().get("with_prefixitems")).getPrefixItems(), null);
        assertNotEquals(((Schema) schema8.getProperties().get("without_items")).getItems(), null);

        Schema schema10 = openAPI.getComponents().getSchemas().get("AnyOfArrayWithPrefixItems");
        assertNotEquals(((Schema) schema10.getAnyOf().get(0)).getItems(), null);
        assertEquals(((Schema) schema10.getAnyOf().get(0)).getPrefixItems(), null);
        assertNotEquals(((Schema) schema10.getAnyOf().get(1)).getItems(), null);

        Schema schema12 = openAPI.getComponents().getSchemas().get("OneOfArrayWithPrefixItems");
        assertNotEquals(((Schema) schema12.getOneOf().get(0)).getItems(), null);
        assertEquals(((Schema) schema12.getOneOf().get(0)).getPrefixItems(), null);
        assertNotEquals(((Schema) schema12.getOneOf().get(1)).getItems(), null);
    }

    @Test
    public void testOpenAPINormalizerProcessingArraySchema31NullabilitySpec() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_1/null-types-simple.yaml");
        Schema schema = openAPI.getComponents().getSchemas().get("WithNullableType");

        assertNull(((Schema) schema.getProperties().get("arrayDataOrNull")).getNullable());
        assertNull(((Schema) schema.getProperties().get("stringDataOrNull")).getNullable());
        assertNull(((Schema) schema.getProperties().get("oneofOrNull")).getNullable());

        Map<String, String> inputRules = Map.of("NORMALIZE_31SPEC", "true");
        OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, inputRules);
        openAPINormalizer.normalize();

        assertTrue(((Schema) schema.getProperties().get("arrayDataOrNull")).getNullable());
        assertTrue(((Schema) schema.getProperties().get("stringDataOrNull")).getNullable());
        assertTrue(((Schema) schema.getProperties().get("oneofOrNull")).getNullable());
    }

    @Test
    public void testOpenAPINormalizerSimplifyOneOfAnyOf31Spec() {
        // to test the rule SIMPLIFY_ONEOF_ANYOF with 3.1 spec
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_1/simplifyOneOfAnyOf_test.yaml");

        Schema schema = openAPI.getComponents().getSchemas().get("AnyOfTest");
        assertEquals(schema.getAnyOf().size(), 4);
        assertNull(schema.getNullable());

        Schema schema2 = openAPI.getComponents().getSchemas().get("OneOfTest");
        assertEquals(schema2.getOneOf().size(), 4);
        assertNull(schema2.getNullable());

        Schema schema2b = openAPI.getComponents().getSchemas().get("OneOfTest2");
        assertEquals(schema2b.getOneOf().size(), 2);
        assertNull(schema2b.getNullable());

        Schema schema5 = openAPI.getComponents().getSchemas().get("OneOfNullableTest");
        assertEquals(schema5.getOneOf().size(), 3);
        assertNull(schema5.getNullable());

        Schema schema7 = openAPI.getComponents().getSchemas().get("Parent");
        assertEquals(((Schema) schema7.getProperties().get("number")).getAnyOf().size(), 1);

        Schema schema9 = openAPI.getComponents().getSchemas().get("AnyOfStringArrayOfString");
        assertEquals(schema9.getAnyOf().size(), 2);

        Schema schema11 = openAPI.getComponents().getSchemas().get("AnyOfAnyType");
        assertEquals(schema11.getAnyOf().size(), 6);

        Schema schema13 = openAPI.getComponents().getSchemas().get("OneOfAnyType");
        assertEquals(schema13.getOneOf().size(), 6);

        Schema schema15 = openAPI.getComponents().getSchemas().get("TypeIntegerWithOneOf");
        assertEquals(schema15.getOneOf().size(), 3);

        Schema schema17 = openAPI.getComponents().getSchemas().get("OneOfNullAndRef3");
        assertEquals(schema17.getOneOf().size(), 2);

        Schema schema19 = openAPI.getComponents().getSchemas().get("ParentWithOneOfProperty");
        assertEquals(((Schema) schema19.getProperties().get("number")).getOneOf().size(), 1);

        Schema schema21 = openAPI.getComponents().getSchemas().get("SingleAnyOfTest");
        assertEquals(schema21.getAnyOf().size(), 1);

        Schema schema23 = openAPI.getComponents().getSchemas().get("PropertiesWithAnyOf");
        assertEquals(((Schema) schema23.getProperties().get("anyof_nullable_string")).getAnyOf().size(), 2);
        assertEquals(((Schema) schema23.getProperties().get("anyof_nullable_number")).getAnyOf().size(), 2);

        // start the normalization
        Map<String, String> options = new HashMap<>();
        options.put("SIMPLIFY_ONEOF_ANYOF", "true");
        OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, options);
        openAPINormalizer.normalize();

        Schema schema3 = openAPI.getComponents().getSchemas().get("AnyOfTest");
        assertNull(schema3.getAnyOf());
        assertEquals(ModelUtils.getType(schema3), "string");
        assertTrue(schema3.getNullable());

        Schema schema4 = openAPI.getComponents().getSchemas().get("OneOfTest");
        assertNull(schema4.getOneOf());
        assertEquals(ModelUtils.getType(schema4), "integer");
        assertTrue(schema4.getNullable());

        Schema schema4b = openAPI.getComponents().getSchemas().get("OneOfTest2");
        assertNull(schema4b.getOneOf());
        assertEquals(ModelUtils.getType(schema4b), "string");
        assertTrue(schema4b.getNullable());

        Schema schema6 = openAPI.getComponents().getSchemas().get("OneOfNullableTest");
        assertEquals(schema6.getOneOf().size(), 2);
        assertTrue(schema6.getNullable());

        Schema schema8 = openAPI.getComponents().getSchemas().get("Parent");
        assertEquals(((Schema) schema8.getProperties().get("number")).get$ref(), "#/components/schemas/Number");

        Schema schema10 = openAPI.getComponents().getSchemas().get("AnyOfStringArrayOfString");
        assertEquals(schema10.getAnyOf().size(), 2);

        Schema schema12 = openAPI.getComponents().getSchemas().get("AnyOfAnyType");
        assertEquals(schema12.getAnyOf(), null);
        assertEquals(schema12.getType(), null);

        Schema schema14 = openAPI.getComponents().getSchemas().get("OneOfAnyType");
        assertEquals(schema14.getOneOf(), null);
        assertEquals(schema14.getType(), null);

        Schema schema16 = openAPI.getComponents().getSchemas().get("TypeIntegerWithOneOf");
        // oneOf should have been removed as the schema is essentially a primitive type
        assertEquals(schema16.getOneOf(), null);

        Schema schema18 = openAPI.getComponents().getSchemas().get("OneOfNullAndRef3");
        // original oneOf removed and simplified to just $ref (oneOf sub-schema) instead
        assertEquals(schema18.getOneOf(), null);
        assertEquals(schema18.get$ref(), "#/components/schemas/Parent");

        Schema schema20 = openAPI.getComponents().getSchemas().get("ParentWithOneOfProperty");
        assertEquals(((Schema) schema20.getProperties().get("number")).get$ref(), "#/components/schemas/Number");

        Schema schema22 = openAPI.getComponents().getSchemas().get("SingleAnyOfTest");
        assertEquals(schema22.getAnyOf(), null);
        assertEquals(schema22.getTypes(), Set.of("string"));
        assertEquals(schema22.getEnum().size(), 2);

        Schema schema24 = openAPI.getComponents().getSchemas().get("PropertiesWithAnyOf");
        assertEquals(((Schema) schema24.getProperties().get("anyof_nullable_string")).getAnyOf(), null);
        assertEquals(((Schema) schema24.getProperties().get("anyof_nullable_string")).getNullable(), true);
        assertEquals(((Schema) schema24.getProperties().get("anyof_nullable_string")).getTypes().size(), 1);
        assertEquals(((Schema) schema24.getProperties().get("anyof_nullable_number")).getAnyOf(), null);
        assertEquals(((Schema) schema24.getProperties().get("anyof_nullable_number")).getNullable(), true);
        assertEquals(((Schema) schema24.getProperties().get("anyof_nullable_number")).getTypes().size(), 1);
    }

    @Test
    public void testOpenAPINormalizerSimplifyOneOfWithSingleRef31Spec() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_1/simplifyOneOfAnyOf_test.yaml");

        Schema oneOfWithSingleRef = openAPI.getComponents().getSchemas().get("ParentWithOneOfProperty");
        assertEquals(((Schema) oneOfWithSingleRef.getProperties().get("number")).getOneOf().size(), 1);

        Map<String, String> options = new HashMap<>();
        options.put("SIMPLIFY_ONEOF_ANYOF", "true");
        OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, options);
        openAPINormalizer.normalize();

        oneOfWithSingleRef = openAPI.getComponents().getSchemas().get("ParentWithOneOfProperty");
        assertEquals(((Schema) oneOfWithSingleRef.getProperties().get("number")).get$ref(), "#/components/schemas/Number");
    }

    @Test
    public void testOpenAPINormalizerSingleConstEnum31Spec() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_1/enum-single-value.yaml");

        Schema reference_3_0 = openAPI.getComponents().getSchemas().get("SingleValueEnum_3_0");
        assertEquals(((Schema) reference_3_0.getProperties().get("type")).getEnum().size(), 1);

        Schema schema = openAPI.getComponents().getSchemas().get("SingleValueEnum_3_1");
        Schema originalTypeSchema = (Schema) schema.getProperties().get("type");
        assertFalse(ModelUtils.isEnumSchema(originalTypeSchema));
        var originalConst = originalTypeSchema.getConst();
        assertNotNull(originalConst);

        Map<String, String> inputRules = Map.of("NORMALIZE_31SPEC", "true");
        OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, inputRules);
        openAPINormalizer.normalize();

        Schema schema2 = openAPI.getComponents().getSchemas().get("SingleValueEnum_3_1");
        Schema normalizedTypeSchema = (Schema) schema2.getProperties().get("type");
        assertTrue(ModelUtils.isEnumSchema(normalizedTypeSchema));
        assertNull(normalizedTypeSchema.getConst());
        assertEquals(normalizedTypeSchema.getEnum().size(), 1);
        assertEquals(Arrays.asList(originalConst), normalizedTypeSchema.getEnum());
    }

    @Test
    public void testOpenAPINormalizerProcessingAllOfSchema31Spec() {
        // to test array schema processing in 3.1 spec
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_1/unsupported_schema_test.yaml");

        Schema schema = openAPI.getComponents().getSchemas().get("Dummy");
        assertEquals(((Schema) schema.getProperties().get("property1")).getAllOf().size(), 2);
        assertNotEquals(((Schema) ((Schema) schema.getProperties().get("property2")).getAllOf().get(0)).getIf(), null); // if is set before normalization
        assertNotEquals(((Schema) ((Schema) schema.getProperties().get("property2")).getAllOf().get(1)).getThen(), null); // then is set before normalization

        Map<String, String> inputRules = Map.of("NORMALIZE_31SPEC", "true");
        OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, inputRules);
        openAPINormalizer.normalize();

        Schema schema2 = openAPI.getComponents().getSchemas().get("Dummy");
        assertEquals(((Schema) schema2.getProperties().get("property1")).getAllOf(), null);
        assertEquals(((Schema) schema2.getProperties().get("property2")).getAllOf(), null);
        assertEquals(((Schema) schema2.getProperties().get("property2")).getAllOf(), null);
    }

    @Test
    public void testOpenAPINormalizerComponentsResponses31Spec() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_1/common-parameters.yaml");
        ApiResponse apiResponse = openAPI.getComponents().getResponses().get("JustAnotherResponse");
        assertEquals(((Schema) apiResponse.getContent().get("application/json").getSchema().getProperties().get("uuid")).getType(), null);
        assertEquals(((Schema) apiResponse.getContent().get("application/json").getSchema().getProperties().get("label")).getType(), null);

        Map<String, String> inputRules = Map.of(
                "NORMALIZE_31SPEC", "true"
        );
        OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, inputRules);
        openAPINormalizer.normalize();

        ApiResponse apiResponse2 = openAPI.getComponents().getResponses().get("JustAnotherResponse");
        assertEquals(((Schema) apiResponse2.getContent().get("application/json").getSchema().getProperties().get("uuid")).getType(), "integer");
        assertEquals(((Schema) apiResponse2.getContent().get("application/json").getSchema().getProperties().get("label")).getType(), "string");
    }

    @Test
    public void testOpenAPINormalizerBearerAuthSpec() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/2_0/globalSecurity.json");
        SecurityScheme scheme = openAPI.getComponents().getSecuritySchemes().get("api_key");
        assertEquals(scheme.getType(), SecurityScheme.Type.APIKEY);
        assertEquals(scheme.getScheme(), null);
        assertEquals(scheme.getName(), "api_key");
        assertEquals(scheme.getIn(), SecurityScheme.In.HEADER);

        Map<String, String> inputRules = Map.of(
                "SET_BEARER_AUTH_FOR_NAME", "api_key"
        );
        OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, inputRules);
        openAPINormalizer.normalize();

        SecurityScheme scheme2 = openAPI.getComponents().getSecuritySchemes().get("api_key");
        assertEquals(scheme.getType(), SecurityScheme.Type.HTTP);
        assertEquals(scheme.getScheme(), "bearer");
        assertEquals(scheme.getName(), null);
        assertEquals(scheme.getIn(), null);
    }

    @Test
    public void testNormalizerClass() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/required-properties.yaml");
        Map<String, String> inputRules = Map.of(
                "NORMALIZER_CLASS", RemoveRequiredNormalizer.class.getName()
        );
        OpenAPINormalizer openAPINormalizer = OpenAPINormalizer.createNormalizer(openAPI, inputRules);
        openAPINormalizer.normalize();
        Schema requiredProperties = openAPI.getComponents().getSchemas().get("RequiredProperties");
        assertEquals(requiredProperties.getRequired(), null);
    }


    @Test
    public void testRemoveXInternalFromInlineProperties() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/inline_x_internal_test.yaml");
        Schema parentSchema = openAPI.getComponents().getSchemas().get("ParentSchema");
        Schema inlineProperty = (Schema) parentSchema.getProperties().get("inlineXInternalProperty");
        
        // Before normalization: x-internal should be present on inline property
        assertNotNull(inlineProperty.getExtensions());
        assertEquals(inlineProperty.getExtensions().get("x-internal"), true);
        
        // Run normalizer with REMOVE_X_INTERNAL=true
        Map<String, String> options = new HashMap<>();
        options.put("REMOVE_X_INTERNAL", "true");
        OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, options);
        openAPINormalizer.normalize();
        
        // After normalization: x-internal should be removed from inline property
        Schema parentSchemaAfter = openAPI.getComponents().getSchemas().get("ParentSchema");
        Schema inlinePropertyAfter = (Schema) parentSchemaAfter.getProperties().get("inlineXInternalProperty");
        
        // x-internal extension should be removed (null or not present in map)
        if (inlinePropertyAfter.getExtensions() != null) {
            assertNull(inlinePropertyAfter.getExtensions().get("x-internal"));
        }
        
        // The property itself should still exist (we're removing the flag, not the property)
        assertNotNull(inlinePropertyAfter);
        assertEquals(inlinePropertyAfter.getType(), "object");
        
        // Nested properties should still exist
        assertNotNull(inlinePropertyAfter.getProperties());
        assertNotNull(inlinePropertyAfter.getProperties().get("nestedField"));
        assertNotNull(inlinePropertyAfter.getProperties().get("nestedNumber"));
    }

    public static class RemoveRequiredNormalizer extends OpenAPINormalizer {

        public RemoveRequiredNormalizer(OpenAPI openAPI, Map<String, String> inputRules) {
            super(openAPI, inputRules);
        }

        @Override
        public Schema normalizeSchema(Schema schema, Set<Schema> visitedSchemas) {
            if (skipNormalization(schema, visitedSchemas)) {
                return schema;
            }
            schema.setRequired(null);
            return super.normalizeSchema(schema, visitedSchemas);
        }
    }
}
