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

import com.google.common.collect.Sets;
import com.samskivert.mustache.Mustache.Lambda;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.headers.Header;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.oas.models.parameters.QueryParameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.responses.ApiResponses;
import io.swagger.v3.oas.models.security.SecurityScheme;
import io.swagger.v3.parser.core.models.ParseOptions;

import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.config.GlobalSettings;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.templating.mustache.CamelCaseLambda;
import org.openapitools.codegen.templating.mustache.IndentedLambda;
import org.openapitools.codegen.templating.mustache.LowercaseLambda;
import org.openapitools.codegen.templating.mustache.TitlecaseLambda;
import org.openapitools.codegen.templating.mustache.UppercaseLambda;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.SemVer;
import org.testng.Assert;
import org.testng.annotations.Ignore;
import org.testng.annotations.Test;

import java.io.File;
import java.nio.file.Files;
import java.util.*;
import java.util.stream.Collectors;

import static org.testng.Assert.*;

public class OpenAPINormalizerTest {
    @Test
    public void testOpenAPINormalizerRefAsParentInAllOf() {
        // to test the rule REF_AS_PARENT_IN_ALLOF
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/allOf_extension_parent.yaml");

        Schema schema = openAPI.getComponents().getSchemas().get("AnotherPerson");
        assertNull(schema.getExtensions());

        Schema schema2 = openAPI.getComponents().getSchemas().get("Person");
        assertEquals(schema2.getExtensions().get("x-parent"), "abstract");

        Map<String, String> options = new HashMap<>();
        options.put("REF_AS_PARENT_IN_ALLOF", "true");
        OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, options);
        openAPINormalizer.normalize();

        Schema schema3 = openAPI.getComponents().getSchemas().get("AnotherPerson");
        assertEquals(schema3.getExtensions().get("x-parent"), true);

        Schema schema4 = openAPI.getComponents().getSchemas().get("AnotherParent");
        assertEquals(schema4.getExtensions().get("x-parent"), true);

        Schema schema5 = openAPI.getComponents().getSchemas().get("Person");
        assertEquals(schema5.getExtensions().get("x-parent"), "abstract");
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
        // to test the rule REMOVE_ANYOF_ONEOF_AND_KEEP_PROPERTIIES_ONLY
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
    public void testOpenAPINormalizerSimplifyOneOfAnyOf() {
        // to test the rule SIMPLIFY_ONEOF_ANYOF
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/simplifyOneOfAnyOf_test.yaml");

        Schema schema = openAPI.getComponents().getSchemas().get("AnyOfTest");
        assertEquals(schema.getAnyOf().size(), 2);
        assertNull(schema.getNullable());

        Schema schema2 = openAPI.getComponents().getSchemas().get("OneOfTest");
        assertEquals(schema2.getOneOf().size(), 2);
        assertNull(schema2.getNullable());

        Schema schema5 = openAPI.getComponents().getSchemas().get("OneOfNullableTest");
        assertEquals(schema5.getOneOf().size(), 3);
        assertNull(schema5.getNullable());

        Schema schema7 = openAPI.getComponents().getSchemas().get("Parent");
        assertEquals(((Schema) schema7.getProperties().get("number")).getAnyOf().size(), 1);

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

        Schema schema6 = openAPI.getComponents().getSchemas().get("OneOfNullableTest");
        assertEquals(schema6.getOneOf().size(), 2);
        assertTrue(schema6.getNullable());

        Schema schema8 = openAPI.getComponents().getSchemas().get("Parent");
        assertEquals(((Schema) schema8.getProperties().get("number")).get$ref(), "#/components/schemas/Number");
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
        assertEquals(schema.getAnyOf().size(), 3);
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
        assertEquals(schema.getAnyOf().size(), 3);
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
        assertEquals(schema.getAnyOf().size(), 3);
        assertNull(schema.getNullable());

        Map<String, String> options = new HashMap<>();
        options.put("DISABLE_ALL", "true");
        OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, options);
        openAPINormalizer.normalize();

        // checks should be the same after test
        Schema schema3 = openAPI.getComponents().getSchemas().get("AnyOfTest");
        assertEquals(schema3.getAnyOf().size(), 3);
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
}
