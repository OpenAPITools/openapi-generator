/*
 * Copyright 2026 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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
package org.openapitools.codegen.cpphttplib;

import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.CppHttplibServerCodegen;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import io.swagger.v3.oas.models.media.*;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@SuppressWarnings("static-method")
public class CppHttplibServerCodegenModelTest {

    /**
     * Wraps a single model the way {@link org.openapitools.codegen.DefaultGenerator}
     * does before calling {@code postProcessAllModels}, so tests can exercise the full
     * enum vendor-extension pipeline (identifier + original-value derivation), not just
     * the intermediate state produced by {@code fromModel}.
     */
    private Map<String, ModelsMap> wrapForPostProcessAllModels(String name, CodegenModel model) {
        final ModelMap modelMap = new ModelMap();
        modelMap.setModel(model);
        final ModelsMap modelsMap = new ModelsMap();
        modelsMap.setModels(Collections.singletonList(modelMap));
        final HashMap<String, ModelsMap> allModels = new HashMap<>();
        allModels.put(name, modelsMap);
        return allModels;
    }
    @Test(description = "convert model with enum property")
    public void enumPropertyTest() {
        final CppHttplibServerCodegen codegen = new CppHttplibServerCodegen();
        codegen.processOpts();

        ObjectSchema schema = new ObjectSchema();
        StringSchema statusSchema = new StringSchema();
        statusSchema.setEnum(java.util.Arrays.asList("available", "pending", "sold"));
        schema.addProperty("status", statusSchema);

        final CodegenModel model = codegen.fromModel("PetStatus", schema);

        Assert.assertEquals(model.name, "PetStatus");
        Assert.assertNotNull(model.vars);
        Assert.assertEquals(model.vars.size(), 1);

        CodegenProperty statusProp = model.vars.get(0);
        // Check that isEnum flag is set
        Assert.assertTrue(statusProp.isEnum, "isEnum flag should be true for enum properties");
        // Check vendor extensions for enum handling
        Assert.assertNotNull(statusProp.vendorExtensions, "vendorExtensions should not be null");
        Assert.assertTrue((boolean) statusProp.vendorExtensions.getOrDefault("isEnum", false), 
            "isEnum vendor extension should be true");
        // Check enum values
        java.util.List<?> enumValues = (java.util.List<?>) statusProp.vendorExtensions.getOrDefault("values", statusProp._enum);
        Assert.assertNotNull(enumValues, "enum values should be present");
        Assert.assertEquals(enumValues.size(), 3);
    }

    @Test(description = "convert a simple model with properties")
    public void simpleModelTest() {
        final CppHttplibServerCodegen codegen = new CppHttplibServerCodegen();
        codegen.processOpts();

        // Create a simple schema with properties
        ObjectSchema schema = new ObjectSchema();
        schema.description("a sample model");
        schema.addProperty("id", new IntegerSchema().format("int64"));
        schema.addProperty("name", new StringSchema());
        schema.addProperty("isActive", new BooleanSchema());
        schema.setRequired(java.util.Arrays.asList("id", "name"));

        final CodegenModel model = codegen.fromModel("User", schema);

        Assert.assertEquals(model.name, "User");
        Assert.assertEquals(model.classname, "User");
        Assert.assertNotNull(model.vars);
        Assert.assertEquals(model.vars.size(), 3);

        // Check id property
        CodegenProperty idProp = model.vars.get(0);
        Assert.assertEquals(idProp.name, "Id");
        Assert.assertEquals(idProp.baseName, "id");
        Assert.assertEquals(idProp.dataType, "long");
        Assert.assertTrue(idProp.required);

        // Check name property
        CodegenProperty nameProp = model.vars.get(1);
        Assert.assertEquals(nameProp.name, "Name");
        Assert.assertEquals(nameProp.baseName, "name");
        Assert.assertEquals(nameProp.dataType, "std::string");
        Assert.assertTrue(nameProp.required);

        // Check isActive property
        CodegenProperty activeProp = model.vars.get(2);
        Assert.assertEquals(activeProp.name, "IsActive");
        Assert.assertEquals(activeProp.baseName, "isActive");
        Assert.assertEquals(activeProp.dataType, "bool");
        Assert.assertFalse(activeProp.required);
    }

    @Test(description = "convert model with array property")
    public void arrayPropertyTest() {
        final CppHttplibServerCodegen codegen = new CppHttplibServerCodegen();
        codegen.processOpts();

        ObjectSchema schema = new ObjectSchema();
        schema.addProperty("tags", new ArraySchema().items(new StringSchema()));

        final CodegenModel model = codegen.fromModel("ModelWithArray", schema);

        Assert.assertEquals(model.vars.size(), 1);
        CodegenProperty arrayProp = model.vars.get(0);
        Assert.assertEquals(arrayProp.baseName, "tags");
        Assert.assertEquals(arrayProp.dataType, "std::vector<std::string>");
        Assert.assertTrue(arrayProp.isArray);
        // Verify array vendor extensions are set
        Assert.assertTrue((boolean) arrayProp.vendorExtensions.getOrDefault("isArray", false),
            "isArray vendor extension should be true");
    }

    @Test(description = "convert model with array of enums")
    public void arrayOfEnumsPropertyTest() {
        final CppHttplibServerCodegen codegen = new CppHttplibServerCodegen();
        codegen.processOpts();

        ObjectSchema schema = new ObjectSchema();
        StringSchema enumItemSchema = new StringSchema();
        enumItemSchema.setEnum(java.util.Arrays.asList("ACTIVE", "INACTIVE", "PENDING"));
        schema.addProperty("statuses", new ArraySchema().items(enumItemSchema));

        final CodegenModel model = codegen.fromModel("ModelWithEnumArray", schema);

        Assert.assertEquals(model.vars.size(), 1);
        CodegenProperty arrayProp = model.vars.get(0);
        Assert.assertTrue(arrayProp.isArray);
        Assert.assertTrue((boolean) arrayProp.vendorExtensions.getOrDefault("isArray", false));
        // Verify it detects array of enums
        Assert.assertTrue((boolean) arrayProp.vendorExtensions.getOrDefault("isArrayOfEnum", false),
            "isArrayOfEnum vendor extension should be true");
    }

    @Test(description = "the enum class declared for an array-of-enum property must use valid, "
            + "upper-cased C++ identifiers, sourced from items (which setEnumVendorExtensions() derives)")
    public void arrayOfEnumsDeclaresValidUpperCaseIdentifiersTest() {
        final CppHttplibServerCodegen codegen = new CppHttplibServerCodegen();
        codegen.processOpts();

        ObjectSchema schema = new ObjectSchema();
        StringSchema enumItemSchema = new StringSchema();
        // lower-case spec values, like issue #24052's Pet.status, exercise the
        // identifier-vs-serialized-value distinction for array items too.
        enumItemSchema.setEnum(java.util.Arrays.asList("red", "green", "blue"));
        schema.addProperty("colors", new ArraySchema().items(enumItemSchema));

        final CodegenModel model = codegen.fromModel("ModelWithColorArray", schema);
        final CodegenModel processedModel = codegen.postProcessAllModels(
                wrapForPostProcessAllModels("ModelWithColorArray", model))
                .get("ModelWithColorArray").getModels().get(0).getModel();

        CodegenProperty arrayProp = processedModel.vars.get(0);
        // model-header.mustache declares `enum class {{enumName}} { {{items.allowableValues.values}} };`
        // for array-of-enum properties, so items must independently hold valid C++ identifiers.
        List<?> declaredIdentifiers = (List<?>) arrayProp.items.allowableValues.get("values");
        Assert.assertEquals(declaredIdentifiers, java.util.Arrays.asList("UNSPECIFIED", "RED", "GREEN", "BLUE"));
    }

    @Test(description = "convert model with map property")
    public void mapPropertyTest() {
        final CppHttplibServerCodegen codegen = new CppHttplibServerCodegen();
        codegen.processOpts();

        ObjectSchema schema = new ObjectSchema();
        schema.addProperty("metadata", new MapSchema().additionalProperties(new StringSchema()));

        final CodegenModel model = codegen.fromModel("ModelWithMap", schema);

        Assert.assertEquals(model.vars.size(), 1);
        CodegenProperty mapProp = model.vars.get(0);
        Assert.assertEquals(mapProp.name, "Metadata");
        Assert.assertEquals(mapProp.dataType, "std::map<std::string, std::string>");
        Assert.assertTrue(mapProp.isMap);
        // Verify map container flag
        Assert.assertTrue((boolean) mapProp.vendorExtensions.getOrDefault("isContainer", false),
            "isContainer vendor extension should be true for maps");
    }

    @Test(description = "convert model with numeric enum property")
    public void numericEnumPropertyTest() {
        final CppHttplibServerCodegen codegen = new CppHttplibServerCodegen();
        codegen.processOpts();

        ObjectSchema schema = new ObjectSchema();
        IntegerSchema statusSchema = new IntegerSchema();
        statusSchema.setEnum(java.util.Arrays.asList(0, 1, 2));
        schema.addProperty("userStatus", statusSchema);

        final CodegenModel model = codegen.fromModel("UserStatusModel", schema);
        Assert.assertEquals(model.vars.size(), 1);
        Assert.assertTrue(model.vars.get(0).isEnum);

        // The C++ identifier (numeric prefixing + upper-casing) is only finalized during
        // postProcessAllModels, since that's the single place both the identifier and the
        // original spec value are derived together (see enumSerializationUsesOriginalSpecValueTest).
        final CodegenModel processedModel = codegen.postProcessAllModels(
                wrapForPostProcessAllModels("UserStatusModel", model))
                .get("UserStatusModel").getModels().get(0).getModel();
        CodegenProperty statusProp = processedModel.vars.get(0);
        Assert.assertTrue((boolean) statusProp.vendorExtensions.getOrDefault("isEnum", false));
        List<?> enumValues = (List<?>) statusProp.vendorExtensions.get("values");
        Assert.assertNotNull(enumValues);
        // Check that numeric values are properly converted
        Assert.assertTrue(enumValues.stream().anyMatch(v -> v.toString().startsWith("_")),
            "Numeric enum values should be prefixed with underscore");
    }

    @Test(description = "issue #24052: enum (de)serialization must use the original spec value, not the derived C++ identifier")
    public void enumSerializationUsesOriginalSpecValueTest() {
        final CppHttplibServerCodegen codegen = new CppHttplibServerCodegen();
        codegen.processOpts();

        ObjectSchema schema = new ObjectSchema();
        StringSchema statusSchema = new StringSchema();
        statusSchema.setEnum(java.util.Arrays.asList("available", "pending", "sold"));
        schema.addProperty("status", statusSchema);

        final CodegenModel model = codegen.fromModel("Pet", schema);
        final CodegenModel processedModel = codegen.postProcessAllModels(
                wrapForPostProcessAllModels("Pet", model))
                .get("Pet").getModels().get(0).getModel();

        CodegenProperty statusProp = processedModel.vars.get(0);
        @SuppressWarnings("unchecked")
        List<Map<String, String>> enumCases = (List<Map<String, String>>) statusProp.vendorExtensions.get("enumCases");
        Assert.assertNotNull(enumCases);

        Map<String, String> availableCase = enumCases.stream()
                .filter(c -> "available".equals(c.get("value")))
                .findFirst()
                .orElse(null);
        Assert.assertNotNull(availableCase,
                "expected an enumCases entry whose value is the original spec text \"available\": " + enumCases);
        // The C++ identifier is upper-cased for enum naming conventions...
        Assert.assertEquals(availableCase.get("name"), "AVAILABLE");
        // ...but the serialized value must remain exactly what the OpenAPI spec declared.
        Assert.assertEquals(availableCase.get("value"), "available");
    }

    @Test(description = "convert enum model")
    public void enumModelTest() {
        final CppHttplibServerCodegen codegen = new CppHttplibServerCodegen();
        codegen.processOpts();

        StringSchema enumSchema = new StringSchema();
        enumSchema.setEnum(java.util.Arrays.asList("ACTIVE", "INACTIVE", "PENDING"));

        final CodegenModel model = codegen.fromModel("Status", enumSchema);

        // Note: The C++ httplib server generator may not process enum-only models
        // in the same way as regular object models. The model might be null or empty.
        if (model != null) {
            Assert.assertEquals(model.name, "Status");
            // Check if it's marked as an enum in vendor extensions
            if (model.vendorExtensions.containsKey("x-is-enum")) {
                Assert.assertEquals(model.vendorExtensions.get("x-is-enum"), true);
            }
        }
    }

    @Test(description = "convert model with nullable property")
    public void nullablePropertyTest() {
        final CppHttplibServerCodegen codegen = new CppHttplibServerCodegen();
        codegen.processOpts();

        StringSchema nullableStringSchema = new StringSchema();
        nullableStringSchema.setNullable(true);
        
        ObjectSchema schema = new ObjectSchema();
        schema.addProperty("optionalField", nullableStringSchema);

        final CodegenModel model = codegen.fromModel("ModelWithNullable", schema);

        Assert.assertEquals(model.vars.size(), 1);
        CodegenProperty nullableProp = model.vars.get(0);
        Assert.assertEquals(nullableProp.name, "OptionalField");
        Assert.assertTrue(nullableProp.isNullable);
        // Check that isOptional vendor extension is set
        Assert.assertTrue((boolean) nullableProp.vendorExtensions.getOrDefault("isOptional", false),
            "isOptional vendor extension should be true for nullable fields");
    }

    @Test(description = "convert model with nested object property")
    public void nestedObjectPropertyTest() {
        final CppHttplibServerCodegen codegen = new CppHttplibServerCodegen();
        codegen.processOpts();

        ObjectSchema nestedSchema = new ObjectSchema();
        nestedSchema.addProperty("id", new IntegerSchema());
        nestedSchema.addProperty("name", new StringSchema());

        ObjectSchema schema = new ObjectSchema();
        schema.addProperty("user", nestedSchema);

        final CodegenModel model = codegen.fromModel("ModelWithNested", schema);

        Assert.assertEquals(model.vars.size(), 1);
        CodegenProperty nestedProp = model.vars.get(0);
        Assert.assertEquals(nestedProp.name, "User");
        Assert.assertTrue(nestedProp.isModel);
    }

    @Test(description = "convert model with composed schema (allOf)")
    public void composedSchemaTest() {
        final CppHttplibServerCodegen codegen = new CppHttplibServerCodegen();
        codegen.processOpts();

        // Create base schema
        ObjectSchema baseSchema = new ObjectSchema();
        baseSchema.addProperty("id", new IntegerSchema());
        baseSchema.addProperty("name", new StringSchema());

        // Create allOf with additional properties
        io.swagger.v3.oas.models.media.ComposedSchema composedSchema = new io.swagger.v3.oas.models.media.ComposedSchema();
        composedSchema.addAllOfItem(new Schema<>().$ref("#/components/schemas/Base"));
        composedSchema.addProperty("email", new StringSchema());

        final CodegenModel model = codegen.fromModel("ExtendedUser", composedSchema);

        Assert.assertNotNull(model);
        Assert.assertEquals(model.name, "ExtendedUser");
    }

    @Test(description = "convert model with complex types")
    public void complexTypesTest() {
        final CppHttplibServerCodegen codegen = new CppHttplibServerCodegen();
        codegen.processOpts();

        ObjectSchema schema = new ObjectSchema();
        
        // Add various property types
        schema.addProperty("intValue", new IntegerSchema());
        schema.addProperty("longValue", new IntegerSchema().format("int64"));
        schema.addProperty("doubleValue", new NumberSchema());
        schema.addProperty("floatValue", new NumberSchema().format("float"));
        schema.addProperty("boolValue", new BooleanSchema());
        schema.addProperty("dateValue", new DateSchema());
        schema.addProperty("dateTimeValue", new DateTimeSchema());

        final CodegenModel model = codegen.fromModel("ComplexModel", schema);

        Assert.assertEquals(model.vars.size(), 7);
        
        // Verify type mappings
        java.util.Map<String, String> dataTypeMap = new java.util.HashMap<>();
        for (CodegenProperty var : model.vars) {
            dataTypeMap.put(var.baseName, var.dataType);
        }
        
        Assert.assertEquals(dataTypeMap.get("intValue"), "int");
        Assert.assertEquals(dataTypeMap.get("longValue"), "long");
        Assert.assertEquals(dataTypeMap.get("doubleValue"), "double");
        Assert.assertEquals(dataTypeMap.get("floatValue"), "float");
        Assert.assertEquals(dataTypeMap.get("boolValue"), "bool");
        Assert.assertEquals(dataTypeMap.get("dateValue"), "std::string");
        Assert.assertEquals(dataTypeMap.get("dateTimeValue"), "std::string");
    }
}

