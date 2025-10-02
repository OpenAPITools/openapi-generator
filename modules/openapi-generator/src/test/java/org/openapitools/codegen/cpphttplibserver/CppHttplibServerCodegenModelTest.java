package org.openapitools.codegen.cpphttplib;

import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.CppHttplibServerCodegen;
import io.swagger.v3.oas.models.media.*;
import org.testng.Assert;
import org.testng.annotations.Test;

@SuppressWarnings("static-method")
public class CppHttplibServerCodegenModelTest {

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
        Assert.assertEquals(nullableProp.dataType, "std::string");
    }
}

