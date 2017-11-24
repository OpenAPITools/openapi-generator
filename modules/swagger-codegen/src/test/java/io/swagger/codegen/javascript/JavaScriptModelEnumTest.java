package io.swagger.codegen.javascript;

import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.languages.JavascriptClientCodegen;
import io.swagger.oas.models.OpenAPI;
import io.swagger.oas.models.media.ComposedSchema;
import io.swagger.oas.models.media.Schema;
import io.swagger.oas.models.media.StringSchema;
import io.swagger.parser.v3.OpenAPIV3Parser;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import static io.swagger.codegen.utils.ModelUtils.updateCodegenPropertyEnum;

@SuppressWarnings("static-method")
public class JavaScriptModelEnumTest {
    @Test(description = "convert a JavaScript model with an enum")
    public void converterTest() {
        final StringSchema enumProperty = new StringSchema();
        enumProperty.setEnum(Arrays.asList("VALUE1", "VALUE2", "VALUE3"));
        final Schema schema = new Schema().addProperties("name", enumProperty);

        final DefaultCodegen codegen = new JavascriptClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty enumVar = cm.vars.get(0);
        Assert.assertEquals(enumVar.baseName, "name");
        Assert.assertEquals(enumVar.datatype, "String");
        Assert.assertEquals(enumVar.datatypeWithEnum, "NameEnum");
        Assert.assertEquals(enumVar.name, "name");
        Assert.assertEquals(enumVar.defaultValue, null);
        Assert.assertEquals(enumVar.baseType, "String");
        Assert.assertTrue(enumVar.isEnum);
    }

    @Test(description = "not override identical parent enums")
    public void overrideEnumTest() {
        final StringSchema identicalEnumProperty = new StringSchema();
        identicalEnumProperty.setEnum(Arrays.asList("VALUE1", "VALUE2", "VALUE3"));

        final StringSchema subEnumProperty = new StringSchema();
        subEnumProperty.setEnum(Arrays.asList("SUB1", "SUB2", "SUB3"));

        // Add one enum property to the parent
        final Map<String, Schema> parentProperties = new HashMap<String, Schema>();
        parentProperties.put("sharedThing", identicalEnumProperty);

        // Add TWO enums to the subType model; one of which is identical to the one in parent class
        final Map<String, Schema> subProperties = new HashMap<String, Schema>();
        subProperties.put("sharedThing", identicalEnumProperty);
        subProperties.put("unsharedThing", identicalEnumProperty);

        final Schema parentModel = new Schema();
        parentModel.setProperties(parentProperties);
        parentModel.name("parentModel");

        final Schema subModel = new Schema();
        subModel.setProperties(subProperties);
        subModel.name("subModel");

        final ComposedSchema composedSchema = new ComposedSchema();

        final DefaultCodegen codegen = new JavascriptClientCodegen();
        final Map<String, Schema> allSchemas = new HashMap<String, Schema>();
        allSchemas.put(parentModel.getName(), parentModel);
        allSchemas.put(subModel.getName(), subModel);

        final CodegenModel cm = codegen.fromModel("sample", composedSchema, allSchemas);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.parent, "ParentModel");
        Assert.assertTrue(cm.imports.contains("ParentModel"));

        // Assert that only the unshared/uninherited enum remains
        Assert.assertEquals(cm.vars.size(), 1);
        final CodegenProperty enumVar = cm.vars.get(0);
        Assert.assertEquals(enumVar.baseName, "unsharedThing");
        Assert.assertEquals(enumVar.datatype, "String");
        Assert.assertEquals(enumVar.datatypeWithEnum, "UnsharedThingEnum");
        Assert.assertTrue(enumVar.isEnum);
    }

    @Test(description = "test enum array model")
    public void enumArrayModelTest() {
        // TODO: update yaml spec
        final OpenAPI openAPI =  new OpenAPIV3Parser().read("src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml");
        final DefaultCodegen codegen = new JavascriptClientCodegen();
        final Schema schema = openAPI.getComponents().getSchemas().get("EnumArrays");

        Map<String, Schema> schemas = schema.getProperties();
        Schema property =  schemas.get("array_enum");
        CodegenProperty prope = codegen.fromProperty("array_enum", property);
        updateCodegenPropertyEnum(prope);
        Assert.assertEquals(prope.datatypeWithEnum, "[ArrayEnumEnum]");
        Assert.assertEquals(prope.enumName, "ArrayEnumEnum");
        Assert.assertTrue(prope.isEnum);
        Assert.assertEquals(prope.allowableValues.get("values"), Arrays.asList("fish", "crab"));

        HashMap<String, String> fish= new HashMap<String, String>();
        fish.put("name", "fish");
        fish.put("value", "\"fish\"");
        HashMap<String, String> crab= new HashMap<String, String>();
        crab.put("name", "crab");
        crab.put("value", "\"crab\"");
        Assert.assertEquals(prope.allowableValues.get("enumVars"), Arrays.asList(fish, crab));

        // assert inner items
        Assert.assertEquals(prope.datatypeWithEnum, "[ArrayEnumEnum]");
        Assert.assertEquals(prope.enumName, "ArrayEnumEnum");
        Assert.assertTrue(prope.items.isEnum);
        Assert.assertEquals(prope.items.allowableValues.get("values"), Arrays.asList("fish", "crab"));
        Assert.assertEquals(prope.items.allowableValues.get("enumVars"), Arrays.asList(fish, crab));

    }

    @Test(description = "test enum model for values (numeric, string, etc)")
    public void enumModelValueTest() {
        // TODO: update yaml spec
        final OpenAPI openAPI =  new OpenAPIV3Parser().read("src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml");
        final DefaultCodegen codegen = new JavascriptClientCodegen();
        final Schema schema = openAPI.getComponents().getSchemas().get("EnumArrays");

        Map<String, Schema> schemas = schema.getProperties();
        Schema property =  schemas.get("enum_integer");
        CodegenProperty prope = codegen.fromProperty("enum_integer", property);
        updateCodegenPropertyEnum(prope);
        Assert.assertEquals(prope.datatypeWithEnum, "EnumIntegerEnum");
        Assert.assertEquals(prope.enumName, "EnumIntegerEnum");
        Assert.assertTrue(prope.isEnum);
        Assert.assertFalse(prope.isContainer);
        Assert.assertNull(prope.items);
        Assert.assertEquals(prope.allowableValues.get("values"), Arrays.asList(1, -1));

        HashMap<String, String> one = new HashMap<String, String>();
        one.put("name", "1");
        one.put("value", "1");
        HashMap<String, String> minusOne = new HashMap<String, String>();
        minusOne.put("name", "-1");
        minusOne.put("value", "-1");
        Assert.assertEquals(prope.allowableValues.get("enumVars"), Arrays.asList(one, minusOne));

    }
}
