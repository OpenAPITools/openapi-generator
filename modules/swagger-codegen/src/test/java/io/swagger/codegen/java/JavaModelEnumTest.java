package io.swagger.codegen.java;

import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.languages.JavaClientCodegen;
import io.swagger.oas.models.media.ComposedSchema;
import io.swagger.oas.models.media.Schema;
import io.swagger.oas.models.media.StringSchema;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

@SuppressWarnings("static-method")
public class JavaModelEnumTest {

    @Test(description = "convert a java model with an enum")
    public void converterTest() {
        final StringSchema enumSchema = new StringSchema();
        enumSchema.setEnum(Arrays.asList("VALUE1", "VALUE2", "VALUE3"));
        final Schema model = new Schema().addProperties("name", enumSchema);

        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty enumVar = cm.vars.get(0);
        Assert.assertEquals(enumVar.baseName, "name");
        Assert.assertEquals(enumVar.datatype, "String");
        Assert.assertEquals(enumVar.datatypeWithEnum, "NameEnum");
        Assert.assertEquals(enumVar.name, "name");
        Assert.assertEquals(enumVar.defaultValue, "null");
        Assert.assertEquals(enumVar.baseType, "String");
        Assert.assertTrue(enumVar.isEnum);
    }

    @Test(description = "not override identical parent enums")
    // TODO verify inheritance with new oas3 structure
    public void overrideEnumTest() {
        final StringSchema identicalEnumProperty = new StringSchema();
        identicalEnumProperty.setEnum(Arrays.asList("VALUE1", "VALUE2", "VALUE3"));

        final StringSchema subEnumProperty = new StringSchema();
        subEnumProperty.setEnum(Arrays.asList("SUB1", "SUB2", "SUB3"));

        // Add one enum property to the parent
        final Map<String, Schema> parentProperties = new HashMap<>();
        parentProperties.put("sharedThing", identicalEnumProperty);

        // Add TWO enums to the subType model; one of which is identical to the one in parent class
        final Map<String, Schema> subProperties = new HashMap<>();
        subProperties.put("sharedThing", identicalEnumProperty);
        subProperties.put("unsharedThing", identicalEnumProperty);

        final Schema parentModel = new Schema();
        parentModel.setProperties(parentProperties);
        parentModel.name("parentModel");

        final Schema subModel = new Schema();
        subModel.setProperties(subProperties);
        subModel.name("subModel");

        final ComposedSchema composedSchema = new ComposedSchema()
                .addAllOfItem(new Schema().$ref(parentModel.getName()))
                .addAllOfItem(subModel);

        final DefaultCodegen codegen = new JavaClientCodegen();
        final Map<String, Schema> allModels = new HashMap<String, Schema>();
        allModels.put(parentModel.getName(), parentModel);
        allModels.put(subModel.getName(), subModel);

        final CodegenModel cm = codegen.fromModel("sample", composedSchema, allModels);

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
}
