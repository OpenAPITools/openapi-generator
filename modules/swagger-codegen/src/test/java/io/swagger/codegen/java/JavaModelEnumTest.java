package io.swagger.codegen.java;

import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.languages.JavaClientCodegen;
import io.swagger.models.ComposedModel;
import io.swagger.models.Model;
import io.swagger.models.ModelImpl;
import io.swagger.models.RefModel;
import io.swagger.models.properties.Property;
import io.swagger.models.properties.StringProperty;
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
        final StringProperty enumProperty = new StringProperty();
        enumProperty.setEnum(Arrays.asList("VALUE1", "VALUE2", "VALUE3"));
        final ModelImpl model = new ModelImpl().property("name", enumProperty);

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
    public void overrideEnumTest() {
        final StringProperty identicalEnumProperty = new StringProperty();
        identicalEnumProperty.setEnum(Arrays.asList("VALUE1", "VALUE2", "VALUE3"));

        final StringProperty subEnumProperty = new StringProperty();
        subEnumProperty.setEnum(Arrays.asList("SUB1", "SUB2", "SUB3"));

        // Add one enum property to the parent
        final Map<String, Property> parentProperties = new HashMap<String, Property>();
        parentProperties.put("sharedThing", identicalEnumProperty);

        // Add TWO enums to the subType model; one of which is identical to the one in parent class
        final Map<String, Property> subProperties = new HashMap<String, Property>();
        subProperties.put("sharedThing", identicalEnumProperty);
        subProperties.put("unsharedThing", identicalEnumProperty);

        final ModelImpl parentModel = new ModelImpl();
        parentModel.setProperties(parentProperties);
        parentModel.name("parentModel");

        final ModelImpl subModel = new ModelImpl();
        subModel.setProperties(subProperties);
        subModel.name("subModel");

        final ComposedModel model = new ComposedModel()
                .parent(new RefModel(parentModel.getName()))
                .child(subModel)
                .interfaces(new ArrayList<RefModel>());

        final DefaultCodegen codegen = new JavaClientCodegen();
        final Map<String, Model> allModels = new HashMap<String, Model>();
        allModels.put(parentModel.getName(), parentModel);
        allModels.put(subModel.getName(), subModel);

        final CodegenModel cm = codegen.fromModel("sample", model, allModels);

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
