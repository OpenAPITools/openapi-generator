package io.swagger.codegen.csharp;


import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.languages.CSharpClientCodegen;
import io.swagger.models.ComposedModel;
import io.swagger.models.Model;
import io.swagger.models.ModelImpl;
import io.swagger.models.RefModel;
import io.swagger.models.properties.Property;
import io.swagger.models.properties.RefProperty;
import io.swagger.models.properties.StringProperty;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

public class CsharpModelEnumTest {
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

        final ModelImpl parentModel = new ModelImpl()
                .description("parentModel");
        parentModel.setProperties(parentProperties);
        parentModel.name("parentModel");

        final ModelImpl subModel = new ModelImpl()
                .description("subModel");
        subModel.setProperties(subProperties);
        subModel.name("subModel");

        final ComposedModel model = new ComposedModel()
                .parent(new RefModel(parentModel.getName()))
                .child(subModel)
                .interfaces(new ArrayList<RefModel>());

        final DefaultCodegen codegen = new CSharpClientCodegen();
        final Map<String, Model> allModels = new HashMap<>();
        allModels.put("ParentModel", parentModel);
        allModels.put("SubModel", subModel);

        final CodegenModel cm = codegen.fromModel("sample", model, allModels);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.parent, "ParentModel");
        Assert.assertTrue(cm.imports.contains("ParentModel"));

        // Assert that only the unshared/uninherited enum remains
        Assert.assertEquals(cm.vars.size(), 1);
        final CodegenProperty enumVar = cm.vars.get(0);
        Assert.assertEquals(enumVar.baseName, "unsharedThing");
        Assert.assertEquals(enumVar.datatype, "string");
        Assert.assertEquals(enumVar.datatypeWithEnum, "UnsharedThingEnum");
        Assert.assertTrue(enumVar.isEnum);
    }
}
