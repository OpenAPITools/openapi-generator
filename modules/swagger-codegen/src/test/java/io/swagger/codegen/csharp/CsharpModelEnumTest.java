package io.swagger.codegen.csharp;


import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.languages.CSharpClientCodegen;
import io.swagger.oas.models.media.Schema;
import io.swagger.oas.models.media.StringSchema;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import static io.swagger.codegen.CodegenModel.IS_ENUM_EXT_NAME;
import static io.swagger.codegen.languages.helpers.ExtensionHelper.getBooleanValue;

public class CsharpModelEnumTest {
    @Test(description = "not override identical parent enums")
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

        final Schema parentModel = new Schema().description("parentModel");
        parentModel.setProperties(parentProperties);
        parentModel.name("parentModel");

        final Schema subModel = new Schema()
                .description("subModel");
        subModel.setProperties(subProperties);
        subModel.name("subModel");

        // TODO resolve compose schema
        /**
        final ComposedModel model = new ComposedModel()
                .parent(new RefModel(parentModel.getName()))
                .child(subModel)
                .interfaces(new ArrayList<RefModel>());
*/
        final DefaultCodegen codegen = new CSharpClientCodegen();
        final Map<String, Schema> allSchemas = new HashMap<>();
        allSchemas.put("ParentModel", parentModel);
        allSchemas.put("SubModel", subModel);

        final CodegenModel cm = codegen.fromModel("sample", /**model*/ null, allSchemas);

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
        Assert.assertTrue(getBooleanValue(enumVar.getVendorExtensions(), IS_ENUM_EXT_NAME));
    }
}
