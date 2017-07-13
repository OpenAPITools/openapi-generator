package io.swagger.codegen.eiffel;
import org.testng.Assert;
import org.testng.annotations.Test;

import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.languages.EiffelClientCodegen;
import io.swagger.codegen.languages.KotlinClientCodegen;
import io.swagger.models.Model;
import io.swagger.models.ModelImpl;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.DateTimeProperty;
import io.swagger.models.properties.LongProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.StringProperty;

@SuppressWarnings("static-method")
public class EiffelModelTest {

    @Test(description = "convert a simple Eiffel model")
    public void simpleModelTest() {
        final Model model = new ModelImpl()
                .description("a sample model")
                .property("id", new LongProperty())
                .property("name", new StringProperty())
                .property("createdAt", new DateTimeProperty())
                .required("id")
                .required("name");
        final DefaultCodegen codegen = new EiffelClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "SAMPLE");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 3);
        Assert.assertEquals(cm.imports.size(), 2);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "id");
        Assert.assertEquals(property1.datatype, "INTEGER_64");
        Assert.assertEquals(property1.name, "id");
        Assert.assertEquals(property1.defaultValue, "null");
        Assert.assertEquals(property1.baseType, "INTEGER_64");
        Assert.assertTrue(property1.hasMore);
        Assert.assertTrue(property1.required);
        Assert.assertTrue(property1.isPrimitiveType);
        Assert.assertTrue(property1.isNotContainer);

        final CodegenProperty property2 = cm.vars.get(1);
        Assert.assertEquals(property2.baseName, "name");
        Assert.assertEquals(property2.datatype, "STRING_32");
        Assert.assertEquals(property2.name, "name");
        Assert.assertEquals(property1.defaultValue, "null");
        Assert.assertEquals(property2.baseType, "STRING_32");
        Assert.assertTrue(property2.hasMore);
        Assert.assertTrue(property2.required);
        Assert.assertFalse(property2.isPrimitiveType);
        Assert.assertTrue(property2.isNotContainer);

        final CodegenProperty property3 = cm.vars.get(2);
        Assert.assertEquals(property3.baseName, "createdAt");
        Assert.assertEquals(property3.complexType, "DATE_TIME");
        Assert.assertEquals(property3.datatype, "DATE_TIME");
        Assert.assertEquals(property3.name, "created_at");
        Assert.assertEquals(property1.defaultValue, "null");
        Assert.assertEquals(property3.baseType, "DATE_TIME");
        Assert.assertFalse(property3.hasMore);
        Assert.assertFalse(property3.required);
        Assert.assertTrue(property3.isNotContainer);
    }
    
    @Test(description = "convert a model with list property")
    public void listPropertyTest() {
        final Model model = new ModelImpl()
                .description("a sample model")
                .property("id", new LongProperty())
                .property("urls", new ArrayProperty()
                        .items(new StringProperty()))
                .required("id");
        final DefaultCodegen codegen = new EiffelClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "SAMPLE");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 2);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "id");
        Assert.assertEquals(property1.datatype, "INTEGER_64");
        Assert.assertEquals(property1.name, "id");
        Assert.assertEquals(property1.defaultValue, "null");
        Assert.assertEquals(property1.baseType, "INTEGER_64");
        Assert.assertTrue(property1.hasMore);
        Assert.assertTrue(property1.required);
        Assert.assertTrue(property1.isPrimitiveType);
        Assert.assertTrue(property1.isNotContainer);

        final CodegenProperty property2 = cm.vars.get(1);
        Assert.assertEquals(property2.baseName, "urls");
        Assert.assertEquals(property2.datatype, "LIST [STRING_32]");
        Assert.assertEquals(property2.name, "urls");
        Assert.assertEquals(property2.baseType, "LIST");
        Assert.assertFalse(property2.hasMore);
        Assert.assertEquals(property2.containerType, "array");
        Assert.assertFalse(property2.required);
        Assert.assertFalse(property2.isPrimitiveType);
        Assert.assertTrue(property2.isContainer);
    }


    
    @Test(description = "convert a model with a map property")
    public void mapPropertyTest() {
        final Model model = getMapModel();
        final DefaultCodegen codegen = new EiffelClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "SAMPLE");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "mapping");
        Assert.assertEquals(property1.datatype, "STRING_TABLE[STRING_32]");
        Assert.assertEquals(property1.name, "mapping");
        Assert.assertEquals(property1.baseType, "STRING_TABLE");
        Assert.assertEquals(property1.containerType, "map");
        Assert.assertFalse(property1.required);
        Assert.assertTrue(property1.isContainer);
        Assert.assertFalse(property1.isPrimitiveType);
    }
    
    private Model getMapModel() {
        return new ModelImpl()
                .description("a sample model")
                .property("mapping", new MapProperty()
                        .additionalProperties(new StringProperty()))
                .required("id");
    }

}
