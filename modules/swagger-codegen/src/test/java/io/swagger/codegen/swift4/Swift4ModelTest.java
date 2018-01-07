package io.swagger.codegen.swift4;

import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.languages.Swift4Codegen;
import io.swagger.models.Model;
import io.swagger.models.ModelImpl;
import io.swagger.models.properties.*;
import org.testng.Assert;
import org.testng.annotations.Test;

@SuppressWarnings("static-method")
public class Swift4ModelTest {

    @Test(description = "convert a simple java model")
    public void simpleModelTest() {
        final Model model = new ModelImpl()
                .description("a sample model")
                .property("id", new LongProperty())
                .property("name", new StringProperty())
                .property("createdAt", new DateTimeProperty())
                .property("binary", new BinaryProperty())
                .property("byte", new ByteArrayProperty())
                .property("uuid", new UUIDProperty())
                .property("dateOfBirth", new DateProperty())
                .required("id")
                .required("name")
                .discriminator("test");
        final DefaultCodegen codegen = new Swift4Codegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 7);
        Assert.assertEquals(cm.discriminator,"test");

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "id");
        Assert.assertEquals(property1.datatype, "Int64");
        Assert.assertEquals(property1.name, "_id");
        Assert.assertNull(property1.defaultValue);
        Assert.assertEquals(property1.baseType, "Int64");
        Assert.assertTrue(property1.hasMore);
        Assert.assertTrue(property1.required);
        Assert.assertTrue(property1.isPrimitiveType);
        Assert.assertTrue(property1.isNotContainer);

        final CodegenProperty property2 = cm.vars.get(1);
        Assert.assertEquals(property2.baseName, "name");
        Assert.assertEquals(property2.datatype, "String");
        Assert.assertEquals(property2.name, "name");
        Assert.assertNull(property2.defaultValue);
        Assert.assertEquals(property2.baseType, "String");
        Assert.assertTrue(property2.hasMore);
        Assert.assertTrue(property2.required);
        Assert.assertTrue(property2.isPrimitiveType);
        Assert.assertTrue(property2.isNotContainer);

        final CodegenProperty property3 = cm.vars.get(2);
        Assert.assertEquals(property3.baseName, "createdAt");
        Assert.assertEquals(property3.datatype, "Date");
        Assert.assertEquals(property3.name, "createdAt");
        Assert.assertNull(property3.defaultValue);
        Assert.assertEquals(property3.baseType, "Date");
        Assert.assertTrue(property3.hasMore);
        Assert.assertFalse(property3.required);
        Assert.assertTrue(property3.isNotContainer);

        final CodegenProperty property4 = cm.vars.get(3);
        Assert.assertEquals(property4.baseName, "binary");
        Assert.assertEquals(property4.datatype, "Data");
        Assert.assertEquals(property4.name, "binary");
        Assert.assertNull(property4.defaultValue);
        Assert.assertEquals(property4.baseType, "Data");
        Assert.assertTrue(property4.hasMore);
        Assert.assertFalse(property4.required);
        Assert.assertTrue(property4.isNotContainer);

        final CodegenProperty property5 = cm.vars.get(4);
        Assert.assertEquals(property5.baseName, "byte");
        Assert.assertEquals(property5.datatype, "Data");
        Assert.assertEquals(property5.name, "byte");
        Assert.assertNull(property5.defaultValue);
        Assert.assertEquals(property5.baseType, "Data");
        Assert.assertTrue(property5.hasMore);
        Assert.assertFalse(property5.required);
        Assert.assertTrue(property5.isNotContainer);

        final CodegenProperty property6 = cm.vars.get(5);
        Assert.assertEquals(property6.baseName, "uuid");
        Assert.assertEquals(property6.datatype, "UUID");
        Assert.assertEquals(property6.name, "uuid");
        Assert.assertNull(property6.defaultValue);
        Assert.assertEquals(property6.baseType, "UUID");
        Assert.assertTrue(property6.hasMore);
        Assert.assertFalse(property6.required);
        Assert.assertTrue(property6.isNotContainer);

        final CodegenProperty property7 = cm.vars.get(6);
        Assert.assertEquals(property7.baseName, "dateOfBirth");
        Assert.assertEquals(property7.datatype, "Date");
        Assert.assertEquals(property7.name, "dateOfBirth");
        Assert.assertNull(property7.defaultValue);
        Assert.assertEquals(property7.baseType, "Date");
        Assert.assertFalse(property7.hasMore);
        Assert.assertFalse(property7.required);
        Assert.assertTrue(property7.isNotContainer);
    }

}
