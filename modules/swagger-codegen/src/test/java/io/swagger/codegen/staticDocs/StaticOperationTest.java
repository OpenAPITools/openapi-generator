package io.swagger.codegen.staticDocs;

import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.languages.StaticDocCodegen;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.RefProperty;
import io.swagger.models.properties.StringProperty;

import org.testng.Assert;
import org.testng.annotations.Test;

@SuppressWarnings("static-method")
public class StaticOperationTest {

    @Test(description = "convert a string parameter")
    public void stringParameterTest() {
        final StringProperty property = new StringProperty();
        final DefaultCodegen codegen = new StaticDocCodegen();
        final CodegenProperty cp = codegen.fromProperty("property", property);

        Assert.assertEquals(cp.baseName, "property");
        Assert.assertEquals(cp.datatype, "String");
        Assert.assertEquals(cp.name, "property");
        Assert.assertEquals(cp.baseType, "string");
        Assert.assertTrue(cp.isNotContainer);
    }

    @Test(description = "convert a complex parameter")
    public void complexParameterTest() {
        final RefProperty property = new RefProperty("Children");
        final DefaultCodegen codegen = new StaticDocCodegen();
        final CodegenProperty cp = codegen.fromProperty("property", property);

        Assert.assertEquals(cp.baseName, "property");
        Assert.assertEquals(cp.complexType, "Children");
        Assert.assertEquals(cp.getter, "getProperty");
        Assert.assertEquals(cp.setter, "setProperty");
        Assert.assertEquals(cp.datatype, "Children");
        Assert.assertEquals(cp.name, "property");
        Assert.assertEquals(cp.defaultValue, "null");
        Assert.assertEquals(cp.baseType, "Children");
        Assert.assertTrue(cp.isNotContainer);
    }

    @Test(description = "convert a complex list parameter")
    public void listParameterTest() {
        final ArrayProperty property = new ArrayProperty().items(new RefProperty("Children"));
        final DefaultCodegen codegen = new StaticDocCodegen();
        final CodegenProperty cp = codegen.fromProperty("property", property);

        Assert.assertEquals(cp.baseName, "property");
        Assert.assertEquals(cp.complexType, "Children");
        Assert.assertEquals(cp.getter, "getProperty");
        Assert.assertEquals(cp.setter, "setProperty");
        Assert.assertEquals(cp.datatype, "List");
        Assert.assertEquals(cp.name, "property");
        Assert.assertEquals(cp.baseType, "array");
        Assert.assertEquals(cp.containerType, "array");
        Assert.assertTrue(cp.isContainer);
    }
}
