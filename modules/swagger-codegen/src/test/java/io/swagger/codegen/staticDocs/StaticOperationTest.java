package io.swagger.codegen.staticDocs;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.languages.StaticDocCodegen;

import io.swagger.oas.models.media.ArraySchema;
import io.swagger.oas.models.media.BooleanSchema;
import io.swagger.oas.models.media.Schema;
import io.swagger.oas.models.media.StringSchema;
import org.testng.Assert;
import org.testng.annotations.Test;

import static io.swagger.codegen.languages.helpers.ExtensionHelper.getBooleanValue;

@SuppressWarnings("static-method")
public class StaticOperationTest {

    @Test(description = "convert a string parameter")
    public void stringParameterTest() {
        final StringSchema property = new StringSchema();
        final DefaultCodegen codegen = new StaticDocCodegen();
        final CodegenProperty cp = codegen.fromProperty("property", property);

        Assert.assertEquals(cp.baseName, "property");
        Assert.assertEquals(cp.datatype, "String");
        Assert.assertEquals(cp.name, "property");
        Assert.assertEquals(cp.baseType, "string");
        Assert.assertTrue(getBooleanValue(cp.getVendorExtensions(), CodegenConstants.IS_NOT_CONTAINER_EXT_NAME));
    }

    @Test(description = "convert a boolean parameter")
    public void booleanParameterTest() {
        final BooleanSchema property = new BooleanSchema();
        final DefaultCodegen codegen = new StaticDocCodegen();
        final CodegenProperty cp = codegen.fromProperty("property", property);

        Assert.assertEquals(cp.baseName, "property");
        Assert.assertEquals(cp.datatype, "Boolean");
        Assert.assertEquals(cp.name, "property");
        Assert.assertEquals(cp.baseType, "boolean");
        Assert.assertTrue(getBooleanValue(cp.getVendorExtensions(), CodegenConstants.IS_NOT_CONTAINER_EXT_NAME));
        Assert.assertTrue(getBooleanValue(cp.getVendorExtensions(), CodegenConstants.IS_BOOLEAN_EXT_NAME));
        Assert.assertEquals(cp.getter, "getProperty");
    }

    @Test(description = "convert a complex parameter")
    public void complexParameterTest() {
        final Schema property = new Schema().$ref("Children");
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
        Assert.assertTrue(getBooleanValue(cp.getVendorExtensions(), CodegenConstants.IS_NOT_CONTAINER_EXT_NAME));
    }

    @Test(description = "convert a complex list parameter")
    public void listParameterTest() {
        final ArraySchema property = new ArraySchema().items(new Schema().$ref("Children"));
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
        Assert.assertTrue(getBooleanValue(cp.getVendorExtensions(), CodegenConstants.IS_CONTAINER_EXT_NAME));
    }
}
