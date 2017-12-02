package io.swagger.codegen.eiffel;
import io.swagger.codegen.CodegenConstants;
import io.swagger.oas.models.media.ArraySchema;
import io.swagger.oas.models.media.DateTimeSchema;
import io.swagger.oas.models.media.IntegerSchema;
import io.swagger.oas.models.media.MapSchema;
import io.swagger.oas.models.media.Schema;
import io.swagger.oas.models.media.StringSchema;
import io.swagger.parser.v3.util.SchemaTypeUtil;
import org.testng.Assert;
import org.testng.annotations.Test;

import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.languages.EiffelClientCodegen;

import static io.swagger.codegen.languages.helpers.ExtensionHelper.getBooleanValue;

@SuppressWarnings("static-method")
public class EiffelModelTest {

    @Test(description = "convert a simple Eiffel model")
    public void simpleModelTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("id", new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperties("name", new StringSchema())
                .addProperties("createdAt", new DateTimeSchema())
                .addRequiredItem("id")
                .addRequiredItem("name");
        final DefaultCodegen codegen = new EiffelClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema);

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
        Assert.assertTrue(getBooleanValue(property1, CodegenConstants.HAS_MORE_EXT_NAME));
        Assert.assertTrue(property1.required);
        Assert.assertTrue(getBooleanValue(property1, CodegenConstants.IS_PRIMITIVE_TYPE_EXT_NAME));
        Assert.assertTrue(getBooleanValue(property1, CodegenConstants.IS_NOT_CONTAINER_EXT_NAME));

        final CodegenProperty property2 = cm.vars.get(1);
        Assert.assertEquals(property2.baseName, "name");
        Assert.assertEquals(property2.datatype, "STRING_32");
        Assert.assertEquals(property2.name, "name");
        Assert.assertEquals(property1.defaultValue, "null");
        Assert.assertEquals(property2.baseType, "STRING_32");
        Assert.assertTrue(getBooleanValue(property2, CodegenConstants.HAS_MORE_EXT_NAME));
        Assert.assertTrue(property2.required);
        Assert.assertTrue(getBooleanValue(property2, CodegenConstants.IS_PRIMITIVE_TYPE_EXT_NAME));
        Assert.assertTrue(getBooleanValue(property2, CodegenConstants.IS_NOT_CONTAINER_EXT_NAME));

        final CodegenProperty property3 = cm.vars.get(2);
        Assert.assertEquals(property3.baseName, "createdAt");
        Assert.assertEquals(property3.complexType, "DATE_TIME");
        Assert.assertEquals(property3.datatype, "DATE_TIME");
        Assert.assertEquals(property3.name, "created_at");
        Assert.assertEquals(property1.defaultValue, "null");
        Assert.assertEquals(property3.baseType, "DATE_TIME");
        Assert.assertFalse(getBooleanValue(property3, CodegenConstants.HAS_MORE_EXT_NAME));
        Assert.assertFalse(property3.required);
        Assert.assertTrue(getBooleanValue(property3, CodegenConstants.IS_NOT_CONTAINER_EXT_NAME));
    }
    
    @Test(description = "convert a model with list property")
    public void listPropertyTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("id", new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperties("urls", new ArraySchema()
                        .items(new StringSchema()))
                .addRequiredItem("id");
        final DefaultCodegen codegen = new EiffelClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema);

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
        Assert.assertTrue(getBooleanValue(property1, CodegenConstants.HAS_MORE_EXT_NAME));
        Assert.assertTrue(property1.required);
        Assert.assertTrue(getBooleanValue(property1, CodegenConstants.IS_PRIMITIVE_TYPE_EXT_NAME));
        Assert.assertTrue(getBooleanValue(property1, CodegenConstants.IS_NOT_CONTAINER_EXT_NAME));

        final CodegenProperty property2 = cm.vars.get(1);
        Assert.assertEquals(property2.baseName, "urls");
        Assert.assertEquals(property2.datatype, "LIST [STRING_32]");
        Assert.assertEquals(property2.name, "urls");
        Assert.assertEquals(property2.baseType, "LIST");
        Assert.assertFalse(getBooleanValue(property2, CodegenConstants.HAS_MORE_EXT_NAME));
        Assert.assertEquals(property2.containerType, "array");
        Assert.assertFalse(property2.required);
        Assert.assertFalse(getBooleanValue(property2, CodegenConstants.IS_PRIMITIVE_TYPE_EXT_NAME));
        Assert.assertTrue(getBooleanValue(property2, CodegenConstants.IS_CONTAINER_EXT_NAME));
    }


    
    @Test(description = "convert a model with a map property")
    public void mapPropertyTest() {
        final Schema schema = getMapSchema();
        final DefaultCodegen codegen = new EiffelClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema);

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
        Assert.assertTrue(getBooleanValue(property1, CodegenConstants.IS_CONTAINER_EXT_NAME));
        Assert.assertFalse(getBooleanValue(property1, CodegenConstants.IS_PRIMITIVE_TYPE_EXT_NAME));
    }
    
    private Schema getMapSchema() {
        return new Schema()
                .description("a sample model")
                .addProperties("mapping", new MapSchema()
                        .additionalProperties(new StringSchema()))
                .addRequiredItem("id");
    }

}
