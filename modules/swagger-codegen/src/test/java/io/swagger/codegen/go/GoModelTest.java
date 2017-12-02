package io.swagger.codegen.go;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.languages.GoClientCodegen;

import com.google.common.collect.Sets;
import io.swagger.oas.models.media.ArraySchema;
import io.swagger.oas.models.media.DateTimeSchema;
import io.swagger.oas.models.media.IntegerSchema;
import io.swagger.oas.models.media.MapSchema;
import io.swagger.oas.models.media.Schema;
import io.swagger.oas.models.media.StringSchema;
import io.swagger.parser.v3.util.SchemaTypeUtil;
import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import static io.swagger.codegen.languages.helpers.ExtensionHelper.getBooleanValue;

@SuppressWarnings("static-method")
public class GoModelTest {

    @Test(description = "convert a simple Go model")
    public void simpleModelTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("id", new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperties("name", new StringSchema())
                .addProperties("createdAt", new DateTimeSchema())
                .addRequiredItem("id")
                .addRequiredItem("name");
        final DefaultCodegen codegen = new GoClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 3);
        Assert.assertEquals(cm.imports.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "id");
        Assert.assertEquals(property1.datatype, "int64");
        Assert.assertEquals(property1.name, "Id");
        Assert.assertEquals(property1.defaultValue, "null");
        Assert.assertEquals(property1.baseType, "int64");
        Assert.assertTrue(getBooleanValue(property1, CodegenConstants.HAS_MORE_EXT_NAME));
        Assert.assertTrue(property1.required);
        Assert.assertTrue(getBooleanValue(property1, CodegenConstants.IS_PRIMITIVE_TYPE_EXT_NAME));
        Assert.assertTrue(getBooleanValue(property1, CodegenConstants.IS_NOT_CONTAINER_EXT_NAME));

        final CodegenProperty property2 = cm.vars.get(1);
        Assert.assertEquals(property2.baseName, "name");
        Assert.assertEquals(property2.datatype, "string");
        Assert.assertEquals(property2.name, "Name");
        Assert.assertEquals(property2.defaultValue, "null");
        Assert.assertEquals(property2.baseType, "string");
        Assert.assertTrue(getBooleanValue(property2, CodegenConstants.HAS_MORE_EXT_NAME));
        Assert.assertTrue(property2.required);
        Assert.assertTrue(getBooleanValue(property2, CodegenConstants.IS_PRIMITIVE_TYPE_EXT_NAME));
        Assert.assertTrue(getBooleanValue(property2, CodegenConstants.IS_NOT_CONTAINER_EXT_NAME));

        final CodegenProperty property3 = cm.vars.get(2);
        Assert.assertEquals(property3.baseName, "createdAt");
        Assert.assertEquals(property3.complexType, "time.Time");
        Assert.assertEquals(property3.datatype, "time.Time");
        Assert.assertEquals(property3.name, "CreatedAt");
        Assert.assertEquals(property3.defaultValue, "null");
        Assert.assertEquals(property3.baseType, "time.Time");
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
        final DefaultCodegen codegen = new GoClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 2);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "id");
        Assert.assertEquals(property1.datatype, "int64");
        Assert.assertEquals(property1.name, "Id");
        Assert.assertEquals(property1.defaultValue, "null");
        Assert.assertEquals(property1.baseType, "int64");
        Assert.assertTrue(getBooleanValue(property1, CodegenConstants.HAS_MORE_EXT_NAME));
        Assert.assertTrue(property1.required);
        Assert.assertTrue(getBooleanValue(property1, CodegenConstants.IS_PRIMITIVE_TYPE_EXT_NAME));
        Assert.assertTrue(getBooleanValue(property1, CodegenConstants.IS_NOT_CONTAINER_EXT_NAME));

        final CodegenProperty property2 = cm.vars.get(1);
        Assert.assertEquals(property2.baseName, "urls");
        Assert.assertEquals(property2.datatype, "[]string");
        Assert.assertEquals(property2.name, "Urls");
        Assert.assertEquals(property2.baseType, "array");
        Assert.assertTrue(getBooleanValue(property1, CodegenConstants.HAS_MORE_EXT_NAME));
        Assert.assertEquals(property2.containerType, "array");
        Assert.assertFalse(property2.required);
        Assert.assertTrue(getBooleanValue(property1, CodegenConstants.IS_PRIMITIVE_TYPE_EXT_NAME));
        Assert.assertTrue(getBooleanValue(property1, CodegenConstants.IS_CONTAINER_EXT_NAME));
    }

    @Test(description = "convert a model with a map property")
    public void mapPropertyTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("translations", new MapSchema()
                        .additionalProperties(new StringSchema()))
                .addRequiredItem("id");
        final DefaultCodegen codegen = new GoClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "translations");
        Assert.assertEquals(property1.datatype, "map[string]string");
        Assert.assertEquals(property1.name, "Translations");
        Assert.assertEquals(property1.baseType, "map");
        Assert.assertEquals(property1.containerType, "map");
        Assert.assertFalse(property1.required);
        Assert.assertTrue(getBooleanValue(property1, CodegenConstants.IS_CONTAINER_EXT_NAME));
        Assert.assertTrue(getBooleanValue(property1, CodegenConstants.IS_PRIMITIVE_TYPE_EXT_NAME));
    }

    @Test(description = "convert a model with complex property")
    public void complexPropertyTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("children", new Schema().$ref("#/definitions/Children"));
        final DefaultCodegen codegen = new GoClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "children");
        Assert.assertEquals(property1.datatype, "Children");
        Assert.assertEquals(property1.name, "Children");
        Assert.assertEquals(property1.baseType, "Children");
        Assert.assertFalse(property1.required);
        getBooleanValue(property1, CodegenConstants.IS_NOT_CONTAINER_EXT_NAME);
    }

    @Test(description = "convert a model with complex list property")
    public void complexListProperty() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("children", new ArraySchema()
                        .items(new Schema().$ref("#/definitions/Children")));
        final DefaultCodegen codegen = new GoClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "children");
        Assert.assertEquals(property1.datatype, "[]Children");
        Assert.assertEquals(property1.name, "Children");
        Assert.assertEquals(property1.baseType, "array");
        Assert.assertEquals(property1.containerType, "array");
        Assert.assertFalse(property1.required);
        Assert.assertTrue(getBooleanValue(property1, CodegenConstants.IS_CONTAINER_EXT_NAME));
    }

    @Test(description = "convert a model with complex map property")
    public void complexMapProperty() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("children", new MapSchema()
                        .additionalProperties(new Schema().$ref("#/definitions/Children")));
        final DefaultCodegen codegen = new GoClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);
        Assert.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("Children")).size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "children");
        Assert.assertEquals(property1.complexType, "Children");
        Assert.assertEquals(property1.datatype, "map[string]Children");
        Assert.assertEquals(property1.name, "Children");
        Assert.assertEquals(property1.baseType, "map");
        Assert.assertEquals(property1.containerType, "map");
        Assert.assertFalse(property1.required);
        Assert.assertTrue(getBooleanValue(property1, CodegenConstants.IS_CONTAINER_EXT_NAME));
        Assert.assertFalse(getBooleanValue(property1, CodegenConstants.IS_NOT_CONTAINER_EXT_NAME));
    }

    @Test(description = "convert an array model")
    public void arrayModelTest() {
        final Schema schema = new ArraySchema()
                .items(new Schema().$ref("#/definitions/Children"))
                .description("an array model");

        final DefaultCodegen codegen = new GoClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "an array model");
        Assert.assertEquals(cm.vars.size(), 0);
        Assert.assertEquals(cm.imports.size(), 1);
    }

    @Test(description = "convert an map model")
    public void mapModelTest() {
        final Schema schema = new ArraySchema()
                .description("a map model")
                .additionalProperties(new Schema().$ref("#/definitions/Children"));
        final DefaultCodegen codegen = new GoClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a map model");
        Assert.assertEquals(cm.vars.size(), 0);
        Assert.assertEquals(cm.imports.size(), 1);
        Assert.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("Children")).size(), 1);
    }

    @DataProvider(name = "modelNames")
    public static Object[][] primeNumbers() {
        return new Object[][] {
            {"sample", "Sample"},
            {"sample_name", "SampleName"},
            {"sample__name", "SampleName"},
            {"/sample", "Sample"},
            {"\\sample", "Sample"},
            {"sample.name", "SampleName"},
            {"_sample", "Sample"},
        };
    }

    @Test(dataProvider = "modelNames", description = "avoid inner class")
    public void modelNameTest(String name, String expectedName) {
        final Schema schema = new Schema();
        final DefaultCodegen codegen = new GoClientCodegen();
        final CodegenModel cm = codegen.fromModel(name, schema);

        Assert.assertEquals(cm.name, name);
        Assert.assertEquals(cm.classname, expectedName);
    }
}
