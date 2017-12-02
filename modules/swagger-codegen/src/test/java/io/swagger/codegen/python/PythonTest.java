package io.swagger.codegen.python;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.languages.PythonClientCodegen;

import io.swagger.oas.models.OpenAPI;
import io.swagger.oas.models.Operation;
import io.swagger.oas.models.media.ArraySchema;
import io.swagger.oas.models.media.DateTimeSchema;
import io.swagger.oas.models.media.IntegerSchema;
import io.swagger.oas.models.media.MapSchema;
import io.swagger.oas.models.media.Schema;
import io.swagger.oas.models.media.StringSchema;
import io.swagger.parser.v3.OpenAPIV3Parser;
import io.swagger.parser.v3.util.SchemaTypeUtil;
import org.testng.Assert;
import org.testng.annotations.Test;
import org.testng.annotations.ITestAnnotation;

import com.google.common.collect.Sets;
import java.util.Map;

import static io.swagger.codegen.languages.helpers.ExtensionHelper.getBooleanValue;

@SuppressWarnings("static-method")
public class PythonTest {

    @Test(description = "convert a python model with dots")
    public void modelTest() {
        // TODO: update json file.
        final OpenAPI openAPI = new OpenAPIV3Parser().read("src/test/resources/2_0/v1beta3.json");
        final DefaultCodegen codegen = new PythonClientCodegen();

        final CodegenModel simpleName = codegen.fromModel("v1beta3.Binding", openAPI.getComponents().getSchemas().get("v1beta3.Binding"));
        Assert.assertEquals(simpleName.name, "v1beta3.Binding");
        Assert.assertEquals(simpleName.classname, "V1beta3Binding");
        Assert.assertEquals(simpleName.classVarName, "v1beta3_binding");

        final CodegenModel compoundName = codegen.fromModel("v1beta3.ComponentStatus", openAPI.getComponents().getSchemas().get("v1beta3.ComponentStatus"));
        Assert.assertEquals(compoundName.name, "v1beta3.ComponentStatus");
        Assert.assertEquals(compoundName.classname, "V1beta3ComponentStatus");
        Assert.assertEquals(compoundName.classVarName, "v1beta3_component_status");

        final String path = "/api/v1beta3/namespaces/{namespaces}/bindings";
        final Operation operation = openAPI.getPaths().get(path).getPost();
        final CodegenOperation codegenOperation = codegen.fromOperation(path, "get", operation, openAPI.getComponents().getSchemas());
        Assert.assertEquals(codegenOperation.returnType, "V1beta3Binding");
        Assert.assertEquals(codegenOperation.returnBaseType, "V1beta3Binding");
    }

    @Test(description = "convert a simple java model")
    public void simpleModelTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("id", new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperties("name", new StringSchema())
                .addProperties("createdAt", new DateTimeSchema())
                .addRequiredItem("id")
                .addRequiredItem("name");
        final DefaultCodegen codegen = new PythonClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 3);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "id");
        Assert.assertEquals(property1.datatype, "int");
        Assert.assertEquals(property1.name, "id");
        Assert.assertNull(property1.defaultValue);
        Assert.assertEquals(property1.baseType, "int");
        Assert.assertTrue(getBooleanValue(property1, CodegenConstants.HAS_MORE_EXT_NAME));
        Assert.assertTrue(property1.required);
        Assert.assertTrue(getBooleanValue(property1, CodegenConstants.IS_PRIMITIVE_TYPE_EXT_NAME));
        Assert.assertTrue(getBooleanValue(property1, CodegenConstants.IS_NOT_CONTAINER_EXT_NAME));

        final CodegenProperty property2 = cm.vars.get(1);
        Assert.assertEquals(property2.baseName, "name");
        Assert.assertEquals(property2.datatype, "str");
        Assert.assertEquals(property2.name, "name");
        Assert.assertNull(property2.defaultValue);
        Assert.assertEquals(property2.baseType, "str");
        Assert.assertTrue(getBooleanValue(property2, CodegenConstants.HAS_MORE_EXT_NAME));
        Assert.assertTrue(property2.required);
        Assert.assertTrue(getBooleanValue(property2, CodegenConstants.IS_PRIMITIVE_TYPE_EXT_NAME));
        Assert.assertTrue(getBooleanValue(property2, CodegenConstants.IS_NOT_CONTAINER_EXT_NAME));

        final CodegenProperty property3 = cm.vars.get(2);
        Assert.assertEquals(property3.baseName, "createdAt");
        Assert.assertEquals(property3.datatype, "datetime");
        Assert.assertEquals(property3.name, "created_at");
        Assert.assertNull(property3.defaultValue);
        Assert.assertEquals(property3.baseType, "datetime");
        Assert.assertTrue(getBooleanValue(property3, CodegenConstants.HAS_MORE_EXT_NAME));
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
        final DefaultCodegen codegen = new PythonClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 2);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "id");
        Assert.assertEquals(property1.datatype, "int");
        Assert.assertEquals(property1.name, "id");
        Assert.assertNull(property1.defaultValue);
        Assert.assertEquals(property1.baseType, "int");
        Assert.assertTrue(getBooleanValue(property1, CodegenConstants.HAS_MORE_EXT_NAME));
        Assert.assertTrue(property1.required);
        Assert.assertTrue(getBooleanValue(property1, CodegenConstants.IS_PRIMITIVE_TYPE_EXT_NAME));
        Assert.assertTrue(getBooleanValue(property1, CodegenConstants.IS_NOT_CONTAINER_EXT_NAME));

        final CodegenProperty property2 = cm.vars.get(1);
        Assert.assertEquals(property2.baseName, "urls");
        Assert.assertEquals(property2.datatype, "list[str]");
        Assert.assertEquals(property2.name, "urls");
        Assert.assertNull(property2.defaultValue);
        Assert.assertEquals(property2.baseType, "list");
        Assert.assertFalse(getBooleanValue(property2, CodegenConstants.HAS_MORE_EXT_NAME));
        Assert.assertEquals(property2.containerType, "array");
        Assert.assertFalse(property2.required);
        Assert.assertTrue(getBooleanValue(property2, CodegenConstants.IS_PRIMITIVE_TYPE_EXT_NAME));
        Assert.assertTrue(getBooleanValue(property2, CodegenConstants.IS_CONTAINER_EXT_NAME));
    }

    @Test(description = "convert a model with a map property")
    public void mapPropertyTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("translations", new MapSchema()
                        .additionalProperties(new StringSchema()))
                .addRequiredItem("id");
        final DefaultCodegen codegen = new PythonClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "translations");
        Assert.assertEquals(property1.datatype, "dict(str, str)");
        Assert.assertEquals(property1.name, "translations");
        Assert.assertEquals(property1.baseType, "dict");
        Assert.assertEquals(property1.containerType, "map");
        Assert.assertFalse(property1.required);
        Assert.assertTrue(getBooleanValue(property1, CodegenConstants.IS_PRIMITIVE_TYPE_EXT_NAME));
        Assert.assertTrue(getBooleanValue(property1, CodegenConstants.IS_CONTAINER_EXT_NAME));
    }

    @Test(description = "convert a model with complex property")
    public void complexPropertyTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("children", new Schema().$ref("#/definitions/Children"));
        final DefaultCodegen codegen = new PythonClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "children");
        Assert.assertEquals(property1.datatype, "Children");
        Assert.assertEquals(property1.name, "children");
        Assert.assertEquals(property1.baseType, "Children");
        Assert.assertFalse(property1.required);
        Assert.assertTrue(getBooleanValue(property1, CodegenConstants.IS_NOT_CONTAINER_EXT_NAME));
    }

    @Test(description = "convert a model with complex list property")
    public void complexListPropertyTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("children", new ArraySchema()
                        .items(new Schema().$ref("#/definitions/Children")));
        final DefaultCodegen codegen = new PythonClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "children");
        Assert.assertEquals(property1.complexType, "Children");
        Assert.assertEquals(property1.datatype, "list[Children]");
        Assert.assertEquals(property1.name, "children");
        Assert.assertEquals(property1.baseType, "list");
        Assert.assertEquals(property1.containerType, "array");
        Assert.assertFalse(property1.required);
        Assert.assertTrue(getBooleanValue(property1, CodegenConstants.IS_CONTAINER_EXT_NAME));
    }

    @Test(description = "convert a model with complex map property")
    public void complexMapPropertyTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("children", new MapSchema()
                        .additionalProperties(new Schema().$ref("#/definitions/Children")));
        final DefaultCodegen codegen = new PythonClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);
        Assert.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("Children")).size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "children");
        Assert.assertEquals(property1.complexType, "Children");
        Assert.assertEquals(property1.datatype, "dict(str, Children)");
        Assert.assertEquals(property1.name, "children");
        Assert.assertEquals(property1.baseType, "dict");
        Assert.assertEquals(property1.containerType, "map");
        Assert.assertFalse(property1.required);
        Assert.assertTrue(getBooleanValue(property1, CodegenConstants.IS_CONTAINER_EXT_NAME));
        Assert.assertFalse(getBooleanValue(property1, CodegenConstants.IS_NOT_CONTAINER_EXT_NAME));
    }


    // should not start with 'null'. need help from the community to investigate further
    @Test(enabled = false, description = "convert an array model")
    public void arrayModelTest() {
        final Schema schema = new ArraySchema()
                .items(new Schema().$ref("#/definitions/Children")
                .description("an array model"));
        final DefaultCodegen codegen = new PythonClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "an array model");
        Assert.assertEquals(cm.vars.size(), 0);
        Assert.assertEquals(cm.parent, "null<Children>");
        Assert.assertEquals(cm.imports.size(), 1);
        Assert.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("Children")).size(), 1);
    }

    // should not start with 'null'. need help from the community to investigate further
    @Test(enabled = false, description = "convert an map model")
    public void mapModelTest() {
        final Schema schema = new Schema()
                .description("a map model")
                .additionalProperties(new Schema().$ref("#/definitions/Children"));
        final DefaultCodegen codegen = new PythonClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a map model");
        Assert.assertEquals(cm.vars.size(), 0);
        Assert.assertEquals(cm.parent, "null<String, Children>");
        Assert.assertEquals(cm.imports.size(), 2); // TODO: need to verify
        Assert.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("Children")).size(), 1);
    }

}
