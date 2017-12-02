package io.swagger.codegen.typescript.fetch;

import com.google.common.collect.Sets;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.languages.TypeScriptFetchClientCodegen;
import io.swagger.oas.models.OpenAPI;
import io.swagger.oas.models.media.ArraySchema;
import io.swagger.oas.models.media.DateSchema;
import io.swagger.oas.models.media.DateTimeSchema;
import io.swagger.oas.models.media.IntegerSchema;
import io.swagger.oas.models.media.Schema;
import io.swagger.oas.models.media.StringSchema;
import io.swagger.parser.v3.OpenAPIV3Parser;
import io.swagger.parser.v3.util.SchemaTypeUtil;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.Arrays;
import java.util.HashMap;

import static io.swagger.codegen.CodegenConstants.IS_ENUM_EXT_NAME;
import static io.swagger.codegen.languages.helpers.ExtensionHelper.getBooleanValue;
import static io.swagger.codegen.utils.ModelUtils.updateCodegenPropertyEnum;

@SuppressWarnings("static-method")
public class TypeScriptFetchModelTest {

    @Test(description = "convert a simple TypeScript Angular model")
    public void simpleModelTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("id", new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperties("name", new StringSchema())
                .addProperties("createdAt", new DateTimeSchema())
                .addProperties("birthDate", new DateSchema())
                .addRequiredItem("id")
                .addRequiredItem("name");

        final DefaultCodegen codegen = new TypeScriptFetchClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 4);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "id");
        Assert.assertEquals(property1.datatype, "number");
        Assert.assertEquals(property1.name, "id");
        Assert.assertEquals(property1.defaultValue, "undefined");
        Assert.assertEquals(property1.baseType, "number");
        Assert.assertTrue(getBooleanValue(property1, CodegenConstants.HAS_MORE_EXT_NAME));
        Assert.assertTrue(property1.required);
        Assert.assertTrue(getBooleanValue(property1, CodegenConstants.IS_NOT_CONTAINER_EXT_NAME));

        final CodegenProperty property2 = cm.vars.get(1);
        Assert.assertEquals(property2.baseName, "name");
        Assert.assertEquals(property2.datatype, "string");
        Assert.assertEquals(property2.name, "name");
        Assert.assertEquals(property2.defaultValue, "undefined");
        Assert.assertEquals(property2.baseType, "string");
        Assert.assertTrue(getBooleanValue(property2, CodegenConstants.HAS_MORE_EXT_NAME));
        Assert.assertTrue(property2.required);
        Assert.assertTrue(getBooleanValue(property2, CodegenConstants.IS_NOT_CONTAINER_EXT_NAME));

        final CodegenProperty property3 = cm.vars.get(2);
        Assert.assertEquals(property3.baseName, "createdAt");
        Assert.assertEquals(property3.complexType, null);
        Assert.assertEquals(property3.datatype, "Date");
        Assert.assertEquals(property3.name, "createdAt");
        Assert.assertEquals(property3.defaultValue, "undefined");
        Assert.assertTrue(getBooleanValue(property3, CodegenConstants.HAS_MORE_EXT_NAME));
        Assert.assertFalse(property3.required);
        Assert.assertTrue(getBooleanValue(property3, CodegenConstants.IS_NOT_CONTAINER_EXT_NAME));

        final CodegenProperty property4 = cm.vars.get(3);
        Assert.assertEquals(property4.baseName, "birthDate");
        Assert.assertEquals(property4.complexType, null);
        Assert.assertEquals(property4.datatype, "string");
        Assert.assertEquals(property4.name, "birthDate");
        Assert.assertEquals(property4.defaultValue, "undefined");
        Assert.assertFalse(getBooleanValue(property4, CodegenConstants.HAS_MORE_EXT_NAME));
        Assert.assertFalse(property4.required);
        Assert.assertTrue(getBooleanValue(property4, CodegenConstants.IS_NOT_CONTAINER_EXT_NAME));
    }

    @Test(description = "convert a model with list property")
    public void listPropertyTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("id", new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperties("urls", new ArraySchema().items(new StringSchema()))
                .addRequiredItem("id");
        final DefaultCodegen codegen = new TypeScriptFetchClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 2);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "id");
        Assert.assertEquals(property1.datatype, "number");
        Assert.assertEquals(property1.name, "id");
        Assert.assertEquals(property1.defaultValue, "undefined");
        Assert.assertEquals(property1.baseType, "number");
        Assert.assertTrue(getBooleanValue(property1, CodegenConstants.HAS_MORE_EXT_NAME));
        Assert.assertTrue(property1.required);
        Assert.assertTrue(getBooleanValue(property1, CodegenConstants.IS_NOT_CONTAINER_EXT_NAME));

        final CodegenProperty property2 = cm.vars.get(1);
        Assert.assertEquals(property2.baseName, "urls");
        Assert.assertEquals(property2.datatype, "Array<string>");
        Assert.assertEquals(property2.name, "urls");
        Assert.assertEquals(property2.baseType, "Array");
        Assert.assertFalse(getBooleanValue(property2, CodegenConstants.HAS_MORE_EXT_NAME));
        Assert.assertFalse(property2.required);
        Assert.assertTrue(getBooleanValue(property2, CodegenConstants.IS_CONTAINER_EXT_NAME));
    }

    @Test(description = "convert a model with complex property")
    public void complexPropertyTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("children", new Schema().$ref("#/definitions/Children"));
        final DefaultCodegen codegen = new TypeScriptFetchClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "children");
        Assert.assertEquals(property1.datatype, "Children");
        Assert.assertEquals(property1.name, "children");
        Assert.assertEquals(property1.defaultValue, "undefined");
        Assert.assertEquals(property1.baseType, "Children");
        Assert.assertFalse(property1.required);
        Assert.assertTrue(getBooleanValue(property1, CodegenConstants.IS_NOT_CONTAINER_EXT_NAME));
    }

    @Test(description = "convert a model with complex list property")
    public void complexListPropertyTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("children", new ArraySchema()
                        .items(new Schema().$ref("#/definitions/Children")));
        final DefaultCodegen codegen = new TypeScriptFetchClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "children");
        Assert.assertEquals(property1.complexType, "Children");
        Assert.assertEquals(property1.datatype, "Array<Children>");
        Assert.assertEquals(property1.name, "children");
        Assert.assertEquals(property1.baseType, "Array");
        Assert.assertFalse(property1.required);
        Assert.assertTrue(getBooleanValue(property1, CodegenConstants.IS_CONTAINER_EXT_NAME));
    }

    @Test(description = "convert an array model")
    public void arrayModelTest() {
        final Schema model = new ArraySchema()
                .items(new Schema().$ref("#/definitions/Children"))
                .description("an array model");
        final DefaultCodegen codegen = new TypeScriptFetchClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "an array model");
        Assert.assertEquals(cm.vars.size(), 0);
    }

    @Test(description = "convert a map model")
    public void mapModelTest() {
        final Schema model = new Schema()
                .description("a map model")
                .additionalProperties(new Schema().$ref("#/definitions/Children"));
        final DefaultCodegen codegen = new TypeScriptFetchClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a map model");
        Assert.assertEquals(cm.vars.size(), 0);
        Assert.assertEquals(cm.imports.size(), 1);
        Assert.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("Children")).size(), 1);
    }

    @Test(description = "test enum array model")
    public void enumArrayMdoelTest() {
        // TODO: update yaml file.
        final OpenAPI openAPI = new OpenAPIV3Parser().read("src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml");
        final DefaultCodegen codegen = new TypeScriptFetchClientCodegen();
        final Schema schema = openAPI.getComponents().getSchemas().get("EnumArrays");

        Schema property = (Schema) schema.getProperties().get("array_enum");
        CodegenProperty prope = codegen.fromProperty("array_enum", property);
        updateCodegenPropertyEnum(prope);
        Assert.assertEquals(prope.datatypeWithEnum, "Array<ArrayEnumEnum>");
        Assert.assertEquals(prope.enumName, "ArrayEnumEnum");
        Assert.assertTrue(getBooleanValue(prope, IS_ENUM_EXT_NAME));
        Assert.assertEquals(prope.allowableValues.get("values"), Arrays.asList("fish", "crab"));

        HashMap<String, String> fish= new HashMap<String, String>();
        fish.put("name", "Fish");
        fish.put("value", "'fish'");
        HashMap<String, String> crab= new HashMap<String, String>();
        crab.put("name", "Crab");
        crab.put("value", "'crab'");
        Assert.assertEquals(prope.allowableValues.get("enumVars"), Arrays.asList(fish, crab));

        // assert inner items
        Assert.assertEquals(prope.datatypeWithEnum, "Array<ArrayEnumEnum>");
        Assert.assertEquals(prope.enumName, "ArrayEnumEnum");
        Assert.assertTrue(getBooleanValue(prope.items, IS_ENUM_EXT_NAME));
        Assert.assertEquals(prope.items.allowableValues.get("values"), Arrays.asList("fish", "crab"));
        Assert.assertEquals(prope.items.allowableValues.get("enumVars"), Arrays.asList(fish, crab));

        //IMPORTANT: these are not final enum values, which may be further updated
        //by postProcessModels

    }

    @Test(description = "test enum model for values (numeric, string, etc)")
    public void enumMdoelValueTest() {
        final OpenAPI openAPI = new OpenAPIV3Parser().read("src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml");
        final DefaultCodegen codegen = new TypeScriptFetchClientCodegen();
        final Schema schema = openAPI.getComponents().getSchemas().get("Enum_Test");

        Schema property = (Schema) schema.getProperties().get("enum_integer");
        CodegenProperty prope = codegen.fromProperty("enum_integer", property);
        updateCodegenPropertyEnum(prope);
        Assert.assertEquals(prope.datatypeWithEnum, "EnumIntegerEnum");
        Assert.assertEquals(prope.enumName, "EnumIntegerEnum");
        Assert.assertTrue(getBooleanValue(prope, IS_ENUM_EXT_NAME));
        Assert.assertFalse(getBooleanValue(prope, CodegenConstants.IS_CONTAINER_EXT_NAME));
        Assert.assertNull(prope.items);
        Assert.assertEquals(prope.allowableValues.get("values"), Arrays.asList(1, -1));

        HashMap<String, String> one = new HashMap<String, String>();
        one.put("name", "NUMBER_1");
        one.put("value", "1");
        HashMap<String, String> minusOne = new HashMap<String, String>();
        minusOne.put("name", "NUMBER_MINUS_1");
        minusOne.put("value", "-1");
        Assert.assertEquals(prope.allowableValues.get("enumVars"), Arrays.asList(one, minusOne));

       //IMPORTANT: these are not final enum values, which may be further updated
       //by postProcessModels

    }


}
