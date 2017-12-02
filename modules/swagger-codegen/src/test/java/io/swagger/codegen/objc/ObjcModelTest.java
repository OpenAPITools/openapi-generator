package io.swagger.codegen.objc;

import com.google.common.collect.Sets;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.languages.ObjcClientCodegen;
import io.swagger.oas.models.OpenAPI;
import io.swagger.oas.models.Operation;
import io.swagger.oas.models.PathItem;
import io.swagger.oas.models.media.ArraySchema;
import io.swagger.oas.models.media.DateTimeSchema;
import io.swagger.oas.models.media.Discriminator;
import io.swagger.oas.models.media.IntegerSchema;
import io.swagger.oas.models.media.MapSchema;
import io.swagger.oas.models.media.Schema;
import io.swagger.oas.models.media.StringSchema;
import io.swagger.parser.v3.OpenAPIV3Parser;
import io.swagger.parser.v3.util.SchemaTypeUtil;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.Map;

import static io.swagger.codegen.CodegenConstants.IS_ARRAY_MODEL_EXT_NAME;
import static io.swagger.codegen.languages.helpers.ExtensionHelper.getBooleanValue;

@SuppressWarnings("static-method")
public class ObjcModelTest {

    @Test(description = "convert a model with a advanced map property")
    public void advancedMapPropertyTest() {
        final Schema schema = new Schema()
        .description("a sample model")
        .addProperties("translations", new MapSchema()
                  .additionalProperties(new MapSchema().additionalProperties(new StringSchema())))
        .addRequiredItem("id");
        final DefaultCodegen codegen = new ObjcClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema);
        
        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "SWGSample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);
        
        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "translations");
        Assert.assertEquals(property1.datatype, "NSDictionary<NSString*, NSDictionary<NSString*, NSString*>*>*");
        Assert.assertEquals(property1.name, "translations");
        Assert.assertEquals(property1.baseType, "NSDictionary");
        Assert.assertEquals(property1.containerType, "map");
        Assert.assertFalse(property1.required);
        Assert.assertTrue(getBooleanValue(property1.getVendorExtensions(), CodegenConstants.IS_CONTAINER_EXT_NAME));
    }
    
    @Test(description = "convert a simple java model")
    public void simpleModelTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("id", new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperties("name", new StringSchema())
                .addProperties("createdAt", new DateTimeSchema())
                .addRequiredItem("id")
                .addRequiredItem("name")
                .discriminator(new Discriminator().mapping("test", ""));
        final DefaultCodegen codegen = new ObjcClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "SWGSample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 3);
        Assert.assertEquals(cm.discriminator,"test");

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "id");
        Assert.assertEquals(property1.datatype, "NSNumber*");
        Assert.assertEquals(property1.name, "_id");
        Assert.assertNull(property1.defaultValue);
        Assert.assertEquals(property1.baseType, "NSNumber");
        Assert.assertTrue(getBooleanValue(property1.getVendorExtensions(), CodegenConstants.HAS_MORE_EXT_NAME));
        Assert.assertTrue(property1.required);
        Assert.assertTrue(getBooleanValue(property1.getVendorExtensions(), CodegenConstants.IS_PRIMITIVE_TYPE_EXT_NAME));
        Assert.assertTrue(getBooleanValue(property1.getVendorExtensions(), CodegenConstants.IS_NOT_CONTAINER_EXT_NAME));

        final CodegenProperty property2 = cm.vars.get(1);
        Assert.assertEquals(property2.baseName, "name");
        Assert.assertEquals(property2.datatype, "NSString*");
        Assert.assertEquals(property2.name, "name");
        Assert.assertNull(property2.defaultValue);
        Assert.assertEquals(property2.baseType, "NSString");
        Assert.assertTrue(getBooleanValue(property2.getVendorExtensions(), CodegenConstants.HAS_MORE_EXT_NAME));
        Assert.assertTrue(property2.required);
        Assert.assertTrue(getBooleanValue(property2.getVendorExtensions(), CodegenConstants.IS_PRIMITIVE_TYPE_EXT_NAME));
        Assert.assertTrue(getBooleanValue(property2.getVendorExtensions(), CodegenConstants.IS_NOT_CONTAINER_EXT_NAME));

        final CodegenProperty property3 = cm.vars.get(2);
        Assert.assertEquals(property3.baseName, "createdAt");
        Assert.assertEquals(property3.datatype, "NSDate*");
        Assert.assertEquals(property3.name, "createdAt");
        Assert.assertNull(property3.defaultValue);
        Assert.assertEquals(property3.baseType, "NSDate");
        Assert.assertFalse(getBooleanValue(property3.getVendorExtensions(), CodegenConstants.HAS_MORE_EXT_NAME));
        Assert.assertFalse(property3.required);
        Assert.assertTrue(getBooleanValue(property3.getVendorExtensions(), CodegenConstants.IS_NOT_CONTAINER_EXT_NAME));
    }

    @Test(description = "convert a model with list property")
    public void listPropertyTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("id", new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperties("urls", new ArraySchema()
                        .items(new StringSchema()))
                .addRequiredItem("id");
        final DefaultCodegen codegen = new ObjcClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "SWGSample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 2);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "id");
        Assert.assertEquals(property1.datatype, "NSNumber*");
        Assert.assertEquals(property1.name, "_id");
        Assert.assertNull(property1.defaultValue);
        Assert.assertEquals(property1.baseType, "NSNumber");
        Assert.assertFalse(getBooleanValue(property1.getVendorExtensions(), CodegenConstants.HAS_MORE_EXT_NAME));
        Assert.assertTrue(property1.required);
        Assert.assertTrue(getBooleanValue(property1.getVendorExtensions(), CodegenConstants.IS_PRIMITIVE_TYPE_EXT_NAME));
        Assert.assertTrue(getBooleanValue(property1.getVendorExtensions(), CodegenConstants.IS_NOT_CONTAINER_EXT_NAME));

        final CodegenProperty property2 = cm.vars.get(1);
        Assert.assertEquals(property2.baseName, "urls");
        Assert.assertEquals(property2.datatype, "NSArray<NSString*>*");
        Assert.assertEquals(property2.name, "urls");
        Assert.assertNull(property2.defaultValue);
        Assert.assertEquals(property2.baseType, "NSArray");
        Assert.assertFalse(getBooleanValue(property2.getVendorExtensions(), CodegenConstants.HAS_MORE_EXT_NAME));
        Assert.assertEquals(property2.containerType, "array");
        Assert.assertFalse(property2.required);
        Assert.assertTrue(getBooleanValue(property2.getVendorExtensions(), CodegenConstants.IS_PRIMITIVE_TYPE_EXT_NAME));
        Assert.assertTrue(getBooleanValue(property2.getVendorExtensions(), CodegenConstants.IS_CONTAINER_EXT_NAME));
    }

    @Test(description = "convert a model with a map property")
    public void mapPropertyTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("translations", new MapSchema()
                        .additionalProperties(new StringSchema()))
                .addRequiredItem("id");
        final DefaultCodegen codegen = new ObjcClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "SWGSample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "translations");
        Assert.assertEquals(property1.datatype, "NSDictionary<NSString*, NSString*>*");
        Assert.assertEquals(property1.name, "translations");
        Assert.assertEquals(property1.baseType, "NSDictionary");
        Assert.assertEquals(property1.containerType, "map");
        Assert.assertFalse(property1.required);
        Assert.assertTrue(getBooleanValue(property1.getVendorExtensions(), CodegenConstants.IS_CONTAINER_EXT_NAME));
        Assert.assertTrue(getBooleanValue(property1.getVendorExtensions(), CodegenConstants.IS_PRIMITIVE_TYPE_EXT_NAME));
    }

    
    @Test(description = "convert a model with complex property")
    public void complexPropertyTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("children", new Schema().$ref("#/definitions/Children"));
        final DefaultCodegen codegen = new ObjcClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "SWGSample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "children");
        Assert.assertEquals(property1.datatype, "SWGChildren*");
        Assert.assertEquals(property1.name, "children");
        Assert.assertEquals(property1.baseType, "SWGChildren");
        Assert.assertFalse(property1.required);
        Assert.assertTrue(getBooleanValue(property1.getVendorExtensions(), CodegenConstants.IS_NOT_CONTAINER_EXT_NAME));
    }

    @Test(description = "convert a model with complex list property")
    public void complexListPropertyTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("children", new ArraySchema()
                        .items(new Schema().$ref("#/definitions/Children")));
        final DefaultCodegen codegen = new ObjcClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "SWGSample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "children");
        Assert.assertEquals(property1.complexType, "SWGChildren");
        Assert.assertEquals(property1.datatype, "NSArray<SWGChildren>*");
        Assert.assertEquals(property1.name, "children");
        Assert.assertEquals(property1.baseType, "NSArray");
        Assert.assertEquals(property1.containerType, "array");
        Assert.assertFalse(property1.required);
        Assert.assertTrue(getBooleanValue(property1.getVendorExtensions(), CodegenConstants.IS_CONTAINER_EXT_NAME));
    }

    @Test(description = "convert a model with complex map property")
    public void complexMapPropertyTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("children", new MapSchema()
                        .additionalProperties(new Schema().$ref("#/definitions/Children")));
        final DefaultCodegen codegen = new ObjcClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "SWGSample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);
        Assert.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("SWGChildren")).size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "children");
        Assert.assertEquals(property1.complexType, "SWGChildren");
        Assert.assertEquals(property1.datatype, "NSDictionary<SWGChildren>*");
        Assert.assertEquals(property1.name, "children");
        Assert.assertEquals(property1.baseType, "NSDictionary");
        Assert.assertEquals(property1.containerType, "map");
        Assert.assertFalse(property1.required);
        Assert.assertTrue(getBooleanValue(property1.getVendorExtensions(), CodegenConstants.IS_CONTAINER_EXT_NAME));
        Assert.assertFalse(getBooleanValue(property1.getVendorExtensions(), CodegenConstants.IS_NOT_CONTAINER_EXT_NAME));
    }

    @Test(description = "convert an array model")
    public void arrayModelTest() {
        final Schema schema = new ArraySchema()
                .items(new Schema().$ref("#/definitions/Children")
                .description("an array model"));
        final DefaultCodegen codegen = new ObjcClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "SWGSample");
        Assert.assertEquals(cm.description, "an array model");
        Assert.assertEquals(cm.vars.size(), 0);
        Assert.assertEquals(cm.parent, "NSMutableArray");
        Assert.assertEquals(cm.imports.size(), 1);
        Assert.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("SWGChildren")).size(), 1);
    }

    @Test(description = "convert an map model")
    public void mapModelTest() {
        final Schema schema = new Schema()
                .description("a map model")
                .additionalProperties(new Schema().$ref("#/definitions/Children"));
        final DefaultCodegen codegen = new ObjcClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "SWGSample");
        Assert.assertEquals(cm.description, "a map model");
        Assert.assertEquals(cm.vars.size(), 0);
        Assert.assertEquals(cm.parent, "NSMutableDictionary");
        Assert.assertEquals(cm.imports.size(), 1);
        Assert.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("SWGChildren")).size(), 1);
    }

    @Test(description = "test udid")
    public void udidAndPasswordDataModelTest() {
        // TODO: update yaml file.
        final OpenAPI openAPI =  new OpenAPIV3Parser().read("src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml");
        final DefaultCodegen codegen = new ObjcClientCodegen();
        Map<String, Schema> schemas = openAPI.getComponents().getSchemas();
        final Schema schema = schemas.get("format_test");
        Map<String, Schema> properties = schema.getProperties();
        Schema property =  properties.get("uuid");
        CodegenProperty prope = codegen.fromProperty("uuid", property);
        Assert.assertEquals(prope.baseType, "NSString");

        prope = codegen.fromProperty("password", property);
        Assert.assertEquals(prope.baseType, "NSString");
    }

    @Test(description = "test mixedProperties")
    public void mixedPropertiesDataModelTest() {
        // TODO: update yaml file.
        final OpenAPI openAPI =  new OpenAPIV3Parser().read("src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml");
        final DefaultCodegen codegen = new ObjcClientCodegen();

        final Map<String, Schema> schemas = openAPI.getComponents().getSchemas();
        final Schema schema = schemas.get("MixedPropertiesAndAdditionalPropertiesClass");

        final Map<String, Schema> properties = schema.getProperties();
        Schema property = properties.get("map");
        CodegenProperty prope = codegen.fromProperty("map", property);
        Assert.assertEquals(prope.baseType, "NSDictionary");
    }

    @Test(description = "test isArrayModel")
    public void isArrayModelModelTest() {
        // TODO: update yaml file.
        final OpenAPI openAPI =  new OpenAPIV3Parser().read("src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml");
        final DefaultCodegen codegen = new ObjcClientCodegen();
        final Map<String, Schema> schemas = openAPI.getComponents().getSchemas();
        final Schema schema = schemas.get("AnimalFarm");
        final CodegenModel codegenModel = codegen.fromModel("AnimalFarm", schema);

        Assert.assertEquals(getBooleanValue(codegenModel.getVendorExtensions(), IS_ARRAY_MODEL_EXT_NAME), true);
        Assert.assertEquals(codegenModel.arrayModelType,"SWGAnimal");
    }


    @Test(description = "test binary data")
    public void binaryDataModelTest() {
        // TODO: update json file.
        final OpenAPI openAPI =  new OpenAPIV3Parser().read("src/test/resources/2_0/binaryDataTest.json");
        final DefaultCodegen codegen = new ObjcClientCodegen();
        final String path = "/tests/binaryResponse";
        final Operation p = openAPI.getPaths().get(path).getPost();
        final CodegenOperation op = codegen.fromOperation(path, "post", p, openAPI.getComponents().getSchemas());

        Assert.assertEquals(op.returnType, "NSData*");
        Assert.assertEquals(op.bodyParam.dataType, "NSData*");
        Assert.assertTrue(op.bodyParam.isBinary);
        Assert.assertTrue(op.responses.get(0).isBinary);
    }

    @Test(description = "create proper imports per #316")
    public void issue316Test() {
        // TODO: update json file.
        final OpenAPI openAPI = new OpenAPIV3Parser().read("src/test/resources/2_0/postBodyTest.json");
        final DefaultCodegen codegen = new ObjcClientCodegen();

        final Map<String, PathItem> animalPaths = openAPI.getPaths();

        final PathItem animalOps = animalPaths.get("/animals");
        Assert.assertNotNull(animalOps.getPost());

        final CodegenOperation animalCo = codegen.fromOperation("/animals", "POST", animalOps.getPost(), openAPI.getComponents().getSchemas());
        Assert.assertEquals(animalCo.imports.size(), 1);
        Assert.assertTrue(animalCo.imports.contains("SWGAnimal"));

        final Map<String, PathItem> insectPaths = openAPI.getPaths();
        final PathItem insectOps = insectPaths.get("/insects");
        Assert.assertNotNull(insectOps.getPost());

        final CodegenOperation insectCo = codegen.fromOperation("/insects", "POST", insectOps.getPost(), openAPI.getComponents().getSchemas());
        Assert.assertEquals(insectCo.imports.size(), 1);
        Assert.assertTrue(insectCo.imports.contains("SWGInsect"));
    }
}
