package io.swagger.codegen.java;

import com.google.common.collect.Sets;
import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenParameter;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.languages.JavaClientCodegen;
import io.swagger.models.ArrayModel;
import io.swagger.models.Model;
import io.swagger.models.ModelImpl;
import io.swagger.models.parameters.QueryParameter;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.ByteArrayProperty;
import io.swagger.models.properties.DateTimeProperty;
import io.swagger.models.properties.DecimalProperty;
import io.swagger.models.properties.IntegerProperty;
import io.swagger.models.properties.LongProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.RefProperty;
import io.swagger.models.properties.StringProperty;
import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.util.List;

@SuppressWarnings("static-method")
public class JavaModelTest {

    @Test(description = "convert a simple java model")
    public void simpleModelTest() {
        final Model model = new ModelImpl()
                .description("a sample model")
                .property("id", new LongProperty())
                .property("name", new StringProperty()
                        .example("Tony"))
                .property("createdAt", new DateTimeProperty())
                .required("id")
                .required("name");
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 3);

        final List<CodegenProperty> vars = cm.vars;

        final CodegenProperty property1 = vars.get(0);
        Assert.assertEquals(property1.baseName, "id");
        Assert.assertEquals(property1.getter, "getId");
        Assert.assertEquals(property1.setter, "setId");
        Assert.assertEquals(property1.datatype, "Long");
        Assert.assertEquals(property1.name, "id");
        Assert.assertEquals(property1.defaultValue, "null");
        Assert.assertEquals(property1.baseType, "Long");
        Assert.assertTrue(property1.hasMore);
        Assert.assertTrue(property1.required);
        Assert.assertTrue(property1.isNotContainer);

        final CodegenProperty property2 = vars.get(1);
        Assert.assertEquals(property2.baseName, "name");
        Assert.assertEquals(property2.getter, "getName");
        Assert.assertEquals(property2.setter, "setName");
        Assert.assertEquals(property2.datatype, "String");
        Assert.assertEquals(property2.name, "name");
        Assert.assertEquals(property2.defaultValue, "null");
        Assert.assertEquals(property2.baseType, "String");
        Assert.assertEquals(property2.example, "Tony");
        Assert.assertTrue(property2.hasMore);
        Assert.assertTrue(property2.required);
        Assert.assertTrue(property2.isNotContainer);

        final CodegenProperty property3 = vars.get(2);
        Assert.assertEquals(property3.baseName, "createdAt");
        Assert.assertEquals(property3.getter, "getCreatedAt");
        Assert.assertEquals(property3.setter, "setCreatedAt");
        Assert.assertEquals(property3.datatype, "Date");
        Assert.assertEquals(property3.name, "createdAt");
        Assert.assertEquals(property3.defaultValue, "null");
        Assert.assertEquals(property3.baseType, "Date");
        Assert.assertNull(property3.hasMore);
        Assert.assertNull(property3.required);
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
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 2);

        final CodegenProperty property = cm.vars.get(1);
        Assert.assertEquals(property.baseName, "urls");
        Assert.assertEquals(property.getter, "getUrls");
        Assert.assertEquals(property.setter, "setUrls");
        Assert.assertEquals(property.datatype, "List<String>");
        Assert.assertEquals(property.name, "urls");
        Assert.assertEquals(property.defaultValue, "new ArrayList<String>()");
        Assert.assertEquals(property.baseType, "List");
        Assert.assertEquals(property.containerType, "array");
        Assert.assertNull(property.required);
        Assert.assertTrue(property.isContainer);
    }

    @Test(description = "convert a model with a map property")
    public void mapPropertyTest() {
        final Model model = new ModelImpl()
                .description("a sample model")
                .property("translations", new MapProperty()
                        .additionalProperties(new StringProperty()))
                .required("id");
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property = cm.vars.get(0);
        Assert.assertEquals(property.baseName, "translations");
        Assert.assertEquals(property.getter, "getTranslations");
        Assert.assertEquals(property.setter, "setTranslations");
        Assert.assertEquals(property.datatype, "Map<String, String>");
        Assert.assertEquals(property.name, "translations");
        Assert.assertEquals(property.defaultValue, "new HashMap<String, String>()");
        Assert.assertEquals(property.baseType, "Map");
        Assert.assertEquals(property.containerType, "map");
        Assert.assertNull(property.required);
        Assert.assertTrue(property.isContainer);
    }

    @Test(description = "convert a model with a map with complex list property")
    public void mapWithListPropertyTest() {
        final Model model = new ModelImpl()
                .description("a sample model")
                .property("translations",
                        new MapProperty().additionalProperties(new ArrayProperty().items(new RefProperty("Pet"))))
                .required("id");
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property = cm.vars.get(0);
        Assert.assertEquals(property.baseName, "translations");
        Assert.assertEquals(property.getter, "getTranslations");
        Assert.assertEquals(property.setter, "setTranslations");
        Assert.assertEquals(property.datatype, "Map<String, List<Pet>>");
        Assert.assertEquals(property.name, "translations");
        Assert.assertEquals(property.defaultValue, "new HashMap<String, List<Pet>>()");
        Assert.assertEquals(property.baseType, "Map");
        Assert.assertEquals(property.containerType, "map");
        Assert.assertNull(property.required);
        Assert.assertTrue(property.isContainer);
    }

    @Test(description = "convert a model with a 2D list property")
    public void list2DPropertyTest() {
        final Model model = new ModelImpl().name("sample").property("list2D", new ArrayProperty().items(
                new ArrayProperty().items(new RefProperty("Pet"))));
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property = cm.vars.get(0);
        Assert.assertEquals(property.baseName, "list2D");
        Assert.assertEquals(property.getter, "getList2D");
        Assert.assertEquals(property.setter, "setList2D");
        Assert.assertEquals(property.datatype, "List<List<Pet>>");
        Assert.assertEquals(property.name, "list2D");
        Assert.assertEquals(property.defaultValue, "new ArrayList<List<Pet>>()");
        Assert.assertEquals(property.baseType, "List");
        Assert.assertEquals(property.containerType, "array");
        Assert.assertNull(property.required);
        Assert.assertTrue(property.isContainer);
    }

    @Test(description = "convert a model with complex properties")
    public void complexPropertiesTest() {
        final Model model = new ModelImpl().description("a sample model")
                .property("children", new RefProperty("#/definitions/Children"));
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property = cm.vars.get(0);
        Assert.assertEquals(property.baseName, "children");
        Assert.assertEquals(property.getter, "getChildren");
        Assert.assertEquals(property.setter, "setChildren");
        Assert.assertEquals(property.datatype, "Children");
        Assert.assertEquals(property.name, "children");
        Assert.assertEquals(property.defaultValue, "null");
        Assert.assertEquals(property.baseType, "Children");
        Assert.assertNull(property.required);
        Assert.assertTrue(property.isNotContainer);
    }

    @Test(description = "convert a model with complex list property")
    public void complexListPropertyTest() {
        final Model model = new ModelImpl()
                .description("a sample model")
                .property("children", new ArrayProperty().items(new RefProperty("#/definitions/Children")));
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property = cm.vars.get(0);
        Assert.assertEquals(property.baseName, "children");
        Assert.assertEquals(property.complexType, "Children");
        Assert.assertEquals(property.getter, "getChildren");
        Assert.assertEquals(property.setter, "setChildren");
        Assert.assertEquals(property.datatype, "List<Children>");
        Assert.assertEquals(property.name, "children");
        Assert.assertEquals(property.defaultValue, "new ArrayList<Children>()");
        Assert.assertEquals(property.baseType, "List");
        Assert.assertEquals(property.containerType, "array");
        Assert.assertNull(property.required);
        Assert.assertTrue(property.isContainer);
    }

    @Test(description = "convert a model with complex map property")
    public void complexMapPropertyTest() {
        final Model model = new ModelImpl()
                .description("a sample model")
                .property("children", new MapProperty().additionalProperties(new RefProperty("#/definitions/Children")));
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);
        Assert.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("Map", "List", "Children")).size(), 3);

        final CodegenProperty property = cm.vars.get(0);
        Assert.assertEquals(property.baseName, "children");
        Assert.assertEquals(property.complexType, "Children");
        Assert.assertEquals(property.getter, "getChildren");
        Assert.assertEquals(property.setter, "setChildren");
        Assert.assertEquals(property.datatype, "Map<String, Children>");
        Assert.assertEquals(property.name, "children");
        Assert.assertEquals(property.defaultValue, "new HashMap<String, Children>()");
        Assert.assertEquals(property.baseType, "Map");
        Assert.assertEquals(property.containerType, "map");
        Assert.assertNull(property.required);
        Assert.assertTrue(property.isContainer);
        Assert.assertNull(property.isNotContainer);

    }

    @Test(description = "convert an array model")
    public void arrayModelTest() {
        final Model model = new ArrayModel()
                .description("an array model")
                .items(new RefProperty("#/definitions/Children"));
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "an array model");
        Assert.assertEquals(cm.vars.size(), 0);
        Assert.assertEquals(cm.parent, "ArrayList<Children>");
        Assert.assertEquals(cm.imports.size(), 4);
        Assert.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("ApiModel", "List", "ArrayList", "Children")).size(), 4);
    }

    @Test(description = "convert an map model")
    public void mapModelTest() {
        final Model model = new ModelImpl()
                .description("an map model")
                .additionalProperties(new RefProperty("#/definitions/Children"));
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "an map model");
        Assert.assertEquals(cm.vars.size(), 0);
        Assert.assertEquals(cm.parent, "HashMap<String, Children>");
        Assert.assertEquals(cm.imports.size(), 4);
        Assert.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("ApiModel", "Map", "HashMap", "Children")).size(), 4);
    }

    @Test(description = "convert a model with upper-case property names")
    public void upperCaseNamesTest() {
        final Model model = new ModelImpl()
                .description("a model with upper-case property names")
                .property("NAME", new StringProperty())
                .required("NAME");
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property = cm.vars.get(0);
        Assert.assertEquals(property.baseName, "NAME");
        Assert.assertEquals(property.getter, "getNAME");
        Assert.assertEquals(property.setter, "setNAME");
        Assert.assertEquals(property.datatype, "String");
        Assert.assertEquals(property.name, "NAME");
        Assert.assertEquals(property.defaultValue, "null");
        Assert.assertEquals(property.baseType, "String");
        Assert.assertNull(property.hasMore);
        Assert.assertTrue(property.required);
        Assert.assertTrue(property.isNotContainer);
    }

    @Test(description = "convert a model with a 2nd char upper-case property names")
    public void secondCharUpperCaseNamesTest() {
        final Model model = new ModelImpl()
                .description("a model with a 2nd char upper-case property names")
                .property("pId", new StringProperty())
                .required("pId");
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property = cm.vars.get(0);
        Assert.assertEquals(property.baseName, "pId");
        Assert.assertEquals(property.getter, "getPId");
        Assert.assertEquals(property.setter, "setPId");
        Assert.assertEquals(property.datatype, "String");
        Assert.assertEquals(property.name, "pId");
        Assert.assertEquals(property.defaultValue, "null");
        Assert.assertEquals(property.baseType, "String");
        Assert.assertNull(property.hasMore);
        Assert.assertTrue(property.required);
        Assert.assertTrue(property.isNotContainer);
    }

    @Test(description = "convert hyphens per issue 503")
    public void hyphensTest() {
        final Model model = new ModelImpl()
                .description("a sample model")
                .property("created-at", new DateTimeProperty());
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        final CodegenProperty property = cm.vars.get(0);
        Assert.assertEquals(property.baseName, "created-at");
        Assert.assertEquals(property.getter, "getCreatedAt");
        Assert.assertEquals(property.setter, "setCreatedAt");
        Assert.assertEquals(property.name, "createdAt");
    }

    @Test(description = "convert query[password] to queryPassword")
    public void squareBracketsTest() {
        final Model model = new ModelImpl()
                .description("a sample model")
                .property("query[password]", new StringProperty());
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        final CodegenProperty property = cm.vars.get(0);
        Assert.assertEquals(property.baseName, "query[password]");
        Assert.assertEquals(property.getter, "getQueryPassword");
        Assert.assertEquals(property.setter, "setQueryPassword");
        Assert.assertEquals(property.name, "queryPassword");
    }

    @Test(description = "properly escape names per 567")
    public void escapeNamesTest() {
        final Model model = new ModelImpl()
                .description("a sample model")
                .property("created-at", new DateTimeProperty());
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("with.dots", model);

        Assert.assertEquals(cm.classname, "WithDots");
    }

    @Test(description = "convert a model with binary data")
    public void binaryDataTest() {
        final Model model = new ModelImpl()
                .description("model with binary")
                .property("inputBinaryData", new ByteArrayProperty());
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        final CodegenProperty property = cm.vars.get(0);
        Assert.assertEquals(property.baseName, "inputBinaryData");
        Assert.assertEquals(property.getter, "getInputBinaryData");
        Assert.assertEquals(property.setter, "setInputBinaryData");
        Assert.assertEquals(property.datatype, "byte[]");
        Assert.assertEquals(property.name, "inputBinaryData");
        Assert.assertEquals(property.defaultValue, "null");
        Assert.assertEquals(property.baseType, "byte[]");
        Assert.assertNull(property.hasMore);
        Assert.assertNull(property.required);
        Assert.assertTrue(property.isNotContainer);
    }

    @Test(description = "translate an invalid param name")
    public void invalidParamNameTest() {
        final Model model = new ModelImpl()
                .description("a model with a 2nd char upper-case property names")
                .property("_", new StringProperty());
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property = cm.vars.get(0);
        Assert.assertEquals(property.baseName, "_");
        Assert.assertEquals(property.getter, "getU");
        Assert.assertEquals(property.setter, "setU");
        Assert.assertEquals(property.datatype, "String");
        Assert.assertEquals(property.name, "u");
        Assert.assertEquals(property.defaultValue, "null");
        Assert.assertEquals(property.baseType, "String");
        Assert.assertNull(property.hasMore);
        Assert.assertTrue(property.isNotContainer);
    }

    @Test(description = "convert a parameter")
    public void convertParameterTest() {
        final QueryParameter parameter = new QueryParameter()
                .property(new IntegerProperty())
                .name("limit")
                .required(true);
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenParameter cm = codegen.fromParameter(parameter, null);

        Assert.assertNull(cm.allowableValues);
    }

    @Test(description = "types used by inner properties should be imported")
    public void mapWithAnListOfBigDecimalTest() {
        final CodegenModel cm1 = new JavaClientCodegen().fromModel("sample", new ModelImpl()
                .description("model with Map<String, List<BigDecimal>>")
                .property("map", new MapProperty().additionalProperties(new ArrayProperty(new DecimalProperty()))));
        Assert.assertEquals(cm1.vars.get(0).datatype, "Map<String, List<BigDecimal>>");
        Assert.assertTrue(cm1.imports.contains("BigDecimal"));

        final CodegenModel cm2 = new JavaClientCodegen().fromModel("sample", new ModelImpl()
                .description("model with Map<String, Map<String, List<BigDecimal>>>")
                .property("map", new MapProperty().additionalProperties(new MapProperty().additionalProperties(new ArrayProperty(new DecimalProperty())))));
        Assert.assertEquals(cm2.vars.get(0).datatype, "Map<String, Map<String, List<BigDecimal>>>");
        Assert.assertTrue(cm2.imports.contains("BigDecimal"));
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
                {"Sample", "Sample"},
        };
    }

    @Test(dataProvider = "modelNames", description = "avoid inner class")
    public void modelNameTest(String name, String expectedName) {
        final Model model = new ModelImpl();
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel(name, model);

        Assert.assertEquals(cm.name, name);
        Assert.assertEquals(cm.classname, expectedName);
    }
}
