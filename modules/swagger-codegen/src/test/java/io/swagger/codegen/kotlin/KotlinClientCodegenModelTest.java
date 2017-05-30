package io.swagger.codegen.kotlin;

import io.swagger.codegen.*;
import io.swagger.codegen.languages.KotlinClientCodegen;
import io.swagger.models.*;
import io.swagger.models.properties.*;

import org.testng.Assert;
import org.testng.annotations.Test;

@SuppressWarnings("static-method")
public class KotlinClientCodegenModelTest {

    private Model getArrayTestModel() {
        return new ModelImpl()
                .description("a sample model")
                .property("id", new LongProperty())
                .property("examples", new ArrayProperty().items(new StringProperty()))
                .required("id");
    }

    private Model getSimpleModel() {
        return new ModelImpl()
                .description("a sample model")
                .property("id", new LongProperty())
                .property("name", new StringProperty())
                .property("createdAt", new DateTimeProperty())
                .required("id")
                .required("name");
    }

    private Model getMapModel() {
        return new ModelImpl()
                .description("a sample model")
                .property("mapping", new MapProperty()
                        .additionalProperties(new StringProperty()))
                .required("id");
    }

    private Model getComplexModel() {
        return new ModelImpl()
                .description("a sample model")
                .property("child", new RefProperty("#/definitions/Child"));
    }

    @Test(description = "convert a simple model")
    public void simpleModelTest() {
        final Model model = getSimpleModel();
        final DefaultCodegen codegen = new KotlinClientCodegen();

        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 3);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "id");
        Assert.assertEquals(property1.datatype, "kotlin.Long");
        Assert.assertEquals(property1.name, "id");
        Assert.assertEquals(property1.defaultValue, "null");
        Assert.assertEquals(property1.baseType, "kotlin.Long");
        Assert.assertTrue(property1.hasMore);
        Assert.assertTrue(property1.required);
        Assert.assertTrue(property1.isPrimitiveType);
        Assert.assertTrue(property1.isNotContainer);

        final CodegenProperty property2 = cm.vars.get(1);
        Assert.assertEquals(property2.baseName, "name");
        Assert.assertEquals(property2.datatype, "kotlin.String");
        Assert.assertEquals(property2.name, "name");
        Assert.assertEquals(property2.defaultValue, "null");
        Assert.assertEquals(property2.baseType, "kotlin.String");
        Assert.assertTrue(property2.hasMore);
        Assert.assertTrue(property2.required);
        Assert.assertTrue(property2.isPrimitiveType);
        Assert.assertTrue(property2.isNotContainer);

        final CodegenProperty property3 = cm.vars.get(2);
        Assert.assertEquals(property3.baseName, "createdAt");
        Assert.assertEquals(property3.datatype, "java.time.LocalDateTime");
        Assert.assertEquals(property3.name, "createdAt");
        Assert.assertEquals(property3.defaultValue, "null");
        Assert.assertEquals(property3.baseType, "java.time.LocalDateTime");
        Assert.assertFalse(property3.hasMore);
        Assert.assertFalse(property3.required);
        Assert.assertTrue(property3.isNotContainer);
    }

    @Test(description = "convert a model with array property to default kotlin.Array")
    public void arrayPropertyTest() {
        final Model model = getArrayTestModel();

        final DefaultCodegen codegen = new KotlinClientCodegen();
        final CodegenModel generated = codegen.fromModel("sample", model);

        Assert.assertEquals(generated.name, "sample");
        Assert.assertEquals(generated.classname, "Sample");
        Assert.assertEquals(generated.description, "a sample model");
        Assert.assertEquals(generated.vars.size(), 2);

        final CodegenProperty property = generated.vars.get(1);
        Assert.assertEquals(property.baseName, "examples");
        Assert.assertEquals(property.getter, "getExamples");
        Assert.assertEquals(property.setter, "setExamples");
        Assert.assertEquals(property.datatype, "kotlin.collections.List<kotlin.String>");
        Assert.assertEquals(property.name, "examples");
        Assert.assertEquals(property.defaultValue, "null");
        Assert.assertEquals(property.baseType, "kotlin.collections.List");
        Assert.assertEquals(property.containerType, "array");
        Assert.assertFalse(property.required);
        Assert.assertTrue(property.isContainer);
    }
    @Test(description = "convert a model with a map property")
    public void mapPropertyTest() {
        final Model model = getMapModel();
        final DefaultCodegen codegen = new KotlinClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "mapping");
        Assert.assertEquals(property1.datatype, "kotlin.collections.Map<kotlin.String, kotlin.String>");
        Assert.assertEquals(property1.name, "mapping");
        Assert.assertEquals(property1.baseType, "kotlin.collections.Map");
        Assert.assertEquals(property1.containerType, "map");
        Assert.assertFalse(property1.required);
        Assert.assertTrue(property1.isContainer);
        Assert.assertTrue(property1.isPrimitiveType);
    }

    @Test(description = "convert a model with complex property")
    public void complexPropertyTest() {
        final Model model = getComplexModel();
        final DefaultCodegen codegen = new KotlinClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "child");
        Assert.assertEquals(property1.datatype, "Child");
        Assert.assertEquals(property1.name, "child");
        Assert.assertEquals(property1.baseType, "Child");
        Assert.assertFalse(property1.required);
        Assert.assertTrue(property1.isNotContainer);
    }
}

