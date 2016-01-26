package io.swagger.codegen.csharp;

import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.languages.CSharpClientCodegen;
import io.swagger.models.Model;
import io.swagger.models.ModelImpl;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.LongProperty;
import io.swagger.models.properties.StringProperty;
import org.testng.Assert;
import org.testng.annotations.Test;

@SuppressWarnings("static-method")
public class CSharpModelTest {

    @Test(description = "convert a model with array property to default List<T>")
    public void arrayPropertyTest() {
        final Model model = getArrayTestModel();

        final DefaultCodegen codegen = new CSharpClientCodegen();
        final CodegenModel generated = codegen.fromModel("sample", model);

        Assert.assertEquals(generated.name, "sample");
        Assert.assertEquals(generated.classname, "Sample");
        Assert.assertEquals(generated.description, "a sample model");
        Assert.assertEquals(generated.vars.size(), 2);

        final CodegenProperty property = generated.vars.get(1);
        Assert.assertEquals(property.baseName, "examples");
        Assert.assertEquals(property.getter, "getExamples");
        Assert.assertEquals(property.setter, "setExamples");
        Assert.assertEquals(property.datatype, "List<string>");
        Assert.assertEquals(property.name, "Examples");
        Assert.assertEquals(property.defaultValue, null);
        Assert.assertEquals(property.baseType, "List");
        Assert.assertEquals(property.containerType, "array");
        Assert.assertNull(property.required);
        Assert.assertTrue(property.isContainer);
    }

    @Test(description = "convert a model with array property to Collection<T>")
    public void arrayPropertyCollectionOptionTest() {
        final Model model = getArrayTestModel();

        final CSharpClientCodegen codegen = new CSharpClientCodegen();
        codegen.setUseCollection(true);

        final CodegenModel generated = codegen.fromModel("sample", model);

        Assert.assertEquals(generated.name, "sample");
        Assert.assertEquals(generated.vars.size(), 2);

        final CodegenProperty property = generated.vars.get(1);
        Assert.assertEquals(property.baseName, "examples");
        Assert.assertEquals(property.name, "Examples");
        Assert.assertEquals(property.defaultValue, null);
        Assert.assertEquals(property.datatype, "Collection<string>");
        Assert.assertEquals(property.baseType, "Collection");
        Assert.assertEquals(property.containerType, "array");
        Assert.assertNull(property.required);
        Assert.assertTrue(property.isContainer);
    }

    @Test(description = "convert a model with array property to Collection<T>")
    public void arrayPropertyICollectionOptionTest() {
        final Model model = getArrayTestModel();

        final CSharpClientCodegen codegen = new CSharpClientCodegen();
        codegen.setUseCollection(true);
        codegen.setReturnICollection(true);

        final CodegenModel generated = codegen.fromModel("sample", model);

        Assert.assertEquals(generated.name, "sample");
        Assert.assertEquals(generated.vars.size(), 2);

        final CodegenProperty property = generated.vars.get(1);
        Assert.assertEquals(property.baseName, "examples");
        Assert.assertEquals(property.name, "Examples");
        Assert.assertEquals(property.datatype, "Collection<string>",
                "returnICollection option should not modify property datatype");
        Assert.assertEquals(property.defaultValue, null);
        Assert.assertEquals(property.baseType, "Collection",
                "returnICollection option should not modify property baseType");
        Assert.assertEquals(property.containerType, "array");
        Assert.assertNull(property.required);
        Assert.assertTrue(property.isContainer);
    }

    private Model getArrayTestModel() {
        return new ModelImpl()
                .description("a sample model")
                .property("id", new LongProperty())
                .property("examples", new ArrayProperty().items(new StringProperty()))
                .required("id");
    }
}