package io.swagger.codegen.lagomScalaApi;

import com.google.common.collect.Sets;
import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.languages.ScalaLagomServerCodegen;
import io.swagger.codegen.languages.ScalaClientCodegen;
import io.swagger.models.ArrayModel;
import io.swagger.models.Model;
import io.swagger.models.ModelImpl;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.DateTimeProperty;
import io.swagger.models.properties.LongProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.RefProperty;
import io.swagger.models.properties.StringProperty;
import org.testng.Assert;
import org.testng.annotations.Test;

@SuppressWarnings("static-method")
public class LagomScalaApiModelTest {

  @Test(description = "convert a simple scala model")
  public void simpleModelTest() {
    final Model model = new ModelImpl()
        .description("a sample model")
        .property("id", new LongProperty())
        .property("name", new StringProperty())
        .property("createdAt", new DateTimeProperty())
        .required("id")
        .required("name");
    final DefaultCodegen codegen = new ScalaLagomServerCodegen();
    final CodegenModel cm = codegen.fromModel("sample", model);

    Assert.assertEquals(cm.name, "sample");
    Assert.assertEquals(cm.classname, "Sample");
    Assert.assertEquals(cm.description, "a sample model");
    Assert.assertEquals(cm.vars.size(), 3);

    final CodegenProperty property1 = cm.vars.get(0);
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

    final CodegenProperty property2 = cm.vars.get(1);
    Assert.assertEquals(property2.baseName, "name");
    Assert.assertEquals(property2.getter, "getName");
    Assert.assertEquals(property2.setter, "setName");
    Assert.assertEquals(property2.datatype, "String");
    Assert.assertEquals(property2.name, "name");
    Assert.assertEquals(property2.defaultValue, "null");
    Assert.assertEquals(property2.baseType, "String");
    Assert.assertTrue(property2.hasMore);
    Assert.assertTrue(property2.required);
    Assert.assertTrue(property2.isNotContainer);

    final CodegenProperty property3 = cm.vars.get(2);
    Assert.assertEquals(property3.baseName, "createdAt");
    Assert.assertEquals(property3.getter, "getCreatedAt");
    Assert.assertEquals(property3.setter, "setCreatedAt");
    Assert.assertEquals(property3.datatype, "DateTime");
    Assert.assertEquals(property3.name, "createdAt");
    Assert.assertEquals(property3.defaultValue, "null");
    Assert.assertEquals(property3.baseType, "DateTime");
    Assert.assertFalse(property3.hasMore);
    Assert.assertFalse(property3.required);
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
    final DefaultCodegen codegen = new ScalaClientCodegen();
    final CodegenModel cm = codegen.fromModel("sample", model);

    Assert.assertEquals(cm.name, "sample");
    Assert.assertEquals(cm.classname, "Sample");
    Assert.assertEquals(cm.description, "a sample model");
    Assert.assertEquals(cm.vars.size(), 2);

    final CodegenProperty property1 = cm.vars.get(1);
    Assert.assertEquals(property1.baseName, "urls");
    Assert.assertEquals(property1.getter, "getUrls");
    Assert.assertEquals(property1.setter, "setUrls");
    Assert.assertEquals(property1.datatype, "List[String]");
    Assert.assertEquals(property1.name, "urls");
    Assert.assertEquals(property1.defaultValue, "new ListBuffer[String]() ");
    Assert.assertEquals(property1.baseType, "List");
    Assert.assertEquals(property1.containerType, "array");
    Assert.assertFalse(property1.required);
    Assert.assertTrue(property1.isContainer);
  }

  @Test(description = "convert a model with a map property")
  public void mapPropertyTest() {
    final Model model = new ModelImpl()
        .description("a sample model")
        .property("translations", new MapProperty()
            .additionalProperties(new StringProperty()))
        .required("id");
    final DefaultCodegen codegen = new ScalaClientCodegen();
    final CodegenModel cm = codegen.fromModel("sample", model);

    Assert.assertEquals(cm.name, "sample");
    Assert.assertEquals(cm.classname, "Sample");
    Assert.assertEquals(cm.description, "a sample model");
    Assert.assertEquals(cm.vars.size(), 1);

    final CodegenProperty property1 = cm.vars.get(0);
    Assert.assertEquals(property1.baseName, "translations");
    Assert.assertEquals(property1.getter, "getTranslations");
    Assert.assertEquals(property1.setter, "setTranslations");
    Assert.assertEquals(property1.datatype, "Map[String, String]");
    Assert.assertEquals(property1.name, "translations");
    Assert.assertEquals(property1.defaultValue, "new HashMap[String, String]() ");
    Assert.assertEquals(property1.baseType, "Map");
    Assert.assertEquals(property1.containerType, "map");
    Assert.assertFalse(property1.required);
    Assert.assertTrue(property1.isContainer);
  }

  @Test(description = "convert a model with complex properties")
  public void complexPropertyTest() {
    final Model model = new ModelImpl()
        .description("a sample model")
        .property("children", new RefProperty("#/definitions/Children"));
    final DefaultCodegen codegen = new ScalaClientCodegen();
    final CodegenModel cm = codegen.fromModel("sample", model);

    Assert.assertEquals(cm.name, "sample");
    Assert.assertEquals(cm.classname, "Sample");
    Assert.assertEquals(cm.description, "a sample model");
    Assert.assertEquals(cm.vars.size(), 1);

    final CodegenProperty property1 = cm.vars.get(0);
    Assert.assertEquals(property1.baseName, "children");
    Assert.assertEquals(property1.getter, "getChildren");
    Assert.assertEquals(property1.setter, "setChildren");
    Assert.assertEquals(property1.datatype, "Children");
    Assert.assertEquals(property1.name, "children");
    Assert.assertEquals(property1.defaultValue, "null");
    Assert.assertEquals(property1.baseType, "Children");
    Assert.assertFalse(property1.required);
    Assert.assertTrue(property1.isNotContainer);
  }

  @Test(description = "convert a model with complex list property")
  public void complexListPropertyTest() {
    final Model model = new ModelImpl()
        .description("a sample model")
        .property("children", new ArrayProperty()
            .items(new RefProperty("#/definitions/Children")));
    final DefaultCodegen codegen = new ScalaClientCodegen();
    final CodegenModel cm = codegen.fromModel("sample", model);

    Assert.assertEquals(cm.name, "sample");
    Assert.assertEquals(cm.classname, "Sample");
    Assert.assertEquals(cm.description, "a sample model");
    Assert.assertEquals(cm.vars.size(), 1);

    final CodegenProperty property1 = cm.vars.get(0);
    Assert.assertEquals(property1.baseName, "children");
    Assert.assertEquals(property1.complexType, "Children");
    Assert.assertEquals(property1.getter, "getChildren");
    Assert.assertEquals(property1.setter, "setChildren");
    Assert.assertEquals(property1.datatype, "List[Children]");
    Assert.assertEquals(property1.name, "children");
    Assert.assertEquals(property1.defaultValue, "new ListBuffer[Children]() ");
    Assert.assertEquals(property1.baseType, "List");
    Assert.assertEquals(property1.containerType, "array");
    Assert.assertFalse(property1.required);
    Assert.assertTrue(property1.isContainer);
  }

  @Test(description = "convert a model with complex map property")
  public void complexMapPropertyTest() {
    final Model model = new ModelImpl()
        .description("a sample model")
        .property("children", new MapProperty()
            .additionalProperties(new RefProperty("#/definitions/Children")));
    final DefaultCodegen codegen = new ScalaClientCodegen();
    final CodegenModel cm = codegen.fromModel("sample", model);

    Assert.assertEquals(cm.name, "sample");
    Assert.assertEquals(cm.classname, "Sample");
    Assert.assertEquals(cm.description, "a sample model");
    Assert.assertEquals(cm.vars.size(), 1);
    Assert.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("Children")).size(), 1);

    final CodegenProperty property1 = cm.vars.get(0);
    Assert.assertEquals(property1.baseName, "children");
    Assert.assertEquals(property1.complexType, "Children");
    Assert.assertEquals(property1.getter, "getChildren");
    Assert.assertEquals(property1.setter, "setChildren");
    Assert.assertEquals(property1.datatype, "Map[String, Children]");
    Assert.assertEquals(property1.name, "children");
    Assert.assertEquals(property1.defaultValue, "new HashMap[String, Children]() ");
    Assert.assertEquals(property1.baseType, "Map");
    Assert.assertEquals(property1.containerType, "map");
    Assert.assertFalse(property1.required);
    Assert.assertTrue(property1.isContainer);
    Assert.assertFalse(property1.isNotContainer);
  }

  @Test(description = "convert an array model")
  public void arrayModelTest() {
    final Model model = new ArrayModel()
        .description("an array model")
        .items(new RefProperty("#/definitions/Children"));
    final DefaultCodegen codegen = new ScalaClientCodegen();
    final CodegenModel cm = codegen.fromModel("sample", model);

    Assert.assertEquals(cm.name, "sample");
    Assert.assertEquals(cm.classname, "Sample");
    Assert.assertEquals(cm.description, "an array model");
    Assert.assertEquals(cm.vars.size(), 0);
    Assert.assertEquals(cm.parent, "ListBuffer[Children]");
    Assert.assertEquals(cm.imports.size(), 2);
    Assert.assertEquals(
        Sets.intersection(cm.imports, Sets.newHashSet("ListBuffer", "Children")).size(), 2);
  }

  @Test(description = "convert an map model")
  public void mapModelTest() {
    final Model model = new ModelImpl()
        .description("a map model")
        .additionalProperties(new RefProperty("#/definitions/Children"));
    final DefaultCodegen codegen = new ScalaClientCodegen();
    final CodegenModel cm = codegen.fromModel("sample", model);

    Assert.assertEquals(cm.name, "sample");
    Assert.assertEquals(cm.classname, "Sample");
    Assert.assertEquals(cm.description, "a map model");
    Assert.assertEquals(cm.vars.size(), 0);
    Assert.assertEquals(cm.parent, "HashMap[String, Children]");
    Assert.assertEquals(cm.imports.size(), 2);
    Assert
        .assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("HashMap", "Children")).size(),
            2);
  }
}
