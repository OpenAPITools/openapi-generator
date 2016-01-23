package io.swagger.codegen.java;

import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.languages.JavaClientCodegen;
import io.swagger.models.ComposedModel;
import io.swagger.models.Model;
import io.swagger.models.ModelImpl;
import io.swagger.models.RefModel;
import io.swagger.models.properties.StringProperty;

import com.google.common.collect.Sets;
import org.testng.Assert;
import org.testng.annotations.Test;

public class JavaInheritanceTest {

    @SuppressWarnings("static-method")
    @Test(description = "convert a composed model")
    public void javaInheritanceTest() {
        final Model model = new ComposedModel().parent(new RefModel("Base"))
                .child(new ModelImpl().additionalProperties(new StringProperty()));

        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.parent, "Base");
        Assert.assertEquals(cm.imports, Sets.newHashSet("Base"));
    }
}
