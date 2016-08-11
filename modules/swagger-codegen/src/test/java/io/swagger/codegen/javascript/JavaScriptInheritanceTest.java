package io.swagger.codegen.javascript;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.testng.Assert;
import org.testng.annotations.Test;

import com.google.common.collect.Sets;

import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.languages.JavascriptClientCodegen;
import io.swagger.models.ComposedModel;
import io.swagger.models.Model;
import io.swagger.models.ModelImpl;
import io.swagger.models.RefModel;
import io.swagger.models.properties.StringProperty;

public class JavaScriptInheritanceTest {
    @SuppressWarnings("static-method")
    @Test(description = "convert a composed model with inheritance enabled")
    public void javascriptInheritanceTest() {
        ModelImpl base = new ModelImpl();
        base.addProperty("baseProp", new StringProperty().required(true));
        ModelImpl intf1 = new ModelImpl();
        intf1.addProperty("intf1Prop", new StringProperty());
        ModelImpl intf2 = new ModelImpl();
        intf2.addProperty("intf2Prop", new StringProperty().required(true));
        ModelImpl child = new ModelImpl();
        child.addProperty("childProp", new StringProperty().required(true));

        final Map<String, Model> allDefinitions = new HashMap<String, Model>();
        allDefinitions.put("Base", base);
        allDefinitions.put("Interface1", intf1);
        allDefinitions.put("Interface2", intf2);

        final Model model = new ComposedModel().parent(new RefModel("Base"))
                .interfaces(Arrays.asList(new RefModel("Interface1"), new RefModel("Interface2")))
                .child(child);

        final JavascriptClientCodegen codegen = new JavascriptClientCodegen();
        codegen.setUseInheritance(true);

        final CodegenModel cm = codegen.fromModel("sample", model, allDefinitions);
        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.parent, "Base");
        Assert.assertEquals(cm.interfaces, Arrays.asList("Interface1", "Interface2"));
        Assert.assertEquals(cm.imports, Sets.newHashSet("Base", "Interface1", "Interface2"));
        Assert.assertEquals(cm.vars.size(), 1);
        Assert.assertEquals(cm.vars.get(0).name, "childProp");
        Assert.assertEquals(cm.allVars.size(), 4);
        String[] allVars = {"intf1Prop", "intf2Prop", "baseProp", "childProp"};
        for (int i = 0; i < allVars.length; i++) {
            Assert.assertEquals(cm.allVars.get(i).name, allVars[i]);
        }
        Assert.assertEquals(cm.mandatory, Sets.newHashSet("childProp"));
        Assert.assertEquals(cm.allMandatory, Sets.newHashSet("baseProp", "intf2Prop", "childProp"));
    }

    @SuppressWarnings("static-method")
    @Test(description = "convert a composed model with inheritance disabled")
    public void javascriptNoInheritanceTest() {
        ModelImpl base = new ModelImpl();
        base.addProperty("baseProp", new StringProperty().required(true));
        ModelImpl intf1 = new ModelImpl();
        intf1.addProperty("intf1Prop", new StringProperty());
        ModelImpl intf2 = new ModelImpl();
        intf2.addProperty("intf2Prop", new StringProperty().required(true));
        ModelImpl child = new ModelImpl();
        child.addProperty("childProp", new StringProperty().required(true));

        final Map<String, Model> allDefinitions = new HashMap<String, Model>();
        allDefinitions.put("Base", base);
        allDefinitions.put("Interface1", intf1);
        allDefinitions.put("Interface2", intf2);

        final Model model = new ComposedModel().parent(new RefModel("Base"))
                .interfaces(Arrays.asList(new RefModel("Interface1"), new RefModel("Interface2")))
                .child(child);

        final JavascriptClientCodegen codegen = new JavascriptClientCodegen();
        codegen.setUseInheritance(false);

        final CodegenModel cm = codegen.fromModel("sample", model, allDefinitions);
        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.parent, "Base");
        Assert.assertEquals(cm.interfaces, Arrays.asList("Interface1", "Interface2"));
        Assert.assertEquals(cm.imports, Sets.newHashSet("Base", "Interface1", "Interface2"));
        Assert.assertEquals(cm.vars.size(), 4);
        Assert.assertEquals(cm.allVars.size(), 4);
        String[] allVars = {"intf1Prop", "intf2Prop", "baseProp", "childProp"};
        for (int i = 0; i < allVars.length; i++) {
            Assert.assertEquals(cm.vars.get(i).name, allVars[i]);
            Assert.assertEquals(cm.allVars.get(i).name, allVars[i]);
        }
        Assert.assertEquals(cm.mandatory, Sets.newHashSet("baseProp", "intf2Prop", "childProp"));
        Assert.assertEquals(cm.allMandatory, Sets.newHashSet("baseProp", "intf2Prop", "childProp"));
    }
}
