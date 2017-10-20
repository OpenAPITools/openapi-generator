package io.swagger.codegen.javascript;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import io.swagger.oas.models.media.ComposedSchema;
import io.swagger.oas.models.media.Schema;
import io.swagger.oas.models.media.StringSchema;
import org.testng.Assert;
import org.testng.annotations.Test;

import com.google.common.collect.Sets;

import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.languages.JavascriptClientCodegen;

public class JavaScriptInheritanceTest {
    @SuppressWarnings("static-method")
    @Test(description = "convert a composed model with inheritance enabled")
    public void javascriptInheritanceTest() {
        Schema base = new Schema();
        base.addProperties("baseProp", new StringSchema());
        Schema intf1 = new Schema();
        intf1.addProperties("intf1Prop", new StringSchema());
        Schema intf2 = new Schema();
        intf2.addProperties("intf2Prop", new StringSchema());
        Schema child = new Schema();
        child.addProperties("childProp", new StringSchema());

        final Map<String, Schema> allSchemas = new HashMap<>();
        allSchemas.put("Base", base);
        allSchemas.put("Interface1", intf1);
        allSchemas.put("Interface2", intf2);

        final Schema schema = new ComposedSchema()
                .addAllOfItem(new Schema().$ref("Base"))
                .addAllOfItem(new Schema().$ref("Interface1"))
                .addAllOfItem(new Schema().$ref("Interface2"));

        final JavascriptClientCodegen codegen = new JavascriptClientCodegen();
        codegen.setUseInheritance(true);

        final CodegenModel cm = codegen.fromModel("sample", schema, allSchemas);
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
        Schema base = new Schema();
        base.addProperties("baseProp", new StringSchema());
        Schema intf1 = new Schema();
        intf1.addProperties("intf1Prop", new StringSchema());
        Schema intf2 = new Schema();
        intf2.addProperties("intf2Prop", new StringSchema());
        Schema child = new Schema();
        child.addProperties("childProp", new StringSchema());

        final Map<String, Schema> allSchemas = new HashMap<String, Schema>();
        allSchemas.put("Base", base);
        allSchemas.put("Interface1", intf1);
        allSchemas.put("Interface2", intf2);

        final Schema schema = new ComposedSchema()
                .addAllOfItem(new Schema().$ref("Base"))
                .addAllOfItem(new Schema().$ref("Interface1"))
                .addAllOfItem(new Schema().$ref("Interface2"));

        final JavascriptClientCodegen codegen = new JavascriptClientCodegen();
        codegen.setUseInheritance(false);

        final CodegenModel cm = codegen.fromModel("sample", schema, allSchemas);
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
