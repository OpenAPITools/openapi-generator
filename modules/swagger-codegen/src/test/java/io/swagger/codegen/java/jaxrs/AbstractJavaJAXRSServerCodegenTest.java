package io.swagger.codegen.java.jaxrs;

import org.testng.Assert;
import org.testng.annotations.Test;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.languages.AbstractJavaJAXRSServerCodegen;

public class AbstractJavaJAXRSServerCodegenTest {

    private final AbstractJavaJAXRSServerCodegen fakeJavaJAXRSCodegen = new P_AbstractJavaJAXRSServerCodegen();

    @Test
    public void convertApiName() throws Exception {
        Assert.assertEquals(fakeJavaJAXRSCodegen.toApiName("name"), "NameApi");
        Assert.assertEquals(fakeJavaJAXRSCodegen.toApiName("$name"), "NameApi");
        Assert.assertEquals(fakeJavaJAXRSCodegen.toApiName("nam#e"), "NameApi");
        Assert.assertEquals(fakeJavaJAXRSCodegen.toApiName("$another-fake?"), "AnotherFakeApi");
        Assert.assertEquals(fakeJavaJAXRSCodegen.toApiName("fake_classname_tags 123#$%^"), "FakeClassnameTags123Api");
    }

    @Test
    public void testInitialConfigValues() throws Exception {
        final AbstractJavaJAXRSServerCodegen codegen = new P_AbstractJavaJAXRSServerCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
        Assert.assertEquals(codegen.modelPackage(), "io.swagger.model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "io.swagger.model");
        Assert.assertEquals(codegen.apiPackage(), "io.swagger.api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "io.swagger.api");
        Assert.assertEquals(codegen.getInvokerPackage(), "io.swagger.api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "io.swagger.api");
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final AbstractJavaJAXRSServerCodegen codegen = new P_AbstractJavaJAXRSServerCodegen();
        codegen.setHideGenerationTimestamp(true);
        codegen.setModelPackage("xx.yyyyyyyy.model");
        codegen.setApiPackage("xx.yyyyyyyy.api");
        codegen.setInvokerPackage("xx.yyyyyyyy.invoker");
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
        Assert.assertEquals(codegen.modelPackage(), "xx.yyyyyyyy.model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "xx.yyyyyyyy.model");
        Assert.assertEquals(codegen.apiPackage(), "xx.yyyyyyyy.api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "xx.yyyyyyyy.api");
        Assert.assertEquals(codegen.getInvokerPackage(), "xx.yyyyyyyy.invoker");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "xx.yyyyyyyy.invoker");
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final AbstractJavaJAXRSServerCodegen codegen = new P_AbstractJavaJAXRSServerCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, "true");
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "xyz.yyyyy.mmmmm.model");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "xyz.yyyyy.aaaaa.api");
        codegen.additionalProperties().put(CodegenConstants.INVOKER_PACKAGE,"xyz.yyyyy.iiii.invoker");
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
        Assert.assertEquals(codegen.modelPackage(), "xyz.yyyyy.mmmmm.model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "xyz.yyyyy.mmmmm.model");
        Assert.assertEquals(codegen.apiPackage(), "xyz.yyyyy.aaaaa.api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "xyz.yyyyy.aaaaa.api");
        Assert.assertEquals(codegen.getInvokerPackage(), "xyz.yyyyy.iiii.invoker");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "xyz.yyyyy.iiii.invoker");
    }

    private static class P_AbstractJavaJAXRSServerCodegen extends AbstractJavaJAXRSServerCodegen {

        @Override
        public CodegenType getTag() {
            return null;
        }

        @Override
        public String getName() {
            return null;
        }

        @Override
        public String getHelp() {
            return null;
        }
    }
}
