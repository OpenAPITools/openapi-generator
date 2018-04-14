package org.openapitools.codegen.eiffel;

import org.testng.Assert;
import org.testng.annotations.Test;

import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.languages.AbstractEiffelCodegen;

public class AbstractEiffelCodegenTest {

    @Test
    public void testInitialConfigValues() throws Exception {
        final AbstractEiffelCodegen codegen = new P_AbstractEiffelCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final AbstractEiffelCodegen codegen = new P_AbstractEiffelCodegen();
        codegen.setHideGenerationTimestamp(true);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final AbstractEiffelCodegen codegen = new P_AbstractEiffelCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, true);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
    }

    private static class P_AbstractEiffelCodegen extends AbstractEiffelCodegen {
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
