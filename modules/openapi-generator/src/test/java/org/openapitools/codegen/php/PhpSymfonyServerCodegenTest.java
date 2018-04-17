package org.openapitools.codegen.php;

import org.testng.Assert;
import org.testng.annotations.Test;

import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.languages.PhpSymfonyServerCodegen;

public class PhpSymfonyServerCodegenTest {

    @Test
    public void testInitialConfigValues() throws Exception {
        final PhpSymfonyServerCodegen codegen = new PhpSymfonyServerCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final PhpSymfonyServerCodegen codegen = new PhpSymfonyServerCodegen();
        codegen.setHideGenerationTimestamp(false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final PhpSymfonyServerCodegen codegen = new PhpSymfonyServerCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
    }

}
