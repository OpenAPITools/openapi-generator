package org.openapitools.codegen.php;

import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.languages.MetariscPhpClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

public class MetariscPhpClientCodegenTest {
    @Test
    public void testInitialConfigValues() throws Exception {
        final MetariscPhpClientCodegen codegen = new MetariscPhpClientCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertTrue(codegen.isHideGenerationTimestamp());
        Assert.assertEquals(codegen.modelPackage(), "Metarisc\\Model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "Metarisc\\Model");

        Assert.assertEquals(codegen.apiPackage(), "Metarisc\\Api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "Metarisc\\Api");
        Assert.assertEquals(codegen.getInvokerPackage(), "Metarisc");

    }
}
