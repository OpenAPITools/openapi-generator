package org.openapitools.codegen.typescript.typescriptangular;

import org.junit.Assert;
import org.junit.Test;
import org.openapitools.codegen.languages.TypeScriptAngularClientCodegen;

public class TypeScriptAngularClientCodegenTest {
    @Test
    public void testModelFileSuffix() {
        TypeScriptAngularClientCodegen codegen = new TypeScriptAngularClientCodegen();
        codegen.additionalProperties().put("modelFileSuffix", "MySuffix");
        codegen.additionalProperties().put("modelSuffix", "MySuffix");
        codegen.processOpts();

        Assert.assertEquals(codegen.toModelFilename("testName"), "testNameMySuffix");
    }
}
