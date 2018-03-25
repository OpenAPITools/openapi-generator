package org.openapitools.codegen.php;

import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.languages.PhpClientCodegen;

import org.testng.Assert;
import org.testng.annotations.Test;

public class PhpClientExampleTest {
    @Test(description = "sets example value")
    public void exampleValueTest() {
        PhpClientCodegen clientCodegen = new PhpClientCodegen();
        CodegenParameter p = new CodegenParameter();
        p.baseType = "object";

        clientCodegen.setParameterExampleValue(p);
        Assert.assertEquals(p.example, "new \\stdClass");
    }
}
