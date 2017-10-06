package io.swagger.codegen.php;

import io.swagger.codegen.CodegenParameter;
import io.swagger.codegen.languages.PhpClientCodegen;

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
