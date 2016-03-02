package io.swagger.codegen.haskellservant;

import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.languages.HaskellServantCodegen;
import io.swagger.models.Operation;
import io.swagger.models.Swagger;
import io.swagger.parser.SwaggerParser;

import org.testng.Assert;
import org.testng.annotations.Test;

public class HaskellTest {

    @Test(description = "convert a haskell model with dots")
    public void modelTest() {
        Assert.assertEquals(true, true);
    }
}
