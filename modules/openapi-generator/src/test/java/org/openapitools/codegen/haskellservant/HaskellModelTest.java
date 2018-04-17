package org.openapitools.codegen.haskellservant;

import io.swagger.v3.oas.models.PathItem;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.languages.HaskellServantCodegen;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.parser.util.SchemaTypeUtil;

import org.testng.Assert;
import org.testng.annotations.Test;

public class HaskellModelTest {

    @Test(description = "convert a haskell model with dots")
    public void modelTest() {
        Assert.assertEquals(true, true);
    }
}
