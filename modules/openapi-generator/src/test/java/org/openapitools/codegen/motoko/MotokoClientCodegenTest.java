package org.openapitools.codegen.motoko;

import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.MotokoClientCodegen;
import io.swagger.models.*;
import io.swagger.parser.SwaggerParser;
import org.testng.Assert;
import org.testng.annotations.Test;

public class MotokoClientCodegenTest {

    MotokoClientCodegen codegen = new MotokoClientCodegen();

    @Test
    public void shouldSucceed() throws Exception {
        codegen.processOpts();

        Assert.assertEquals(codegen.getName(), "motoko");
        Assert.assertEquals(codegen.getTag(), CodegenType.CLIENT);
        Assert.assertNotNull(codegen.getHelp());
    }
}
