package org.openapitools.codegen.python.uplink;

import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.PythonUplinkClientCodegen;
import io.swagger.models.*;
import io.swagger.models.properties.*;

import org.testng.Assert;
import org.testng.annotations.Test;

@SuppressWarnings("static-method")
public class PythonUplinkClientCodegenModelTest {

    @Test(description = "convert a simple java model")
    public void simpleModelTest() {
        final Model model = new ModelImpl()
                .description("a sample model")
                .property("id", new LongProperty())
                .property("name", new StringProperty())
                .required("id")
                .required("name");
        final DefaultCodegen codegen = new PythonUplinkClientCodegen();

        // TODO: Complete this test.
//        Assert.fail("Not implemented.");
    }

}

