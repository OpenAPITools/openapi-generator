package org.openapitools.codegen.gdscript;

import io.swagger.models.Model;
import io.swagger.models.ModelImpl;
import io.swagger.models.properties.LongProperty;
import io.swagger.models.properties.StringProperty;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.languages.GdscriptClientCodegen;
import org.testng.annotations.Test;

@SuppressWarnings("static-method")
public class GdscriptClientCodegenModelTest {

    @Test(description = "convert a simple java model")
    public void simpleModelTest() {
        final Model model = new ModelImpl()
                .description("a sample model")
                .property("id", new LongProperty())
                .property("name", new StringProperty())
                .required("id")
                .required("name");
        final DefaultCodegen codegen = new GdscriptClientCodegen();

        // TODO: Complete this test.
        //Assert.fail("Not implemented.");
    }

}

