package org.openapitools.codegen.scala.http4s.scala3;

import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.ScalaHttp4sScala3ClientCodegen;
import io.swagger.models.*;
import io.swagger.models.properties.*;

import org.testng.Assert;
import org.testng.annotations.Test;

@SuppressWarnings("static-method")
public class ScalaHttp4sScala3ClientCodegenModelTest {

    @Test(description = "convert a simple java model")
    public void simpleModelTest() {
        final Model model = new ModelImpl()
                .description("a sample model")
                .property("id", new LongProperty())
                .property("name", new StringProperty())
                .required("id")
                .required("name");
        final ScalaHttp4sScala3ClientCodegen codegen = new ScalaHttp4sScala3ClientCodegen();



        // TODO: Complete this test.
        //Assert.fail("Not implemented.");
    }

}

