package org.openapitools.codegen.swift6;

import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.Swift6ClientCodegen;
import io.swagger.models.*;
import io.swagger.models.properties.*;

import org.testng.Assert;
import org.testng.annotations.Test;

@SuppressWarnings("static-method")
public class Swift6ClientCodegenModelTest {

    @Test(description = "convert a simple java model")
    public void simpleModelTest() {
        final Model model = new ModelImpl()
                .description("a sample model")
                .property("id", new LongProperty())
                .property("name", new StringProperty())
                .required("id")
                .required("name");
        final DefaultCodegen codegen = new Swift6ClientCodegen();

        // TODO: Complete this test.
        Assert.fail("Not implemented.");
    }

}

