package org.openapitools.codegen.scala;

import org.openapitools.codegen.languages.ScalaSttpClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

public class SttpCodegenTest {

    private final ScalaSttpClientCodegen codegen = new ScalaSttpClientCodegen();

    @Test
    public void encodePath() {
        Assert.assertEquals(codegen.encodePath("{user_name}"), "${userName}");
        Assert.assertEquals(codegen.encodePath("{userName}"), "${userName}");
        Assert.assertEquals(codegen.encodePath("{UserName}"), "${userName}");
        Assert.assertEquals(codegen.encodePath("user_name"), "user_name");
        Assert.assertEquals(codegen.encodePath("before/{UserName}/after"), "before/${userName}/after");
    }

}
