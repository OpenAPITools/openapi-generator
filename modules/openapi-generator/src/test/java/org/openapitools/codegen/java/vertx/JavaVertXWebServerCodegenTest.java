package org.openapitools.codegen.java.vertx;

import org.openapitools.codegen.languages.JavaVertXWebServerCodegen;
import org.testng.Assert;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class JavaVertXWebServerCodegenTest {

    private JavaVertXWebServerCodegen underTest;

    @BeforeMethod
    public void setup() {
        this.underTest = new JavaVertXWebServerCodegen();
    }

    @Test
    public void itShouldSetTheDefaultTemplateKeys() {
        underTest.processOpts();

        Assert.assertTrue(underTest.apiTemplateFiles().containsKey("api.mustache"));
        Assert.assertTrue(underTest.apiTemplateFiles().containsKey("apiHandler.mustache"));
        Assert.assertTrue(underTest.apiTemplateFiles().containsKey("apiImpl.mustache"));
    }

    @Test
    public void itShouldNotSetApiImplMustacheKeyWhenInterfaceOnlyIsTrue() {
        underTest.additionalProperties().put(JavaVertXWebServerCodegen.INTERFACE_ONLY, "true");
        underTest.processOpts();

        Assert.assertFalse(underTest.apiTemplateFiles().containsKey("apiImpl.mustache"));
    }
}
