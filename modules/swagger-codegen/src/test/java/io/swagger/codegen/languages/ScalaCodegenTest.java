package io.swagger.codegen.languages;

import io.swagger.codegen.CodegenProperty;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.BeforeTest;
import org.testng.annotations.Test;

import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

public class ScalaCodegenTest {

    private ScalaClientCodegen scalaClientCodegen;

    @BeforeTest
    public void setup() {
        this.scalaClientCodegen = new ScalaClientCodegen();
    }

    @Test
    public void shouldCallFormatIdentifierOnGetModelName() {
        String className = "models.WebsiteBodyModel";

        ScalaClientCodegen scalaClientCodegenSpy = Mockito.spy(this.scalaClientCodegen);

        String result = scalaClientCodegenSpy.toModelName(className);

        verify(scalaClientCodegenSpy, times(1)).stripPackageName(anyString());
        Assert.assertEquals("WebsiteBodyModel", result);
    }

    @Test
    public void shouldCallFormatIdentifierOnToEnumName() {
        String className = "models.WebsiteBodyModel";
        ScalaClientCodegen scalaClientCodegenSpy = Mockito.spy(this.scalaClientCodegen);
        CodegenProperty property = new CodegenProperty();
        property.baseName = className;
        String result = scalaClientCodegenSpy.toEnumName(property);
        verify(scalaClientCodegenSpy, times(1)).stripPackageName(anyString());
        verify(scalaClientCodegenSpy, times(1)).formatIdentifier(anyString(), anyBoolean());
        Assert.assertEquals("WebsiteBodyModel", result);
    }
}
