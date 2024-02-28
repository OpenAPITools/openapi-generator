package org.openapitools.codegen.cppue4;

import org.openapitools.codegen.languages.CppUE4ClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

public class CppUE4AdditionalPropertiesTest {
    protected CppUE4ClientCodegen codegen = new CppUE4ClientCodegen();

    @Test(description = "When set modelNameCamelize is true, then modelName 'Model_HelloRequest' should be 'ModelHelloRequest'")
    public void shouldModelNameCamelize() {
        // Arrange
        String packageName = "OpenAPI";
        String modelName = "Model_HelloRequest";
        String expectedModelName = modelName.replace("_", "");
        boolean isCamelize = true;

        codegen.setModelNameCamelize(isCamelize);

        // Act
        String actualModelName = codegen.toModelName("Model_HelloRequest");

        // Assert
        Assert.assertEquals(actualModelName, packageName + expectedModelName);
    }

    @Test(description = "When set modelNameCamelize is false, then modelName 'Model_HelloRequest' should be 'Model_HelloRequest'")
    public void shouldNotModelNameCamelize() {
        // Arrange
        String packageName = "OpenAPI";
        String modelName = "Model_HelloRequest";
        boolean isCamelize = false;

        codegen.setModelNameCamelize(isCamelize);

        // Act
        String actualModelName = codegen.toModelName("Model_HelloRequest");

        // Assert
        Assert.assertEquals(actualModelName, packageName + modelName);
    }
}
