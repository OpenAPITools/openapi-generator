package org.openapitools.codegen.scala;

import org.testng.Assert;
import org.testng.annotations.Test;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.languages.ScalaSttpClientCodegen;

import java.util.HashMap;
import java.util.Map;

public class SttpPackagePropertyTest {
    @Test
    public void shouldUseDefaultPackageNameIfAdditionalPropertiesAreEmpty(){
        ScalaSttpClientCodegen.PackageProperty property = new ScalaSttpClientCodegen.PackageProperty();
        Map<String, Object> additionalProperties = new HashMap<>();
        property.updateAdditionalProperties(additionalProperties);

        Assert.assertEquals(ScalaSttpClientCodegen.DEFAULT_PACKAGE_NAME + ".api",
                additionalProperties.get(CodegenConstants.API_PACKAGE));
        Assert.assertEquals(ScalaSttpClientCodegen.DEFAULT_PACKAGE_NAME + ".model",
                additionalProperties.get(CodegenConstants.MODEL_PACKAGE));
        Assert.assertEquals(ScalaSttpClientCodegen.DEFAULT_PACKAGE_NAME + ".core",
                additionalProperties.get(CodegenConstants.INVOKER_PACKAGE));
    }

    @Test
    public void shouldUseCustomMainPackageNameIfProvided(){
        ScalaSttpClientCodegen.PackageProperty property = new ScalaSttpClientCodegen.PackageProperty();
        Map<String, Object> additionalProperties = new HashMap<>();
        String customPackageName = "my.custom.pkg.name";
        additionalProperties.put("mainPackage", customPackageName);
        property.updateAdditionalProperties(additionalProperties);

        Assert.assertEquals(customPackageName + ".api",
                additionalProperties.get(CodegenConstants.API_PACKAGE));
        Assert.assertEquals(customPackageName + ".model",
                additionalProperties.get(CodegenConstants.MODEL_PACKAGE));
        Assert.assertEquals(customPackageName + ".core",
                additionalProperties.get(CodegenConstants.INVOKER_PACKAGE));
    }

    @Test
    public void shouldAllowToMixCustomPackages(){
        ScalaSttpClientCodegen.PackageProperty property = new ScalaSttpClientCodegen.PackageProperty();
        Map<String, Object> additionalProperties = new HashMap<>();
        String customPackageName = "my.custom.pkg.name";
        additionalProperties.put("mainPackage", customPackageName);
        String otherCustomPackageName = "some.other.custom.pkg.api";
        additionalProperties.put(CodegenConstants.API_PACKAGE, otherCustomPackageName);
        property.updateAdditionalProperties(additionalProperties);

        Assert.assertEquals(otherCustomPackageName,
                additionalProperties.get(CodegenConstants.API_PACKAGE));
        Assert.assertEquals(customPackageName + ".model",
                additionalProperties.get(CodegenConstants.MODEL_PACKAGE));
        Assert.assertEquals(customPackageName + ".core",
                additionalProperties.get(CodegenConstants.INVOKER_PACKAGE));
    }
}
