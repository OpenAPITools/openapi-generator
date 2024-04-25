package org.openapitools.codegen.scala;

import org.testng.Assert;
import org.testng.annotations.Test;
import org.openapitools.codegen.languages.ScalaSttpClientCodegen;

import java.util.HashMap;
import java.util.Map;

public class SttpJsonLibraryPropertyTest {
    @Test
    public void shouldUseJson4sByDefault() {
        ScalaSttpClientCodegen.JsonLibraryProperty property = new ScalaSttpClientCodegen.JsonLibraryProperty();
        Map<String, Object> additionalProperties = new HashMap<>();
        property.updateAdditionalProperties(additionalProperties);
        Assert.assertEquals(additionalProperties.get("json4s"), true);
        Assert.assertEquals(additionalProperties.get("circe"), false);
    }

    @Test
    public void shouldUseJson4sIfExplicitlyAskTo() {
        ScalaSttpClientCodegen.JsonLibraryProperty property = new ScalaSttpClientCodegen.JsonLibraryProperty();
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put("jsonLibrary", "json4s");
        property.updateAdditionalProperties(additionalProperties);
        Assert.assertEquals(additionalProperties.get("json4s"), true);
        Assert.assertEquals(additionalProperties.get("circe"), false);
    }

    @Test
    public void shouldUseCirceIfExplicitlyAskTo() {
        ScalaSttpClientCodegen.JsonLibraryProperty property = new ScalaSttpClientCodegen.JsonLibraryProperty();
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put("jsonLibrary", "circe");
        property.updateAdditionalProperties(additionalProperties);
        Assert.assertEquals(additionalProperties.get("json4s"), false);
        Assert.assertEquals(additionalProperties.get("circe"), true);
    }
}
