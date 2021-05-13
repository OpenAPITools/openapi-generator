package org.openapitools.codegen.scala;

import org.junit.Assert;
import org.junit.Test;
import org.openapitools.codegen.languages.ScalaSttpClientCodegen;

import java.util.HashMap;
import java.util.Map;

public class SttpJsonLibraryPropertyTest {
    @Test
    public void shouldUseJson4sByDefault() {
        ScalaSttpClientCodegen.JsonLibraryProperty property = new ScalaSttpClientCodegen.JsonLibraryProperty();
        Map<String, Object> additionalProperties = new HashMap<>();
        property.updateAdditionalProperties(additionalProperties);
        Assert.assertEquals(true, additionalProperties.get("json4s"));
        Assert.assertEquals(false, additionalProperties.get("circe"));
    }

    @Test
    public void shouldUseJson4sIfExplicitlyAskTo() {
        ScalaSttpClientCodegen.JsonLibraryProperty property = new ScalaSttpClientCodegen.JsonLibraryProperty();
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put("jsonLibrary", "json4s");
        property.updateAdditionalProperties(additionalProperties);
        Assert.assertEquals(true, additionalProperties.get("json4s"));
        Assert.assertEquals(false, additionalProperties.get("circe"));
    }

    @Test
    public void shouldUseCirceIfExplicitlyAskTo() {
        ScalaSttpClientCodegen.JsonLibraryProperty property = new ScalaSttpClientCodegen.JsonLibraryProperty();
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put("jsonLibrary", "circe");
        property.updateAdditionalProperties(additionalProperties);
        Assert.assertEquals(false, additionalProperties.get("json4s"));
        Assert.assertEquals(true, additionalProperties.get("circe"));
    }
}
