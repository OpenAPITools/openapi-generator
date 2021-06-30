package org.openapitools.codegen.scala;

import org.junit.Assert;
import org.junit.Test;
import org.openapitools.codegen.languages.ScalaSttpClientCodegen;

import java.util.HashMap;
import java.util.Map;

public class SttpStringPropertyTest {

    @Test
    public void shouldUseDefaultValueIfAdditionalPropertiesAreEmpty(){
        ScalaSttpClientCodegen.StringProperty property = new ScalaSttpClientCodegen.StringProperty("k1", "desc", "default");
        Map<String, Object> additionalProperties = new HashMap<>();
        property.updateAdditionalProperties(additionalProperties);

        Assert.assertEquals("default", additionalProperties.get("k1"));
    }

    @Test
    public void shouldUseGivenValueIfProvided(){
        ScalaSttpClientCodegen.StringProperty property = new ScalaSttpClientCodegen.StringProperty("k1", "desc", "default");
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put("k1", "custom");
        property.updateAdditionalProperties(additionalProperties);

        Assert.assertEquals("custom", additionalProperties.get("k1"));
    }
}
