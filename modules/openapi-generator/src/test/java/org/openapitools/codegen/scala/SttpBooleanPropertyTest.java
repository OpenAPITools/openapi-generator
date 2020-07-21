package org.openapitools.codegen.scala;

import org.junit.Assert;
import org.junit.Test;
import org.openapitools.codegen.languages.ScalaSttpClientCodegen;

import java.util.HashMap;
import java.util.Map;

public class SttpBooleanPropertyTest {
    @Test
    public void shouldUseDefaultValueIfAdditionalPropertiesAreEmpty() {
        ScalaSttpClientCodegen.BooleanProperty booleanProperty = new ScalaSttpClientCodegen.BooleanProperty("k1", "desc", false);
        Map<String, Object> additionalProperties = new HashMap<>();
        booleanProperty.updateAdditionalProperties(additionalProperties);

        Assert.assertEquals(false, additionalProperties.get("k1"));
    }

    @Test
    public void shouldUseGivenValueIfProvided() {
        ScalaSttpClientCodegen.BooleanProperty booleanProperty = new ScalaSttpClientCodegen.BooleanProperty("k1", "desc", false);
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put("k1", true);
        booleanProperty.updateAdditionalProperties(additionalProperties);

        Assert.assertEquals(true, additionalProperties.get("k1"));
    }
}
