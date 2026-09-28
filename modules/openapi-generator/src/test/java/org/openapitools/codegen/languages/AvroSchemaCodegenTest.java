package org.openapitools.codegen.languages;

import org.openapitools.codegen.model.EnumVarMap;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class AvroSchemaCodegenTest {
    @Test
    public void preservesRawValuesWhenSanitizingEnumSymbols() {
        AvroSchemaCodegen codegen = new AvroSchemaCodegen();
        List<Object> values = Arrays.asList("a-b", null, "a_b", 42);

        List<EnumVarMap> enumVars = codegen.buildEnumVars(values, "string");

        Assert.assertEquals(enumVars.size(), 3);
        Assert.assertEquals(enumVars.get(0).getEnumValue(), "\"a_b\"");
        Assert.assertEquals(enumVars.get(1).getEnumValue(), "\"a_b2\"");
        Assert.assertEquals(enumVars.get(2).getEnumValue(), "\"_42\"");
        Assert.assertEquals(enumVars.get(0).getEnumRawValue(), "a-b");
        Assert.assertEquals(enumVars.get(1).getEnumRawValue(), "a_b");
        Assert.assertEquals(enumVars.get(2).getEnumRawValue(), Integer.valueOf(42));
        Assert.assertEquals(values, Arrays.asList("a-b", null, "a_b", 42));
    }

    @Test
    public void preservesSyntheticUnknownDefaultEntry() {
        AvroSchemaCodegen codegen = new AvroSchemaCodegen();
        codegen.setEnumUnknownDefaultCase(true);

        EnumVarMap fallback = codegen.buildEnumVars(Collections.emptyList(), "string").get(0);
        List<EnumVarMap> enumVars = codegen.buildEnumVars(Arrays.asList(null, "in-progress"), "string");

        Assert.assertEquals(enumVars.size(), 2);
        Assert.assertEquals(enumVars.get(0).getEnumRawValue(), "in-progress");
        Assert.assertEquals(enumVars.get(1), fallback);
    }
}
