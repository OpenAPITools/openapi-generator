package org.openapitools.codegen;

import com.google.common.collect.ImmutableList;
import org.testng.annotations.Test;

import java.util.List;

import static org.openapitools.codegen.CodegenModel.removeDuplicatedProperty;
import static org.testng.Assert.assertEquals;

public class CodegenModelTest {

    @Test
    public void shouldRemoveDupplicates() {
        // GIVEN
        final CodegenProperty property1 = new CodegenProperty();
        property1.baseName = "baseName";
        property1.name = "property1";

        final CodegenProperty property1bis = new CodegenProperty();
        property1bis.baseName = "baseName";
        property1bis.name = "property1_bis";

        final CodegenProperty property2 = new CodegenProperty();
        property2.baseName = "baseName2";
        property2.name = "property2";

        List<CodegenProperty> properties = ImmutableList.of(property1, property1bis, property2);

        // WHEN
        List<CodegenProperty> result = removeDuplicatedProperty(properties);

        // THEN
        assertEquals(ImmutableList.of(property1, property2), result);
    }

}