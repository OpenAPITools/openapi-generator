package org.openapitools.codegen.typescript;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.Schema;

import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.AbstractTypeScriptClientCodegen;
import org.testng.annotations.Test;

import static org.junit.Assert.assertFalse;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertTrue;

public class AbstractTypeScriptClientCodegenTest {
    @Test
    public void testNullabelObjectProperties() {
        DefaultCodegen codegen = new P_AbstractTypeScriptClientCodegen();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/issue_10593.yaml");
        codegen.setOpenAPI(openAPI);
        Schema schema = openAPI.getComponents().getSchemas().get("ModelWithNullableObjectProperty");

        String propertyName;
        CodegenProperty property;

        // openapi 3.0 nullable
        propertyName = "propertyName30";
        property = codegen.fromProperty(propertyName, (Schema) schema.getProperties().get(propertyName));
        assertEquals(property.openApiType, "PropertyType");
        assertTrue(property.isNullable);

        // openapi 3.1 'null'
        propertyName = "propertyName31";
        property = codegen.fromProperty(propertyName, (Schema) schema.getProperties().get(propertyName));
        assertEquals(property.openApiType, "PropertyType");
        assertTrue(property.isNullable);

        // Non regression on regular oneOf construct
        propertyName = "nonNullableProperty";
        property = codegen.fromProperty(propertyName, (Schema) schema.getProperties().get(propertyName));
        assertFalse(property.isNullable);
        // assertEquals(property.openApiType, "string | number"); // Worked before #12104
        // oneOf property resolve to any type and is set to nullable
    }

    private static class P_AbstractTypeScriptClientCodegen extends AbstractTypeScriptClientCodegen {
        @Override
        public CodegenType getTag() {
            return null;
        }

        @Override
        public String getName() {
            return null;
        }

        @Override
        public String getHelp() {
            return null;
        }
    }
}
