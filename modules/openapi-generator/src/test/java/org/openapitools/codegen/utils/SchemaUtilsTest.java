package org.openapitools.codegen.utils;

import io.swagger.v3.oas.models.media.ComposedSchema;
import io.swagger.v3.oas.models.media.NumberSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import org.junit.Test;

import java.math.BigDecimal;
import java.util.List;

import static org.junit.Assert.assertEquals;

public class SchemaUtilsTest {

    @Test
    public void cloneNumberSchema() {
        Schema schema = new NumberSchema()
                .name("test-schema")
                .minimum(new BigDecimal(100));

        Schema deepCopy = SchemaUtils.cloneSchema(schema);

        assertEquals(schema, deepCopy);
    }

    @Test
    public void cloneCustomSchema() {
        Schema schema = new Schema().type("money");

        Schema deepCopy = SchemaUtils.cloneSchema(schema);

        assertEquals(schema, deepCopy);
    }

    @Test
    public void cloneComposedSchema() {
        Schema base1 = new Schema()
                .name("Base1")
                .addProperty("foo", new StringSchema());
        Schema base2 = new Schema()
                .name("Base2")
                .addProperty("bar", new StringSchema());
        Schema composedSchema = new ComposedSchema()
                .name("Composed")
                .allOf(List.of(base1, base2))
                .addProperty("baz", new StringSchema());

        var deepCopy = SchemaUtils.cloneSchema(composedSchema);

        assertEquals(composedSchema, deepCopy);
    }
}
