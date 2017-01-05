package io.swagger.client.model;

import java.io.StringWriter;
import java.io.PrintWriter;
import java.util.HashMap;
import java.util.ArrayList;
import java.util.Map;
import java.util.List;

import io.swagger.client.Pair;
import org.junit.*;
import static org.junit.Assert.*;

import com.fasterxml.jackson.databind.*;
import com.fasterxml.jackson.databind.SerializationFeature.*;

public class EnumValueTest {
    @Test
    public void testEnumClass() {
        assertEquals(EnumClass._ABC.toString(), "_abc");
        assertEquals(EnumClass._EFG.toString(), "-efg");
        assertEquals(EnumClass._XYZ_.toString(), "(xyz)");
    }

    @Test
    public void testEnumTest() {
        // test enum value
        EnumTest enumTest = new EnumTest();
        enumTest.setEnumString(EnumTest.EnumStringEnum.LOWER);
        enumTest.setEnumInteger(EnumTest.EnumIntegerEnum.NUMBER_1);
        enumTest.setEnumNumber(EnumTest.EnumNumberEnum.NUMBER_1_DOT_1);

        assertEquals(EnumTest.EnumStringEnum.UPPER.toString(), "UPPER");
        assertEquals(EnumTest.EnumStringEnum.LOWER.toString(), "lower");

        assertEquals(EnumTest.EnumIntegerEnum.NUMBER_1.toString(), "1");
        assertEquals(EnumTest.EnumIntegerEnum.NUMBER_MINUS_1.toString(), "-1");

        assertEquals(EnumTest.EnumNumberEnum.NUMBER_1_DOT_1.toString(), "1.1");
        assertEquals(EnumTest.EnumNumberEnum.NUMBER_MINUS_1_DOT_2.toString(), "-1.2");

        try {
            // test serialization (object => json)
            ObjectMapper mapper = new ObjectMapper();
            mapper.enable(SerializationFeature.WRITE_ENUMS_USING_TO_STRING);
            ObjectWriter ow = mapper.writer();
            String json = ow.writeValueAsString(enumTest);
            assertEquals(json, "{\"enum_string\":\"lower\",\"enum_integer\":\"1\",\"enum_number\":\"1.1\",\"outerEnum\":null}");

            // test deserialization (json => object)
            EnumTest fromString = mapper.readValue(json, EnumTest.class);
            assertEquals(fromString.getEnumString().toString(), "lower");
            assertEquals(fromString.getEnumInteger().toString(), "1");
            assertEquals(fromString.getEnumNumber().toString(), "1.1");

        } catch (Exception e) {
            fail("Exception thrown during serialization/deserialzation of JSON: " + e.getMessage());
        }

    }

}
