package io.swagger.client.model;

import org.junit.Test;

import com.google.gson.Gson;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

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
        assertEquals(EnumTest.EnumStringEnum.UPPER.getValue(), "UPPER");
        assertEquals(EnumTest.EnumStringEnum.LOWER.toString(), "lower");
        assertEquals(EnumTest.EnumStringEnum.LOWER.getValue(), "lower");

        assertEquals(EnumTest.EnumIntegerEnum.NUMBER_1.toString(), "1");
        assertTrue(EnumTest.EnumIntegerEnum.NUMBER_1.getValue() == 1);
        assertEquals(EnumTest.EnumIntegerEnum.NUMBER_MINUS_1.toString(), "-1");
        assertTrue(EnumTest.EnumIntegerEnum.NUMBER_MINUS_1.getValue() == -1);

        assertEquals(EnumTest.EnumNumberEnum.NUMBER_1_DOT_1.toString(), "1.1");
        assertTrue(EnumTest.EnumNumberEnum.NUMBER_1_DOT_1.getValue() == 1.1);
        assertEquals(EnumTest.EnumNumberEnum.NUMBER_MINUS_1_DOT_2.toString(), "-1.2");
        assertTrue(EnumTest.EnumNumberEnum.NUMBER_MINUS_1_DOT_2.getValue() == -1.2);

        // test serialization
        Gson gson = new Gson();
        String json = gson.toJson(enumTest);
        assertEquals(json, "{\"enum_string\":\"lower\",\"enum_integer\":1,\"enum_number\":1.1}");

        // test deserialization
        EnumTest fromString = gson.fromJson(json, EnumTest.class);
        assertEquals(fromString.getEnumString().toString(), "lower");
        assertEquals(fromString.getEnumString().getValue(), "lower");
        assertEquals(fromString.getEnumInteger().toString(), "1");
        assertTrue(fromString.getEnumInteger().getValue() == 1);
        assertEquals(fromString.getEnumNumber().toString(), "1.1");
        assertTrue(fromString.getEnumNumber().getValue() == 1.1);
    }
}
