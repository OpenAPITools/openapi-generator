/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.utils;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.core.JsonPointer;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.JsonNodeType;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.datatype.joda.JodaModule;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.fasterxml.jackson.datatype.threetenbp.ThreeTenModule;
import org.openapitools.codegen.utils.JsonCache.CacheException;
import org.openapitools.codegen.utils.JsonCache.Root.MergePolicy;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.UnsupportedEncodingException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Locale;

import static org.testng.Assert.*;

/**
 * Tests the JsonCache class.
 *
 * @author aprice
 */
public class JsonCacheTest {
    public static class TestObject {
        @JsonProperty
        public boolean booleanField;
        @JsonProperty
        public double doubleField;
        @JsonProperty
        public float floatField;
        @JsonProperty
        public int intField;
        @JsonProperty
        public long longField;
        @JsonProperty
        public short shortField;
        @JsonProperty
        public String stringField;

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            TestObject other = (TestObject) obj;
            if (booleanField != other.booleanField)
                return false;
            if (Double.doubleToLongBits(doubleField) != Double.doubleToLongBits(other.doubleField))
                return false;
            if (Float.floatToIntBits(floatField) != Float.floatToIntBits(other.floatField))
                return false;
            if (intField != other.intField)
                return false;
            if (longField != other.longField)
                return false;
            if (shortField != other.shortField)
                return false;
            if (stringField == null) {
                if (other.stringField != null)
                    return false;
            } else if (!stringField.equals(other.stringField))
                return false;
            return true;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + (booleanField ? 1231 : 1237);
            long temp;
            temp = Double.doubleToLongBits(doubleField);
            result = prime * result + (int) (temp ^ (temp >>> 32));
            result = prime * result + Float.floatToIntBits(floatField);
            result = prime * result + intField;
            result = prime * result + (int) (longField ^ (longField >>> 32));
            result = prime * result + shortField;
            result = prime * result + ((stringField == null) ? 0 : stringField.hashCode());
            return result;
        }

        @Override
        public String toString() {
            return "TestObject [booleanField=" + booleanField + ", doubleField=" + doubleField + ", floatField="
                    + floatField + ", intField=" + intField + ", longField=" + longField + ", shortField=" + shortField
                    + ", stringField=" + stringField + "]";
        }
    }

    private static final String JSON = "{\n" // split
            + "  \"JsonCacheTest\": {\n" // split
            + "    \"array\": [1, \"2\", 3.0, 4.0],\n" // split
            + "    \"boolean\": true,\n" // split
            + "    \"date\": \"2019-01-16\",\n" // split
            + "    \"dateTimeZ\": \"2019-01-16T14:02:00.000Z\",\n" // split
            + "    \"number\": 3.14,\n" // split
            + "    \"string\": \"a string\",\n" // split
            + "    \"object\": {\n" // split
            + "      \"nestedArray\": [\"a\", \"b\", \"c\", \"d\"],\n" // split
            + "      \"nestedBoolean\": true,\n" // split
            + "      \"nestedDateTimeZ\": \"2019-01-16T14:02:00.000Z\",\n" // split
            + "      \"nestedNumber\": 2.72,\n" // split
            + "      \"nestedString\": \"a nested string\",\n" // split
            + "      \"nestedObject\": {\n" // split
            + "        \"a\": \"foo\",\n" // split
            + "        \"b\": \"bar\"\n" // split
            + "      }\n" // split
            + "    },\n" // split
            + "    \"testObjects\": [{\n" // split
            + "        \"booleanField\": true,\n" // split
            + "        \"shortField\": 1,\n" // split
            + "        \"intField\": 2,\n" // split
            + "        \"longField\": 3,\n" // split
            + "        \"floatField\": 1.23,\n" // split
            + "        \"doubleField\": 4.56,\n" // split
            + "        \"stringField\": \"a test string field\"\n" // split
            + "      }, {\n" // split
            + "        \"booleanField\": false,\n" // split
            + "        \"shortField\": 4,\n" // split
            + "        \"intField\": 5,\n" // split
            + "        \"longField\": 6,\n" // split
            + "        \"floatField\": 8.23,\n" // split
            + "        \"doubleField\": 9.56,\n" // split
            + "        \"stringField\": \"another test string field\"\n" // split
            + "      }\n" // split
            + "    ]\n" // split
            + "  }\n" // split
            + "}";

    private static final TestObject TEST_OBJECT_0 = new TestObject();

    private static final TestObject TEST_OBJECT_1 = new TestObject();

    private static final Date EXPECTED_DATE_JAVA;

    private static final org.joda.time.LocalDate EXPECTED_DATE_JODA;

    private static final SimpleDateFormat ISO8601_DATETIME_FORMAT_JAVA = new SimpleDateFormat(
            "yyyy-MM-dd'T'HH:mm:ss.SSSX", Locale.getDefault());

    private static final String DATE_STR = "2019-01-21";
    private static final String DATETIME_OFFSET_STR = "2019-01-21T12:28:31.234+00:00";

    static {
        TEST_OBJECT_0.booleanField = true;
        TEST_OBJECT_0.shortField = 1;
        TEST_OBJECT_0.intField = 2;
        TEST_OBJECT_0.longField = 3L;
        TEST_OBJECT_0.floatField = 1.23F;
        TEST_OBJECT_0.doubleField = 4.56D;
        TEST_OBJECT_0.stringField = "a test string field";

        TEST_OBJECT_1.booleanField = false;
        TEST_OBJECT_1.shortField = 4;
        TEST_OBJECT_1.intField = 5;
        TEST_OBJECT_1.longField = 6L;
        TEST_OBJECT_1.floatField = 8.23F;
        TEST_OBJECT_1.doubleField = 9.56D;
        TEST_OBJECT_1.stringField = "another test string field";

        try {
            EXPECTED_DATE_JAVA = ISO8601_DATETIME_FORMAT_JAVA.parse("2019-01-16T14:02:00.000Z");
            EXPECTED_DATE_JODA = org.joda.time.LocalDate.parse("2019-01-16");
        } catch (ParseException | IllegalArgumentException e) {
            IllegalArgumentException t = e instanceof IllegalArgumentException // split
                    ? (IllegalArgumentException) e
                    : new IllegalArgumentException("Unable to parse date string", e);
            throw t;
        }
    }

    private JsonCache.Root root;
    private JsonCache cache;

    private void reload() throws CacheException, UnsupportedEncodingException {
        root.unload();
        root.load(new ByteArrayInputStream(JSON.getBytes(StandardCharsets.UTF_8)));
    }

    @BeforeMethod(alwaysRun = true)
    public void setUp() throws Exception {
        // NOTE: we want each test to have its own pristine cache instance.
        root = JsonCache.Factory.instance.create();
        ObjectMapper mapper = root.getMapper();
        mapper.registerModule(new JavaTimeModule());
        mapper.registerModule(new JodaModule());
        mapper.registerModule(new ThreeTenModule());
        cache = root.child("/JsonCacheTest");
        reload();
    }

    @Test
    public void testAddBigDecimal() throws Exception {
        BigDecimal value = new BigDecimal(5.1);
        cache.add("/array/1", value);
        assertEquals(5, cache.size("/array"), "Array size incorrect after add(path, value)");
        assertEquals(1, cache.get("/array/0"), "Array element at index 0 incorrect after add(path, value);");
        assertEquals(value, cache.getBigDecimal("/array/1"),
                "Array element at index 1 incorrect after add(path, value);");
        assertEquals("2", cache.get("/array/2"), "Array element at index 2 incorrect after add(path, value);");

        value = new BigDecimal(6.2);
        cache.add("/object/nestedArray/5", value);
        assertEquals(6, cache.size("/object/nestedArray"), "Array size incorrect after add(path, value)");
        assertEquals(null, cache.get("/object/nestedArray/4"),
                "Array element at index 4 incorrect after add(path, value);");
        assertEquals(value, cache.getBigDecimal("/object/nestedArray/5"),
                "Array element at index 1 incorrect after add(path, value);");
    }

    @Test
    public void testAddBigInteger() throws Exception {
        BigInteger value = new BigInteger("5");
        cache.add("/array/1", value);
        assertEquals(5, cache.size("/array"), "Array size incorrect after add(path, value)");
        assertEquals(1, cache.get("/array/0"), "Array element at index 0 incorrect after add(path, value);");
        assertEquals(value, cache.getBigInteger("/array/1"),
                "Array element at index 1 incorrect after add(path, value);");
        assertEquals("2", cache.get("/array/2"), "Array element at index 2 incorrect after add(path, value);");

        value = new BigInteger("6");
        cache.add("/object/nestedArray/5", value);
        assertEquals(6, cache.size("/object/nestedArray"), "Array size incorrect after add(path, value)");
        assertEquals(null, cache.get("/object/nestedArray/4"),
                "Array element at index 4 incorrect after add(path, value);");
        assertEquals(value, cache.getBigInteger("/object/nestedArray/5"),
                "Array element at index 1 incorrect after add(path, value);");
    }

    @Test
    public void testAddBoolean() throws Exception {
        cache.add("/array/1", true);
        assertEquals(5, cache.size("/array"), "Array size incorrect after add(path, value)");
        assertEquals(1, cache.get("/array/0"), "Array element at index 0 incorrect after add(path, value);");
        assertEquals(true, cache.getBoolean("/array/1"), "Array element at index 1 incorrect after add(path, value);");
        assertEquals("2", cache.get("/array/2"), "Array element at index 2 incorrect after add(path, value);");

        cache.add("/object/nestedArray/5", true);
        assertEquals(6, cache.size("/object/nestedArray"), "Array size incorrect after add(path, value)");
        assertEquals(null, cache.get("/object/nestedArray/4"),
                "Array element at index 4 incorrect after add(path, value);");
        assertEquals(true, cache.getBoolean("/object/nestedArray/5"),
                "Array element at index 1 incorrect after add(path, value);");
    }

    @Test
    public void testAddDate() throws Exception {
        testAddDate0(DATETIME_OFFSET_STR, ISO8601_DATETIME_FORMAT_JAVA.parse(DATETIME_OFFSET_STR));
    }

    private void testAddDate0(String dateStr, Object date) throws Exception {
        cache.add("/array/1", dateStr);
        assertEquals(5, cache.size("/array"), "Array size incorrect after add(path, value)");
        assertEquals(1, cache.get("/array/0"), "Array element at index 0 incorrect after add(path, value);");
        assertEquals(date, cache.getObject("/array/1", date.getClass()),
                "Array element at index 1 incorrect after add(path, value);");
        assertEquals("2", cache.get("/array/2"), "Array element at index 2 incorrect after add(path, value);");

        cache.add("/object/nestedArray/5", date);
        assertEquals(6, cache.size("/object/nestedArray"), "Array size incorrect after add(path, value)");
        assertEquals(null, cache.get("/object/nestedArray/4"),
                "Array element at index 4 incorrect after add(path, value);");
        assertEquals(date, cache.getObject("/object/nestedArray/5", date.getClass()),
                "Array element at index 1 incorrect after add(path, value);");
    }

    @Test
    public void testAddDateTimeJava8() throws Exception {
        testAddDate0(DATETIME_OFFSET_STR, java.time.OffsetDateTime.parse(DATETIME_OFFSET_STR));
    }

    @Test
    public void testAddDateTimeJoda() throws Exception {
        testAddDate0(DATETIME_OFFSET_STR, org.joda.time.DateTime.parse(DATETIME_OFFSET_STR));
    }

    @Test
    public void testAddDateTimeThreeTen() throws Exception {
        testAddDate0(DATETIME_OFFSET_STR, org.threeten.bp.OffsetDateTime.parse(DATETIME_OFFSET_STR));
    }

    @Test
    public void testAddDouble() throws Exception {
        cache.add("/array/1", 5.1D);
        assertEquals(5, cache.size("/array"), "Array size incorrect after add(path, value)");
        assertEquals(1, cache.get("/array/0"), "Array element at index 0 incorrect after add(path, value);");
        assertEquals(5.1D, cache.getDouble("/array/1"), Double.MIN_VALUE,
                "Array element at index 1 incorrect after add(path, value);");
        assertEquals("2", cache.get("/array/2"), "Array element at index 2 incorrect after add(path, value);");

        cache.add("/object/nestedArray/5", 6.2D);
        assertEquals(6, cache.size("/object/nestedArray"), "Array size incorrect after add(path, value)");
        assertEquals(null, cache.get("/object/nestedArray/4"),
                "Array element at index 4 incorrect after add(path, value);");
        assertEquals(6.2D, cache.getDouble("/object/nestedArray/5"),
                Double.MIN_VALUE, "Array element at index 1 incorrect after add(path, value);");
    }

    @Test
    public void testAddFloat() throws Exception {
        cache.add("/array/1", 5.1F);
        assertEquals(5, cache.size("/array"), "Array size incorrect after add(path, value)");
        assertEquals(1, cache.get("/array/0"), "Array element at index 0 incorrect after add(path, value);");
        assertEquals(5.1F, cache.getFloat("/array/1"), Float.MIN_VALUE,
                "Array element at index 1 incorrect after add(path, value);");
        assertEquals("2", cache.get("/array/2"), "Array element at index 2 incorrect after add(path, value);");

        cache.add("/object/nestedArray/5", 6.2F);
        assertEquals(6, cache.size("/object/nestedArray"), "Array size incorrect after add(path, value)");
        assertEquals(null, cache.get("/object/nestedArray/4"),
                "Array element at index 4 incorrect after add(path, value);");
        assertEquals(6.2F, cache.getFloat("/object/nestedArray/5"),
                Float.MIN_VALUE, "Array element at index 1 incorrect after add(path, value);");
    }

//    private void assertEquals(String string, float f, float float1, float minValue) {
//        // TODO Auto-generated method stub
//
//    }

    @Test
    public void testAddInt() throws Exception {
        cache.add("/array/1", 5);
        assertEquals(5, cache.size("/array"), "Array size incorrect after add(path, value)");
        assertEquals(1, cache.get("/array/0"), "Array element at index 0 incorrect after add(path, value);");
        assertEquals(5, cache.getInt("/array/1"), "Array element at index 1 incorrect after add(path, value);");
        assertEquals("2", cache.get("/array/2"), "Array element at index 2 incorrect after add(path, value);");

        cache.add("/object/nestedArray/5", 6);
        assertEquals(6, cache.size("/object/nestedArray"), "Array size incorrect after add(path, value)");
        assertEquals(null, cache.get("/object/nestedArray/4"),
                "Array element at index 4 incorrect after add(path, value);");
        assertEquals(6, cache.getInt("/object/nestedArray/5"),
                "Array element at index 1 incorrect after add(path, value);");
    }

    @Test
    public void testAddLocalDateJava8() throws Exception {
        testAddDate0(DATE_STR, java.time.LocalDate.parse(DATE_STR));
    }

    @Test
    public void testAddLocalDateJoda() throws Exception {
        testAddDate0(DATE_STR, org.joda.time.LocalDate.parse(DATE_STR));
    }

    @Test
    public void testAddLocalDateThreeTen() throws Exception {
        testAddDate0(DATE_STR, org.threeten.bp.LocalDate.parse(DATE_STR));
    }

    @Test
    public void testAddLong() throws Exception {
        cache.add("/array/1", 5L);
        assertEquals(5, cache.size("/array"), "Array size incorrect after add(path, value)");
        assertEquals(1, cache.get("/array/0"), "Array element at index 0 incorrect after add(path, value);");
        assertEquals(5L, cache.getLong("/array/1"), "Array element at index 1 incorrect after add(path, value);");
        assertEquals("2", cache.get("/array/2"), "Array element at index 2 incorrect after add(path, value);");

        cache.add("/object/nestedArray/5", 6L);
        assertEquals(6, cache.size("/object/nestedArray"), "Array size incorrect after add(path, value)");
        assertEquals(null, cache.get("/object/nestedArray/4"),
                "Array element at index 4 incorrect after add(path, value);");
        assertEquals(6L, cache.getLong("/object/nestedArray/5"),
                "Array element at index 1 incorrect after add(path, value);");
    }

    public void testAddObject() throws Exception {
        cache.add("/array/1", TEST_OBJECT_0);
        assertEquals(5, cache.size("/array"), "Array size incorrect after add(path, value)");
        assertEquals(1, cache.get("/array/0"), "Array element at index 0 incorrect after add(path, value);");
        assertEquals(TEST_OBJECT_0, cache.getObject("/array/1", TestObject.class),
                "Array element at index 1 incorrect after add(path, value);");
        assertEquals("2", cache.get("/array/2"), "Array element at index 2 incorrect after add(path, value);");

        cache.add("/object/nestedArray/5", TEST_OBJECT_1);
        assertEquals(6, cache.size("/object/nestedArray"), "Array size incorrect after add(path, value)");
        assertEquals(null, cache.get("/object/nestedArray/4"),
                "Array element at index 4 incorrect after add(path, value);");
        assertEquals(TEST_OBJECT_1, cache.getObject("/object/nestedArray/5", TestObject.class),
                "Array element at index 1 incorrect after add(path, value);");
    }

    @Test
    public void testAddShort() throws Exception {
        cache.add("/array/1", (short) 5);
        assertEquals(5, cache.size("/array"), "Array size incorrect after add(path, value)");
        assertEquals(1, cache.get("/array/0"), "Array element at index 0 incorrect after add(path, value);");
        assertEquals((short) 5, cache.getShort("/array/1"),
                "Array element at index 1 incorrect after add(path, value);");
        assertEquals("2", cache.get("/array/2"), "Array element at index 2 incorrect after add(path, value);");

        cache.add("/object/nestedArray/5", (short) 6);
        assertEquals(6, cache.size("/object/nestedArray"), "Array size incorrect after add(path, value)");
        assertEquals(null, cache.get("/object/nestedArray/4"),
                "Array element at index 4 incorrect after add(path, value);");
        assertEquals((short) 6, cache.getShort("/object/nestedArray/5"),
                "Array element at index 1 incorrect after add(path, value);");
    }

    @Test
    public void testAddString() throws Exception {
        cache.add("/array/1", "5");
        assertEquals(5, cache.size("/array"), "Array size incorrect after add(path, value)");
        assertEquals(1, cache.get("/array/0"), "Array element at index 0 incorrect after add(path, value);");
        assertEquals("5", cache.getString("/array/1"), "Array element at index 1 incorrect after add(path, value);");
        assertEquals("2", cache.get("/array/2"), "Array element at index 2 incorrect after add(path, value);");

        cache.add("/object/nestedArray/5", "6");
        assertEquals(6, cache.size("/object/nestedArray"), "Array size incorrect after add(path, value)");
        assertEquals(null, cache.get("/object/nestedArray/4"),
                "Array element at index 4 incorrect after add(path, value);");
        assertEquals("6", cache.getString("/object/nestedArray/5"),
                "Array element at index 1 incorrect after add(path, value);");
    }

    @Test
    public void testDelete() throws Exception {
        assertTrue(cache.get("") instanceof ObjectNode, "existing root element is not an object;");

        cache.delete("/array/2");
        assertEquals(3, cache.size("/array"), "Array size incorrect after delete element;");
        assertEquals("2", cache.getString("/array/1"), "Array element at index 1 incorrect after delete;");
        assertEquals(4.0D, cache.getDouble("/array/2"), Double.MIN_VALUE,
                "Array element at index 2 incorrect after delete;");

        cache.delete("/object/nestedArray/2");
        assertEquals(3, cache.size("/object/nestedArray"), "Array size incorrect after delete element;");
        assertEquals("b", cache.getString("/object/nestedArray/1"), "Array element at index 1 incorrect after delete;");
        assertEquals("d", cache.getString("/object/nestedArray/2"), "Array element at index 2 incorrect after delete;");

        cache.delete("/object");
        assertFalse(cache.exists("/object"), "delete object failed;");

        cache.delete("");
        assertFalse(cache.exists(""), "delete root failed;");

        // This call should have reinstated the missing root as an array.
        cache.set("/0", true);
        assertTrue(cache.get("") instanceof ArrayNode, "new root element is not an array;");
        assertEquals(true, cache.getBoolean("/0"), "Failed to set root array element 0 when root is null;");

        cache.set("/1", 3.14);
        assertEquals(3.14, cache.getDouble("/1"), Double.MIN_VALUE, "Failed to set root array element 1;");

        cache.set("/2", "a string element");
        assertEquals("a string element", cache.getString("/2"), "Failed to set root array element 2;");

        // Now that root has been reallocated as an array, try and set a non-integer property on it.
        try {
            cache.set("/astring", "won't work!");
            fail("attempting to set a non-integer property on an array should have failed");
        } catch (NumberFormatException e) {
            assertFalse(cache.exists("/astring"), "setting a non-numeric property on an array worked!;");
        }
    }


    @Test
    public void testFlushSaveLoad() throws Exception {
        ByteArrayOutputStream out = new ByteArrayOutputStream();

        root.flush(out);
        assertTrue(out.size() == 0, "non-dirty flush() should not have written bytes;");
        assertFalse(root.isDirty(), "cache should not be dirty after no-op flush();");

        root.save(out);
        assertTrue(out.size() > 0, "save() wrote no bytes;");
        assertFalse(root.isDirty(), "cache should not be dirty after save();");

        root.delete("");
        assertFalse(cache.exists(""), "cache not empty;");

        root.unload();
        assertFalse(root.isDirty(), "cache should not be dirty after unload();");

        root.load(new ByteArrayInputStream(out.toByteArray()));
        assertFalse(root.isDirty(), "cache should not be dirty after load();");

        testGet();

        assertFalse(root.isDirty(), "cache should not be dirty after reads;");
    }

    @Test
    public void testGet() throws Exception {
        assertEquals(true, cache.get("/boolean"), "boolean get() returned incorrect result;");
        assertEquals(3.14, cache.get("/number"), "number get() returned incorrect result;");
        assertEquals("a string", cache.get("/string"), "string get() returned incorrect result;");
        Object array = cache.get("/array");
        assertNotNull(array, "array get() returned null;");
        assertTrue(array instanceof ArrayNode, "array get() returned incorrect type;");
        assertEquals(4, cache.size("/array"), "array size() returned incorrect result;");
        assertEquals(1, cache.get("/array/0"), "array get element 0 returned incorrect result;");
        assertEquals(JsonNodeType.NUMBER, cache.getNodeType("/array/0"),
                "array get element 2 returned incorrect result;");
        assertEquals("2", cache.get("/array/1"), "array get element 1 returned incorrect result;");
        assertEquals(3.0, cache.get("/array/2"), "array get element 2 returned incorrect result;");
        assertEquals(JsonNodeType.NUMBER, cache.getNodeType("/array/2"),
                "array get element 2 returned incorrect result;");
        assertEquals(4.0D, cache.get("/array/3"), "array get element 3 returned incorrect result;");
        Object object = cache.get("/object");
        assertNotNull(object, "object get() returned null;");
        assertTrue(object instanceof ObjectNode, "object get() returned incorrect type;");

        assertEquals(true, cache.get("/object/nestedBoolean"), "nested boolean get() returned incorrect result;");
        assertEquals(2.72, cache.get("/object/nestedNumber"), "nested number get() returned incorrect result;");
        assertEquals("a nested string", cache.get("/object/nestedString"),
                "nested string get() returned incorrect result;");
        Object nestedArray = cache.get("/object/nestedArray");
        assertNotNull(nestedArray, "nested array get() returned null;");
        assertTrue(nestedArray instanceof ArrayNode, "nested array get() returned incorrect type;");
        assertEquals(4, cache.size("/object/nestedArray"), "nested array size() returned incorrect result;");
        assertEquals("a", cache.get("/object/nestedArray/0"), "nested array get element returned incorrect result;");
        assertEquals("d", cache.get("/object/nestedArray/3"), "nested array get element returned incorrect result;");
        Object nestedObject = cache.get("/object/nestedObject");
        assertNotNull(nestedObject, "object get() returned null;");
        assertTrue(nestedObject instanceof ObjectNode, "object get() returned incorrect type;");

        assertEquals(true, cache.get("/testObjects/0/booleanField"), "test object 0 booleanField is incorrect;");
        assertEquals(1, cache.get("/testObjects/0/shortField"), "test object 0 shortField is incorrect;");
        assertEquals(2, cache.get("/testObjects/0/intField"), "test object 0 intField is incorrect;");
        assertEquals(3, cache.get("/testObjects/0/longField"), "test object 0 longField is incorrect;");
        assertEquals(1.23, cache.get("/testObjects/0/floatField"), "test object 0 floatField is incorrect;");
        assertEquals(4.56, cache.get("/testObjects/0/doubleField"), "test object 0 doubleField is incorrect;");
        assertEquals("a test string field", cache.get("/testObjects/0/stringField"),
                "test object 0 stringField is incorrect;");

        assertEquals(false, cache.get("/testObjects/1/booleanField"), "test object 1 booleanField is incorrect;");
        assertEquals(4, cache.get("/testObjects/1/shortField"), "test object 1 shortField is incorrect;");
        assertEquals(5, cache.get("/testObjects/1/intField"), "test object 1 intField is incorrect;");
        assertEquals(6, cache.get("/testObjects/1/longField"), "test object 1 longField is incorrect;");
        assertEquals(8.23, cache.get("/testObjects/1/floatField"), "test object 1 floatField is incorrect;");
        assertEquals(9.56, cache.get("/testObjects/1/doubleField"), "test object 1 doubleField is incorrect;");
        assertEquals("another test string field", cache.get("/testObjects/1/stringField"),
                "test object 1 stringField is incorrect;");
    }

    @Test
    public void testGetBigDecimal() throws Exception {
        assertEquals(new BigDecimal("3.14"), cache.getBigDecimal("/number"),
                "getBigDecimal(path) returned incorrect result;");
        assertEquals(new BigDecimal("2.72"), cache.getBigDecimal("/object/nestedNumber"),
                "getBigDecimal(nestedPath) returned incorrect result;");
        assertFalse(root.isDirty(), "cache should not be dirty after getBigDecimal(path);");
    }

    @Test
    public void testGetBigDecimalWithDefault() throws Exception {
        BigDecimal bd162 = new BigDecimal("1.62");
        BigDecimal bd272 = new BigDecimal("2.72");
        BigDecimal bd314 = new BigDecimal("3.14");
        BigDecimal bd628 = new BigDecimal("3.14");

        assertEquals(bd314, cache.getBigDecimal("/number", bd162),
                "getBigDecimal(path, default) returned incorrect result;");
        assertEquals(bd272, cache.getBigDecimal("/object/nestedNumber", bd628),
                "getBigDecimal(nestedPath, default) returned incorrect result;");
        assertFalse(root.isDirty(), "cache should not be dirty after getBigDecimal(path, default);");

        assertEquals(bd162, cache.getBigDecimal("/nonExistentNumber", bd162),
                "getBigDecimal(nonExistentPath, default) returned incorrect result;");
        assertEquals(bd628, cache.getBigDecimal("/object/nonExistentNestedNumber", bd628),
                "nested getBigDecimal(nonExistentNestedPath, default) returned incorrect result;");
        assertTrue(root.isDirty(), "cache should be dirty after getBigDecimal(nonExistentPath, default);");

        assertEquals(bd162, cache.getBigDecimal("/nonExistentNumber"),
                "getBigDecimal(path) returned incorrect result after update;");
        assertEquals(bd628, cache.getBigDecimal("/object/nonExistentNestedNumber"),
                "nested getBigDecimal(path) returned incorrect result after update;");
    }

    @Test
    public void testGetBigInteger() throws Exception {
        BigInteger bi2 = new BigInteger("2");
        BigInteger bi3 = new BigInteger("3");

        assertEquals(bi3, cache.getBigInteger("/number"), "getBigInteger(path) returned incorrect result;");
        assertEquals(bi2, cache.getBigInteger("/object/nestedNumber"),
                "getBigInteger(nestedPath) returned incorrect result;");
        assertFalse(root.isDirty(), "cache should not be dirty after getBigInteger(path);");
    }

    @Test
    public void testGetBigIntegerWithDefault() throws Exception {
        BigInteger bi2 = new BigInteger("2");
        BigInteger bi3 = new BigInteger("3");
        BigInteger bi4 = new BigInteger("4");
        BigInteger bi5 = new BigInteger("5");

        assertEquals(bi3, cache.getBigInteger("/number", bi4),
                "getBigInteger(path, default) returned incorrect result;");
        assertEquals(bi2, cache.getBigInteger("/object/nestedNumber", bi5),
                "getBigInteger(nestedPath, default) returned incorrect result;");
        assertFalse(root.isDirty(), "cache should not be dirty after getBigInteger(path, default);");

        assertEquals(bi4, cache.getBigInteger("/nonExistentNumber", bi4),
                "getBigInteger(nonExistentPath, default) returned incorrect result;");
        assertEquals(bi5, cache.getBigInteger("/object/nonExistentNestedNumber", bi5),
                "getBigInteger(nonExistentNestedPath, default) returned incorrect result;");
        assertTrue(root.isDirty(), "cache should be dirty after getBigInteger(nonExistentPath, default);");

        assertEquals(bi4, cache.getBigInteger("/nonExistentNumber"),
                "getBigInteger(path) returned incorrect result after update;");
        assertEquals(bi5, cache.getBigInteger("/object/nonExistentNestedNumber"),
                "getBigInteger(nestedPath) returned incorrect result after update;");
    }

    @Test
    public void testGetBoolean() throws Exception {
        assertEquals(true, cache.getBoolean("/boolean"), "getBoolean(path) returned incorrect result;");
        assertEquals(true, cache.getBoolean("/object/nestedBoolean"),
                "getBoolean(nestedPath) returned incorrect result;");
        assertFalse(root.isDirty(), "cache should not be dirty after getBoolean(path);");
    }

    @Test
    public void testGetBooleanWithDefault() throws Exception {
        assertEquals(true, cache.getBoolean("/boolean", false), "getBoolean(path, default) returned incorrect result;");
        assertEquals(true, cache.getBoolean("/object/nestedBoolean", false),
                "getBoolean(nestedPath, default) returned incorrect result;");
        assertFalse(root.isDirty(), "cache should not be dirty after getBoolean(path, default);");

        assertEquals(true, cache.getBoolean("/nonExistentBoolean", true),
                "getBoolean(nonExistentPath, default) returned incorrect result;");
        assertEquals(true, cache.getBoolean("/object/nonExistentNestedBoolean", true),
                "getBoolean(nonExistentNestedPath, default) returned incorrect result;");
        assertTrue(root.isDirty(), "cache should be dirty after getBoolean(nonExistentPath, default);");

        assertEquals(true, cache.getBoolean("/nonExistentBoolean"),
                "getBoolean(nonExistentPath, default) returned incorrect result;");
        assertEquals(true, cache.getBoolean("/object/nonExistentNestedBoolean"),
                "getBoolean(nonExistentNestedPath, default) returned incorrect result;");
    }

    @Test
    public void testGetDate() throws Exception {
        assertEquals(EXPECTED_DATE_JAVA, cache.getObject("/dateTimeZ", Date.class),
                "getDate(path) returned incorrect result;");
        assertEquals(EXPECTED_DATE_JAVA, cache.getObject("/object/nestedDateTimeZ", Date.class),
                "getDate(nestedPath) returned incorrect result;");
        assertFalse(root.isDirty(), "cache should not be dirty after getDate(path);");
    }

    @Test
    public void testGetDateWithDefault() throws Exception {
        Date defaultDate = new Date();
        assertEquals(EXPECTED_DATE_JAVA, cache.getObject("/dateTimeZ", defaultDate),
                "getDate(path, default) returned incorrect result;");
        assertEquals(EXPECTED_DATE_JAVA, cache.getObject("/object/nestedDateTimeZ", defaultDate),
                "getDate(nestedPath, default) returned incorrect result;");
        assertFalse(root.isDirty(), "cache should not be dirty after getDate(path, default);");

        assertEquals(defaultDate, cache.getObject("/nonExistentDate", defaultDate),
                "getDate(nonExistentPath, default) returned incorrect result;");
        assertEquals(defaultDate, cache.getObject("/object/nonExistentNestedDate", defaultDate),
                "getDate(nonExistentNestedPath, default) returned incorrect result;");
        assertTrue(root.isDirty(), "cache should be dirty after getDate(nonExistentPath, default);");

        assertEquals(defaultDate, cache.getObject("/nonExistentDate", Date.class),
                "getDate(path) returned incorrect result after update;");
        assertEquals(defaultDate, cache.getObject("/object/nonExistentNestedDate", Date.class),
                "getDate(nestedPath) returned incorrect result after update;");
    }

    @Test
    public void testGetDouble() throws Exception {
        assertEquals(3.14D, cache.getDouble("/number"), Double.MIN_VALUE, "getDouble(path) returned incorrect result;");
        assertEquals(2.72D, cache.getDouble("/object/nestedNumber"), Double.MIN_VALUE,
                "getDouble(nestedPath) returned incorrect result;");
        assertFalse(root.isDirty(), "cache should not be dirty after getDouble(path);");
    }

    @Test
    public void testGetDoubleWithDefault() throws Exception {
        assertEquals(3.14D, cache.getDouble("/number", 7.77D), Double.MIN_VALUE,
                "getDouble(path, default) returned incorrect result;");
        assertEquals(2.72D, cache.getDouble("/object/nestedNumber", 8.88D),
                Double.MIN_VALUE, "getDouble(nestedPath, default) returned incorrect result;");
        assertFalse(root.isDirty(), "cache should not be dirty after getDouble(path, default);");

        assertEquals(7.77D, cache.getDouble("/nonExistentNumber", 7.77D),
                Double.MIN_VALUE, "getDouble(nonExistentPath, default) returned incorrect result;");
        assertEquals(8.88D, cache.getDouble("/object/nonExistentNestedNumber", 8.88D),
                Double.MIN_VALUE, "getDouble(nonExistentNestedPath, default) returned incorrect result;");
        assertTrue(root.isDirty(), "cache should be dirty after getDouble(nonExistentPath, default);");

        assertEquals(7.77D, cache.getDouble("/nonExistentNumber"),
                Double.MIN_VALUE, "getDouble(path) returned incorrect result after update;");
        assertEquals(8.88D, cache.getDouble("/object/nonExistentNestedNumber"),
                Double.MIN_VALUE, "getDouble(nestedPath) returned incorrect result after update;");
    }

    @Test
    public void testGetFloat() throws Exception {
        assertEquals(3.14F, cache.getFloat("/number"), Float.MIN_VALUE, "getFloat(path) returned incorrect result;");
        assertEquals(2.72F, cache.getFloat("/object/nestedNumber"),
                Float.MIN_VALUE, "nested getFloat(nestedPath) returned incorrect result;");
        assertFalse(root.isDirty(), "cache should not be dirty after getFloat(path);");
    }

    @Test
    public void testGetFloatWithDefault() throws Exception {
        assertEquals(3.14F, cache.getFloat("/number", 7.77F), Float.MIN_VALUE,
                "getFloat(path, default) returned incorrect result;");
        assertEquals(2.72F, cache.getFloat("/object/nestedNumber", 8.88F),
                Float.MIN_VALUE, "nested getFloat(nestedPath, default) returned incorrect result;");
        assertFalse(root.isDirty(), "cache should not be dirty after getFloat(path, default);");

        assertEquals(7.77F, cache.getFloat("/nonExistentNumber", 7.77F),
                Float.MIN_VALUE, "getFloat(nonExistentPath, default) returned incorrect result;");
        assertEquals(8.88F, cache.getFloat("/object/nonExistentNestedNumber", 8.88F),
                Float.MIN_VALUE, "nested getFloat(nonExistentNestedPath, default) returned incorrect result;");
        assertTrue(root.isDirty(), "cache should be dirty after getFloat(nonExistentPath, default);");

        assertEquals(7.77F, cache.getFloat("/nonExistentNumber"),
                Float.MIN_VALUE, "getFloat(path) returned incorrect result after update;");
        assertEquals(8.88F, cache.getFloat("/object/nonExistentNestedNumber"),
                Float.MIN_VALUE, "nested getFloat(nestedPath) returned incorrect result after update;");
    }

    @Test
    public void testGetInt() throws Exception {
        assertEquals(3, cache.getInt("/number"), "getInt(path) returned incorrect result;");
        assertEquals(2, cache.getInt("/object/nestedNumber"), "getInt(nestedPath) returned incorrect result;");
        assertFalse(root.isDirty(), "cache should not be dirty after getInt(path);");
    }

    @Test
    public void testGetIntWithDefault() throws Exception {
        assertEquals(3, cache.getInt("/number", 5), "getInt(path, default) returned incorrect result;");
        assertEquals(4, cache.getInt("/object/nestedNumber"), 2,
                "getInt(nestedPath, default) returned incorrect result;");
        assertFalse(root.isDirty(), "cache should not be dirty after getInt(path, default);");

        assertEquals(5, cache.getInt("/nonExistentNumber", 5),
                "getInt(nonExistentPath, default) returned incorrect result;");
        assertEquals(4, cache.getInt("/object/nonExistentNestedNumber", 4),
                "getInt(nonExistentNestedPath, default) returned incorrect result;");
        assertTrue(root.isDirty(), "cache should be dirty after getInt(nonExistentPath, default);");

        assertEquals(5, cache.getInt("/nonExistentNumber"), "getInt(path) returned incorrect result after update;");
        assertEquals(4, cache.getInt("/object/nonExistentNestedNumber"),
                "getInt(nestedPath) returned incorrect result after update;");
    }

    @Test
    public void testGetLocalDate() throws Exception {
        assertEquals(EXPECTED_DATE_JODA, cache.getObject("/date", org.joda.time.LocalDate.class),
                "getLocalDate(path) returned incorrect result;");
        assertFalse(root.isDirty(), "cache should not be dirty after getLocalDate(path);");
    }

    public void testGetLocalDateWithDefault() throws Exception {
        org.joda.time.LocalDate defaultDate = new org.joda.time.LocalDate();
        assertEquals(EXPECTED_DATE_JODA, cache.getObject("/dateTimeZ", defaultDate),
                "getLocalDate(path, default) returned incorrect result;");
        assertEquals(EXPECTED_DATE_JODA, cache.getObject("/object/nestedLocalDate", defaultDate),
                "getLocalDate(nestedPath, default) returned incorrect result;");
        assertFalse(root.isDirty(), "cache should not be dirty after getLocalDate(path, default);");

        assertEquals(defaultDate, cache.getObject("/nonExistentLocalDate", defaultDate),
                "getLocalDate(nonExistentPath, default) returned incorrect result;");
        assertEquals(defaultDate, cache.getObject("/object/nonExistentNestedLocalDate", defaultDate),
                "getLocalDate(nonExistentNestedPath, default) returned incorrect result;");
        assertTrue(root.isDirty(), "cache should be dirty after getLocalDate(nonExistentPath, default);");

        assertEquals(defaultDate, cache.getObject("/nonExistentLocalDate", org.joda.time.LocalDate.class),
                "getLocalDate(path) returned incorrect result after update;");
        assertEquals(defaultDate, cache.getObject("/object/nonExistentNestedLocalDate", org.joda.time.LocalDate.class),
                "getLocalDate(nestedPath) returned incorrect result after update;");
    }

    @Test
    public void testGetLong() throws Exception {
        assertEquals(3L, cache.getLong("/number"), "getLong(path) returned incorrect result;");
        assertEquals(2L, cache.getLong("/object/nestedNumber"), "getLong(nestedPath) returned incorrect result;");
        assertFalse(root.isDirty(), "cache should not be dirty after getLong(path);");
    }

    @Test
    public void testGetLongWithDefault() throws Exception {
        assertEquals(3L, cache.getLong("/number", 5L), "getLong(path, default) returned incorrect result;");
        assertEquals(2L, cache.getLong("/object/nestedNumber", 4L),
                "getLong(nestedPath, default) returned incorrect result;");
        assertFalse(root.isDirty(), "cache should not be dirty after getLong(path, default);");

        assertEquals(5L, cache.getLong("/nonExistentNumber", 5L),
                "getLong(nonExistentPath, default) returned incorrect result;");
        assertEquals(4L, cache.getLong("/object/nonExistentNestedNumber", 4L),
                "getLong(nonExistentNestedPath, default) returned incorrect result;");
        assertTrue(root.isDirty(), "cache should be dirty after getLong(nonExistentPath, default);");

        assertEquals(5L, cache.getLong("/nonExistentNumber"),
                "getLong(path, default) returned incorrect result after update;");
        assertEquals(4L, cache.getLong("/object/nonExistentNestedNumber"),
                "getLong(nestedPath, default) returned incorrect result after update;");
    }

    @Test
    public void testGetNumber() throws Exception {
        Number number = cache.getNumber("/number");
        assertTrue(number instanceof Double, "getNumber(path) returned incorrect type;");
        assertEquals(3.14D, number, "getNumber(path) returned incorrect result;");
        Number nestedNumber = cache.getNumber("/object/nestedNumber");
        assertTrue(nestedNumber instanceof Double, "getNumber(nestedPath) returned incorrect type;");
        assertEquals(2.72D, nestedNumber, "nested getNumber(nestedPath) returned incorrect result;");
        assertFalse(root.isDirty(), "cache should not be dirty after getNumber(path);");
    }

    @Test
    public void testGetNumberWithDefault() throws Exception {
        assertEquals(3.14D, cache.getNumber("/number", 7.77D), "getNumber(path, default) returned incorrect result;");
        assertEquals(2.72D, cache.getNumber("/object/nestedNumber", 8.88D),
                "getNumber(nestedPath, default) returned incorrect result;");
        assertFalse(root.isDirty(), "cache should not be dirty after getNumber(path, default);");

        assertEquals(7.77D, cache.getNumber("/nonExistentNumber", 7.77D),
                "getNumber(nonExistentPath, default) returned incorrect result;");
        assertEquals(8.88D, cache.getNumber("/object/nonExistentNestedNumber", 8.88D),
                "getNumber(nonExistentNestedPath, default) returned incorrect result;");
        assertTrue(root.isDirty(), "cache should be dirty after getNumber(nonExistentPath, default);");

        assertEquals(7.77D, cache.getNumber("/nonExistentNumber"),
                "getNumber(path, default) returned incorrect result after update;");
        assertEquals(8.88D, cache.getNumber("/object/nonExistentNestedNumber"),
                "getNumber(nestedPath, default) returned incorrect result after update;");
    }

    @Test
    public void testGetObject() throws Exception {
        TestObject testObject0 = cache.getObject("/testObjects/0", TestObject.class);
        assertEquals(TEST_OBJECT_0, testObject0, "getObject(path) returned incorrect result;");
        TestObject testObject1 = cache.getObject("/testObjects/1", TestObject.class);
        assertEquals(TEST_OBJECT_1, testObject1, "getObject(path) returned incorrect result;");
        assertFalse(root.isDirty(), "cache should not be dirty after getObject(path, type);");
    }

    @Test
    public void testGetObjects() throws Exception {
        List<TestObject> testObjects = cache.getObjects("/testObjects", TestObject.class);
        assertEquals(TEST_OBJECT_0, testObjects.get(0), "getObjects(path, type) returned incorrect result;");
        assertEquals(TEST_OBJECT_1, testObjects.get(1), "getObjects(path, type) returned incorrect result;");
        assertFalse(root.isDirty(), "cache should not be dirty after getObjects(path, type);");
    }

    @Test
    public void testGetObjectsWithDefault() throws Exception {
        List<TestObject> defaultValue = Arrays.asList(TEST_OBJECT_1, TEST_OBJECT_0);
        List<TestObject> testObjects = cache.getObjects("/testObjects", TestObject.class, defaultValue);
        assertEquals(TEST_OBJECT_0, testObjects.get(0),
                "getObjects(path, type, defaultValue) returned incorrect result;");
        assertEquals(TEST_OBJECT_1, testObjects.get(1),
                "getObjects(path, type, defaultValue) returned incorrect result;");
        assertFalse(root.isDirty(), "cache should not be dirty after getObjects(path, type, defaultValue);");

        testObjects = cache.getObjects("/nonExistentTestObjects", TestObject.class, defaultValue);
        assertEquals(TEST_OBJECT_1, testObjects.get(0),
                "getObjects(nonExistentPath, type, defaultValue) returned incorrect result;");
        assertEquals(TEST_OBJECT_0, testObjects.get(1),
                "getObjects(nonExistentPath, type, defaultValue) returned incorrect result;");
        assertTrue(root.isDirty(), "cache should be dirty after getObjects(nonExistentPath, type, defaultValue);");

        testObjects = cache.getObjects("/nonExistentTestObjects", TestObject.class);
        assertEquals(TEST_OBJECT_1, testObjects.get(0),
                "getObjects(path, type) returned incorrect result after update;");
        assertEquals(TEST_OBJECT_0, testObjects.get(1),
                "getObjects(path, type) returned incorrect result after update;");
    }

    @Test
    public void testGetObjectWithDefault() throws Exception {
        assertEquals(TEST_OBJECT_0, cache.getObject("/testObjects/0", TEST_OBJECT_1),
                "getObject(path, default) returned incorrect result;");
        assertFalse(root.isDirty(), "cache should not be dirty after getObject(path, default);");

        assertSame(TEST_OBJECT_1, cache.getObject("/testObjects/2", TEST_OBJECT_1),
                "getObject(nonExistentPath, default) returned incorrect result;");
        assertTrue(root.isDirty(), "cache should be dirty after getObject(nonExistentPath, default);");

        assertSame(TEST_OBJECT_1, cache.getObject("/testObjects/2", TestObject.class),
                "getObject(path, type) returned incorrect result after update;");
    }

    @Test
    public void testGetShort() throws Exception {
        assertEquals(3, cache.getShort("/number"), "getShort(path) returned incorrect result;");
        assertEquals(2, cache.getShort("/object/nestedNumber"),
                "nested getShort(nestedPath) returned incorrect result;");
        assertFalse(root.isDirty(), "cache should not be dirty after getShort(path);");
    }

    @Test
    public void testGetShortWithDefault() throws Exception {
        assertEquals(3, cache.getShort("/number", (short) 5), "getShort(path, default) returned incorrect result;");
        assertEquals(2, cache.getShort("/object/nestedNumber", (short) 4),
                "nested getShort(nestedPath, default) returned incorrect result;");
        assertFalse(root.isDirty(), "cache should not be dirty after getShort(path, default);");

        assertEquals(5, cache.getShort("/nonExistentNumber", (short) 5),
                "getShort(nonExistentPath, default) returned incorrect result;");
        assertEquals(4, cache.getShort("/object/nonExistentNestedNumber", (short) 4),
                "nested getShort(nonExistentPathNestedPath, default) returned incorrect result;");
        assertTrue(root.isDirty(), "cache should be dirty after getShort(nonExistentPath, default);");

        assertEquals(5, cache.getShort("/nonExistentNumber"), "getShort(path) returned incorrect result after update;");
        assertEquals(4, cache.getShort("/object/nonExistentNestedNumber"),
                "nested getShort(nestedPath) returned incorrect result after update;");
    }

    @Test
    public void testGetString() throws Exception {
        assertEquals("a string", cache.getString("/string"), "getString(path) returned incorrect result;");
        assertEquals("a nested string", cache.getString("/object/nestedString"),
                "getString(nestedPath) returned incorrect result;");
        assertFalse(root.isDirty(), "cache should not be dirty after getString(path);");
    }

    @Test
    public void testGetStringWithDefault() throws Exception {
        assertEquals("a string", cache.getString("/string", "a different string"),
                "getString(path, default) returned incorrect result;");
        assertEquals("a nested string", cache.getString("/object/nestedString", "a different nested string"),
                "getString(nestedPath, default) returned incorrect result;");
        assertFalse(root.isDirty(), "cache should not be dirty after getString(path, default);");

        assertEquals("a different string", cache.getString("/nonExistentString", "a different string"),
                "getString(nonExistentPath, default) returned incorrect result;");
        assertEquals("a different nested string",
                cache.getString("/object/nonExistentNestedString", "a different nested string"),
                "getString(nonExistentNestedPath, default) returned incorrect result;");
        assertTrue(root.isDirty(), "cache should be dirty after getString(nonExistentPath, default);");

        assertEquals("a different string", cache.getString("/nonExistentString"),
                "getString(path) returned incorrect result after update;");
        assertEquals("a different nested string", cache.getString("/object/nonExistentNestedString"),
                "getString(nestedPath) returned incorrect result after update;");
    }

    @Test
    public void testMerge() throws Exception {
        String incoming = "{\n" // split
                + "  \"JsonCacheTest\": {\n" // split
                + "    \"boolean\": false,\n" // modified
                + "    \"string\": \"a different string\",\n" // modified
                + "    \"anotherString\": \"another string\",\n" // added
                + "    \"array\": [1, \"2\", 3.0, 4.0, true, {}, []],\n" // elements added
                + "    \"object\": {\n" // split
                + "      \"nestedString\": \"a different nested string\",\n" // modified
                + "      \"nestedArray\": [\"a\", \"b\", \"e\", \"f\", \"g\"],\n" // elements modified & added
                + "      \"nestedObject\": {\n" // split
                + "        \"b\": \"baz\",\n" // modified
                + "        \"nestedNested\": {\"d\": \"waz\"}\n" // added
                + "      }\n" // split
                + "    }\n" // split
                + "  },\n" // split
                + "  \"JsonCacheMerge\": {" // added
                + "    \"number\": 10" // split
                + "  }" // split
                + "}";
        ByteArrayInputStream in = new ByteArrayInputStream(incoming.getBytes(StandardCharsets.UTF_8));

        root.mergePolicy(MergePolicy.NO_MERGE).load(in);
        assertFalse(root.isDirty(), "cache should not be dirty after second load() in NO_MERGE mode");
        assertEquals(1, root.size(""), "root size incorrect after second load() in NO_MERGE mode;");
        assertEquals(8, cache.size(""), "cache size incorrect after second load() in NO_MERGE mode;");
        testGet();

        in.reset();
        root.mergePolicy(MergePolicy.KEEP_EXISTING).load(in);
        assertTrue(root.isDirty(), "cache should be dirty after second load() in KEEP_EXISTING mode");
        assertEquals(2, root.size(""), "root size incorrect after second load() in KEEP_EXISTING mode;");
        assertEquals(8, cache.size(""), "cache size incorrect after second load() in KEEP_EXISTING mode;");
        assertFalse(cache.exists("/anotherString"), "descendent elements should not be merged in KEEP_EXISTING mode");
        assertFalse(cache.exists("/object/nestedObject/nestedNested"),
                "descendent elements should not be merged in KEEP_EXISTING mode");
        assertEquals(10, root.get("/JsonCacheMerge/number"), "added root property not merged in KEEP_EXISTING mode");
        assertEquals(4, cache.size("/array"), "added array elements merged in KEEP_EXISTING mode");
        assertEquals(4, cache.size("/object/nestedArray"), "added array elements merged in KEEP_EXISTING mode");
        testGet();

        reload();
        in.reset();
        root.mergePolicy(MergePolicy.MERGE_RECURSIVE).load(in);
        assertTrue(root.isDirty(), "cache should be dirty after second load() in MERGE_RECURSIVE mode");
        assertEquals(2, root.size(""), "root size incorrect after second load() in MERGE_RECURSIVE mode;");
        assertEquals(10, root.get("/JsonCacheMerge/number"), "added root property not merged in MERGE_RECURSIVE mode");
        assertEquals("another string", cache.get("/anotherString"),
                "added string property not merged in MERGE_RECURSIVE mode");
        assertEquals(7, cache.size("/array"), "added child array elements not merged in MERGE_RECURSIVE mode");
        assertEquals(JsonNodeType.BOOLEAN, cache.getNodeType("/array/4"),
                "added child array element 4 not merged in MERGE_RECURSIVE mode");
        assertEquals(JsonNodeType.OBJECT, cache.getNodeType("/array/5"),
                "added child array element 5 not merged in MERGE_RECURSIVE mode");
        assertEquals(JsonNodeType.ARRAY, cache.getNodeType("/array/6"),
                "added child array element 6 not merged in MERGE_RECURSIVE mode");
        assertEquals(Arrays.asList("a", "b", "c", "d", "e", "f", "g"),
                cache.getObjects("/object/nestedArray", String.class),
                "array not merged correctly in MERGE_RECURSIVE mode");
        assertEquals("waz", cache.get("/object/nestedObject/nestedNested/d"),
                "nested object not merged correctly in MERGE_RECURSIVE mode");

        reload();
        in.reset();
        root.mergePolicy(MergePolicy.OVERWRITE_EXISTING).load(in);
        assertTrue(root.isDirty(), "cache should be dirty after second load() in OVERWRITE_EXISTING mode");
        assertEquals(false, cache.get("/boolean"),
                "existing boolean not overwritten by load() in OVERWRITE_EXISTING mode;");
        assertFalse(cache.exists("/number"), "existing number not deleted by load() in OVERWRITE_EXISTING mode;");
        assertEquals("a different string", cache.get("/string"),
                "existing string not overwritten by load() in OVERWRITE_EXISTING mode;");
        assertEquals("another string", cache.get("/anotherString"),
                "added string not merged in OVERWRITE_EXISTING mode;");
        assertEquals(7, cache.size("/array"), "added array size incorrect in OVERWRITE_EXISTING mode;");
        assertEquals(3, cache.size("/object"), "added object size incorrect in OVERWRITE_EXISTING mode;");
        assertEquals("a different nested string", cache.get("/object/nestedString"),
                "added nested string incorrect in OVERWRITE_EXISTING mode;");
        assertEquals(5, cache.size("/object/nestedArray"),
                "added nested array size incorrect in OVERWRITE_EXISTING mode;");
        assertFalse(cache.exists("/object/nestedObject/a"),
                "added nested string incorrect in OVERWRITE_EXISTING mode;");
        assertEquals("baz", cache.get("/object/nestedObject/b"),
                "added nested string incorrect in OVERWRITE_EXISTING mode;");
        assertEquals("waz", cache.get("/object/nestedObject/nestedNested/d"),
                "added deep nested string incorrect in OVERWRITE_EXISTING mode;");
        assertEquals(10, root.get("/JsonCacheMerge/number"),
                "added root property not merged in OVERWRITE_EXISTING mode");
    }

    @Test
    public void testPojoSerialization() throws Exception {
        cache.set("/testObjects/0", TEST_OBJECT_0);
        cache.set("/testObjects/1", TEST_OBJECT_1);
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        root.save(out);
        cache.delete("");
        assertNull(cache.get(""), "root is not null;");

        root.unload();
        root.load(new ByteArrayInputStream(out.toByteArray()));

        testGet();

        assertFalse(root.isDirty(), "cache should not be dirty after reads;");
    }

    @Test
    public void testSet() throws Exception {
        cache.set("/boolean", false);
        assertEquals(false, cache.get("/boolean"), "boolean get() returned incorrect result;");

        cache.set("/number", 6.28);
        assertEquals(6.28, cache.get("/number"), "number get() returned incorrect result;");

        cache.set("/string", "another string");
        assertEquals("another string", cache.get("/string"), "string get() returned incorrect result;");

        Object array = cache.get("/array");
        assertNotNull(array, "array get() returned null;");
        assertTrue(array instanceof ArrayNode, "array get() returned incorrect type;");

        cache.set("/array/0", 2);
        assertEquals(2, cache.get("/array/0"), "array get element returned incorrect result;");
        cache.set("/array/3", 8.0D);
        assertEquals(8.0D, cache.get("/array/3"), "array get element returned incorrect result;");

        Object object = cache.get("/object");
        assertNotNull(object, "object get() returned null;");
        assertTrue(object instanceof ObjectNode, "object get() returned incorrect type;");

        cache.set("/object/nestedBoolean", false);
        assertEquals(false, cache.get("/object/nestedBoolean"), "nested boolean get() returned incorrect result;");

        cache.set("/object/nestedNumber", 1.618);
        assertEquals(1.618, cache.get("/object/nestedNumber"), "nested number get() returned incorrect result;");

        cache.set("/object/nestedString", "another nested string");
        assertEquals("another nested string", cache.get("/object/nestedString"),
                "nested string get() returned incorrect result;");

        Object nestedArray = cache.get("/object/nestedArray");
        assertNotNull(nestedArray, "nested array get() returned null;");
        assertTrue(nestedArray instanceof ArrayNode, "nested array get() returned incorrect type;");

        cache.set("/object/nestedArray/0", "x");
        assertEquals("x", cache.get("/object/nestedArray/0"), "nested array get element returned incorrect result;");
        cache.set("/object/nestedArray/3", "y");
        assertEquals("y", cache.get("/object/nestedArray/3"), "nested array get element returned incorrect result;");

        Object nestedObject = cache.get("/object/nestedObject");
        assertNotNull(nestedObject, "object get() returned null;");
        assertTrue(nestedObject instanceof ObjectNode, "object get() returned incorrect type;");

        assertTrue(root.isDirty(), "cache should be dirty after writes;");
    }

    @Test
    public void testSetBigDecimal() throws Exception {
        BigDecimal bd628 = new BigDecimal("6.28");
        cache.set("/number", bd628);
        assertEquals(bd628, cache.getBigDecimal("/number"), "getBigInteger() returned incorrect result;");

        BigDecimal bd1618 = new BigDecimal("1.618");
        cache.set("/object/nestedNumber", bd1618);
        assertEquals(bd1618, cache.getBigDecimal("/object/nestedNumber"),
                "nested getBigInteger() returned incorrect result;");

        assertTrue(root.isDirty(), "cache should be dirty after BigDecimal writes;");
    }

    @Test
    public void testSetBigInteger() throws Exception {
        BigInteger bi4 = new BigInteger("4");
        cache.set("/number", bi4);
        assertEquals(bi4, cache.getBigInteger("/number"), "getBigInteger() returned incorrect result;");

        BigInteger bi6 = new BigInteger("6");
        cache.set("/object/nestedNumber", bi6);
        assertEquals(bi6, cache.getBigInteger("/object/nestedNumber"),
                "nested getBigInteger() returned incorrect result;");

        assertTrue(root.isDirty(), "cache should be dirty after BigInteger writes;");
    }

    @Test
    public void testSetBoolean() throws Exception {
        cache.set("/boolean", false);
        assertEquals(false, cache.getBoolean("/boolean"), "getBoolean returned incorrect result;");

        cache.set("/object/nestedBoolean", false);
        assertEquals(false, cache.getBoolean("/object/nestedBoolean"), "nested getBoolean returned incorrect result;");

        assertTrue(root.isDirty(), "cache should be dirty after boolean writes;");
    }

    @Test
    public void testSetDate() throws Exception {
        Date date = new Date();
        cache.set("/dateTimeZ", date);
        assertEquals(date, cache.getObject("/dateTimeZ", Date.class), "getDate() returned incorrect result;");

        cache.set("/object/nestedDateTimeZ", date);
        assertEquals(date, cache.getObject("/object/nestedDateTimeZ", Date.class),
                "nested getDate() returned incorrect result;");

        assertTrue(root.isDirty(), "cache should be dirty after date writes;");
    }

    private void testSetDate0(String dateStr, Object date) throws CacheException {
        cache.set("/dateStr", dateStr);
        assertEquals(date,
                cache.getObject("/dateStr", date.getClass()), "getObject(\"/dateStr\", " + date.getClass().getName() + ".class) returned incorrect result;");
        cache.set("/date", date);
        assertEquals(date,
                cache.getObject("/dateStr", date.getClass()), "getObject(\"/date\", " + date.getClass().getName() + ".class) returned incorrect result;");
        assertTrue(root.isDirty(), "cache should be dirty after date writes;");
    }

    @Test
    public void testSetDateTime310() throws Exception {
        testSetDate0(DATETIME_OFFSET_STR, org.threeten.bp.OffsetDateTime.parse(DATETIME_OFFSET_STR));
    }

    @Test
    public void testSetDateTimeJava8() throws Exception {
        testSetDate0(DATETIME_OFFSET_STR, java.time.OffsetDateTime.parse(DATETIME_OFFSET_STR));
    }

    @Test
    public void testSetDateTimeJoda() throws Exception {
        testSetDate0(DATETIME_OFFSET_STR, org.joda.time.DateTime.parse(DATETIME_OFFSET_STR));
    }

    @Test
    public void testSetDouble() throws Exception {
        cache.set("/number", 6.28D);
        assertEquals(6.28D, cache.getDouble("/number"), Double.MIN_VALUE, "getDouble() returned incorrect result;");

        cache.set("/object/nestedNumber", 1.618D);
        assertEquals(1.618D, cache.getDouble("/object/nestedNumber"), Double.MIN_VALUE,
                "nested getDouble() returned incorrect result;");

        assertTrue(root.isDirty(), "cache should be dirty after double writes;");
    }

    @Test
    public void testSetFloat() throws Exception {
        cache.set("/number", 6.28F);
        assertEquals(6.28F, cache.getFloat("/number"), Float.MIN_VALUE, "getFloat() returned incorrect result;");

        cache.set("/object/nestedNumber", 1.618F);
        assertEquals(1.618F, cache.getFloat("/object/nestedNumber"), Float.MIN_VALUE,
                "nested getFloat() returned incorrect result;");

        assertTrue(root.isDirty(), "cache should be dirty after float writes;");
    }

    @Test
    public void testSetInt() throws Exception {
        cache.set("/number", 23);
        assertEquals(23, cache.getInt("/number"), "getInt() returned incorrect result;");

        cache.set("/object/nestedNumber", 22);
        assertEquals(22, cache.getInt("/object/nestedNumber"), "nested getInt() returned incorrect result;");

        assertTrue(root.isDirty(), "cache should be dirty after integer writes;");
    }

    @Test
    public void testSetLocalDate310() throws Exception {
        testSetDate0(DATE_STR, org.threeten.bp.LocalDate.parse(DATE_STR));
    }

    @Test
    public void testSetLocalDateJava8() throws Exception {
        testSetDate0(DATE_STR, java.time.LocalDate.parse(DATE_STR));
    }

    @Test
    public void testSetLocalDateJoda() throws Exception {
        testSetDate0(DATE_STR, org.joda.time.LocalDate.parse(DATE_STR));
    }

    @Test
    public void testSetLong() throws Exception {
        cache.set("/number", 33L);
        assertEquals(33L, cache.getLong("/number"), "getLong() returned incorrect result;");

        cache.set("/object/nestedNumber", 32L);
        assertEquals(32L, cache.getLong("/object/nestedNumber"), "nested getLong() returned incorrect result;");

        assertTrue(root.isDirty(), "cache should be dirty after long writes;");
    }

    @Test
    public void testSetNumber() throws Exception {
        cache.set("/number", 6.28F);
        assertEquals(6.28F, cache.getNumber("/number"), "getNumber() returned incorrect result;");

        cache.set("/object/nestedNumber", 1.618D);
        assertEquals(1.618D, cache.getNumber("/object/nestedNumber"), "nested getNumber() returned incorrect result;");

        assertTrue(root.isDirty(), "cache should be dirty after number writes;");
    }

    @Test
    public void testSetObject() throws Exception {
        cache.set("/testObjects/0", TEST_OBJECT_1);
        TestObject testObject0 = cache.getObject("/testObjects/0", TestObject.class);
        assertSame(TEST_OBJECT_1, testObject0, "test object 0 was not set correctly");
        assertNull(cache.get("/testObjects/0/booleanField"), "POJO field did not return null;");

        cache.set("/testObjects/1", TEST_OBJECT_0);
        TestObject testObject1 = cache.getObject("/testObjects/1", TestObject.class);
        assertSame(TEST_OBJECT_0, testObject1, "test object 1 was not deserialized correctly");
        assertNull(cache.get("/testObjects/1/stringField"), "POJO field did not return null;");

        cache.set("/boolean", Boolean.FALSE);
        assertEquals(false, cache.getBoolean("/boolean"), "set(Boolean) did not work;");

        cache.set("/number", Integer.valueOf(666));
        assertEquals(666, cache.getInt("/number"), "set(Integer) did not work;");

        cache.set("/string", (Object) "a replacement string");
        assertEquals("a replacement string", cache.getString("/string"), "set(Object) did not work;");

        // TODO: should we be able to set ArrayNode and ObjectNode values in this fashion?

        assertTrue(root.isDirty(), "cache should be dirty after object writes;");
    }

    @Test
    public void testSetObjects() throws Exception {
        List<?> testObjects = Arrays.asList(TEST_OBJECT_1, TEST_OBJECT_0);
        cache.set("/testObjects", testObjects);
        List<TestObject> result = cache.getObjects("/testObjects", TestObject.class);
        assertEquals(testObjects, result, "set(path, list) did not work");
        assertTrue(root.isDirty(), "cache should be dirty after set(path, list);");
    }

    @Test
    public void testSetShort() throws Exception {
        cache.set("/number", 13);
        assertEquals(13, cache.getShort("/number"), "getShort() returned incorrect result;");

        cache.set("/object/nestedNumber", 12);
        assertEquals(12, cache.getShort("/object/nestedNumber"), "nested getShort() returned incorrect result;");

        assertTrue(root.isDirty(), "cache should be dirty after short writes;");
    }

    @Test
    public void testSetString() throws Exception {
        cache.set("/string", "a different string");
        assertEquals("a different string", cache.getString("/string"), "getString returned incorrect result;");

        cache.set("/object/nestedString", "a different nested string");
        assertEquals("a different nested string", cache.getString("/object/nestedString"),
                "nested getString returned incorrect result;");

        assertTrue(root.isDirty(), "cache should be dirty after string writes;");
    }

    @Test
    public void testSetWithMissingAncestors() throws Exception {
        JsonPointer ptr = JsonPointer.compile("/nonExistentArray/0/nonExistentObject/stringProperty");
        assertFalse(cache.exists(ptr), "exists(ptr) returned incorrect value;");
        Object value = cache.get(ptr);
        assertNull(value, "stringProperty is non-null;");
        cache.set(ptr, "string value");
        assertTrue(cache.exists(ptr), "exists(ptr) returned incorrect value;");
        value = cache.get(ptr);
        assertEquals("string value", value, "stringProperty is null after being set;");
    }

    // TODO: get/set POJO
}
