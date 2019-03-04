/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.UnsupportedEncodingException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Locale;

import org.junit.Before;
import org.junit.Test;
import org.openapitools.codegen.utils.JsonCache.CacheException;
import org.openapitools.codegen.utils.JsonCache.Root.MergePolicy;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.core.JsonPointer;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.JsonNodeType;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.datatype.joda.JodaModule;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.fasterxml.jackson.datatype.threetenbp.ThreeTenModule;

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
        root.load(new ByteArrayInputStream(JSON.getBytes("UTF-8")));
    }

    @Before
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
        assertEquals("Array size incorrect after add(path, value)", 5, cache.size("/array"));
        assertEquals("Array element at index 0 incorrect after add(path, value);", 1, cache.get("/array/0"));
        assertEquals("Array element at index 1 incorrect after add(path, value);", value,
                cache.getBigDecimal("/array/1"));
        assertEquals("Array element at index 2 incorrect after add(path, value);", "2", cache.get("/array/2"));

        value = new BigDecimal(6.2);
        cache.add("/object/nestedArray/5", value);
        assertEquals("Array size incorrect after add(path, value)", 6, cache.size("/object/nestedArray"));
        assertEquals("Array element at index 4 incorrect after add(path, value);", null,
                cache.get("/object/nestedArray/4"));
        assertEquals("Array element at index 1 incorrect after add(path, value);", value,
                cache.getBigDecimal("/object/nestedArray/5"));
    }

    @Test
    public void testAddBigInteger() throws Exception {
        BigInteger value = new BigInteger("5");
        cache.add("/array/1", value);
        assertEquals("Array size incorrect after add(path, value)", 5, cache.size("/array"));
        assertEquals("Array element at index 0 incorrect after add(path, value);", 1, cache.get("/array/0"));
        assertEquals("Array element at index 1 incorrect after add(path, value);", value,
                cache.getBigInteger("/array/1"));
        assertEquals("Array element at index 2 incorrect after add(path, value);", "2", cache.get("/array/2"));

        value = new BigInteger("6");
        cache.add("/object/nestedArray/5", value);
        assertEquals("Array size incorrect after add(path, value)", 6, cache.size("/object/nestedArray"));
        assertEquals("Array element at index 4 incorrect after add(path, value);", null,
                cache.get("/object/nestedArray/4"));
        assertEquals("Array element at index 1 incorrect after add(path, value);", value,
                cache.getBigInteger("/object/nestedArray/5"));
    }

    @Test
    public void testAddBoolean() throws Exception {
        cache.add("/array/1", true);
        assertEquals("Array size incorrect after add(path, value)", 5, cache.size("/array"));
        assertEquals("Array element at index 0 incorrect after add(path, value);", 1, cache.get("/array/0"));
        assertEquals("Array element at index 1 incorrect after add(path, value);", true, cache.getBoolean("/array/1"));
        assertEquals("Array element at index 2 incorrect after add(path, value);", "2", cache.get("/array/2"));

        cache.add("/object/nestedArray/5", true);
        assertEquals("Array size incorrect after add(path, value)", 6, cache.size("/object/nestedArray"));
        assertEquals("Array element at index 4 incorrect after add(path, value);", null,
                cache.get("/object/nestedArray/4"));
        assertEquals("Array element at index 1 incorrect after add(path, value);", true,
                cache.getBoolean("/object/nestedArray/5"));
    }

    @Test
    public void testAddDate() throws Exception {
        testAddDate0(DATETIME_OFFSET_STR, ISO8601_DATETIME_FORMAT_JAVA.parse(DATETIME_OFFSET_STR));
    }

    private void testAddDate0(String dateStr, Object date) throws Exception {
        cache.add("/array/1", dateStr);
        assertEquals("Array size incorrect after add(path, value)", 5, cache.size("/array"));
        assertEquals("Array element at index 0 incorrect after add(path, value);", 1, cache.get("/array/0"));
        assertEquals("Array element at index 1 incorrect after add(path, value);", date,
                cache.getObject("/array/1", date.getClass()));
        assertEquals("Array element at index 2 incorrect after add(path, value);", "2", cache.get("/array/2"));

        cache.add("/object/nestedArray/5", date);
        assertEquals("Array size incorrect after add(path, value)", 6, cache.size("/object/nestedArray"));
        assertEquals("Array element at index 4 incorrect after add(path, value);", null,
                cache.get("/object/nestedArray/4"));
        assertEquals("Array element at index 1 incorrect after add(path, value);", date,
                cache.getObject("/object/nestedArray/5", date.getClass()));
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
        assertEquals("Array size incorrect after add(path, value)", 5, cache.size("/array"));
        assertEquals("Array element at index 0 incorrect after add(path, value);", 1, cache.get("/array/0"));
        assertEquals("Array element at index 1 incorrect after add(path, value);", 5.1D, cache.getDouble("/array/1"),
                Double.MIN_VALUE);
        assertEquals("Array element at index 2 incorrect after add(path, value);", "2", cache.get("/array/2"));

        cache.add("/object/nestedArray/5", 6.2D);
        assertEquals("Array size incorrect after add(path, value)", 6, cache.size("/object/nestedArray"));
        assertEquals("Array element at index 4 incorrect after add(path, value);", null,
                cache.get("/object/nestedArray/4"));
        assertEquals("Array element at index 1 incorrect after add(path, value);", 6.2D,
                cache.getDouble("/object/nestedArray/5"), Double.MIN_VALUE);
    }

    @Test
    public void testAddFloat() throws Exception {
        cache.add("/array/1", 5.1F);
        assertEquals("Array size incorrect after add(path, value)", 5, cache.size("/array"));
        assertEquals("Array element at index 0 incorrect after add(path, value);", 1, cache.get("/array/0"));
        assertEquals("Array element at index 1 incorrect after add(path, value);", 5.1F, cache.getFloat("/array/1"),
                Float.MIN_VALUE);
        assertEquals("Array element at index 2 incorrect after add(path, value);", "2", cache.get("/array/2"));

        cache.add("/object/nestedArray/5", 6.2F);
        assertEquals("Array size incorrect after add(path, value)", 6, cache.size("/object/nestedArray"));
        assertEquals("Array element at index 4 incorrect after add(path, value);", null,
                cache.get("/object/nestedArray/4"));
        assertEquals("Array element at index 1 incorrect after add(path, value);", 6.2F,
                cache.getFloat("/object/nestedArray/5"), Float.MIN_VALUE);
    }

    @Test
    public void testAddInt() throws Exception {
        cache.add("/array/1", 5);
        assertEquals("Array size incorrect after add(path, value)", 5, cache.size("/array"));
        assertEquals("Array element at index 0 incorrect after add(path, value);", 1, cache.get("/array/0"));
        assertEquals("Array element at index 1 incorrect after add(path, value);", 5, cache.getInt("/array/1"));
        assertEquals("Array element at index 2 incorrect after add(path, value);", "2", cache.get("/array/2"));

        cache.add("/object/nestedArray/5", 6);
        assertEquals("Array size incorrect after add(path, value)", 6, cache.size("/object/nestedArray"));
        assertEquals("Array element at index 4 incorrect after add(path, value);", null,
                cache.get("/object/nestedArray/4"));
        assertEquals("Array element at index 1 incorrect after add(path, value);", 6,
                cache.getInt("/object/nestedArray/5"));
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
        assertEquals("Array size incorrect after add(path, value)", 5, cache.size("/array"));
        assertEquals("Array element at index 0 incorrect after add(path, value);", 1, cache.get("/array/0"));
        assertEquals("Array element at index 1 incorrect after add(path, value);", 5L, cache.getLong("/array/1"));
        assertEquals("Array element at index 2 incorrect after add(path, value);", "2", cache.get("/array/2"));

        cache.add("/object/nestedArray/5", 6L);
        assertEquals("Array size incorrect after add(path, value)", 6, cache.size("/object/nestedArray"));
        assertEquals("Array element at index 4 incorrect after add(path, value);", null,
                cache.get("/object/nestedArray/4"));
        assertEquals("Array element at index 1 incorrect after add(path, value);", 6L,
                cache.getLong("/object/nestedArray/5"));
    }

    public void testAddObject() throws Exception {
        cache.add("/array/1", TEST_OBJECT_0);
        assertEquals("Array size incorrect after add(path, value)", 5, cache.size("/array"));
        assertEquals("Array element at index 0 incorrect after add(path, value);", 1, cache.get("/array/0"));
        assertEquals("Array element at index 1 incorrect after add(path, value);", TEST_OBJECT_0,
                cache.getObject("/array/1", TestObject.class));
        assertEquals("Array element at index 2 incorrect after add(path, value);", "2", cache.get("/array/2"));

        cache.add("/object/nestedArray/5", TEST_OBJECT_1);
        assertEquals("Array size incorrect after add(path, value)", 6, cache.size("/object/nestedArray"));
        assertEquals("Array element at index 4 incorrect after add(path, value);", null,
                cache.get("/object/nestedArray/4"));
        assertEquals("Array element at index 1 incorrect after add(path, value);", TEST_OBJECT_1,
                cache.getObject("/object/nestedArray/5", TestObject.class));
    }

    @Test
    public void testAddShort() throws Exception {
        cache.add("/array/1", (short) 5);
        assertEquals("Array size incorrect after add(path, value)", 5, cache.size("/array"));
        assertEquals("Array element at index 0 incorrect after add(path, value);", 1, cache.get("/array/0"));
        assertEquals("Array element at index 1 incorrect after add(path, value);", (short) 5,
                cache.getShort("/array/1"));
        assertEquals("Array element at index 2 incorrect after add(path, value);", "2", cache.get("/array/2"));

        cache.add("/object/nestedArray/5", (short) 6);
        assertEquals("Array size incorrect after add(path, value)", 6, cache.size("/object/nestedArray"));
        assertEquals("Array element at index 4 incorrect after add(path, value);", null,
                cache.get("/object/nestedArray/4"));
        assertEquals("Array element at index 1 incorrect after add(path, value);", (short) 6,
                cache.getShort("/object/nestedArray/5"));
    }

    @Test
    public void testAddString() throws Exception {
        cache.add("/array/1", "5");
        assertEquals("Array size incorrect after add(path, value)", 5, cache.size("/array"));
        assertEquals("Array element at index 0 incorrect after add(path, value);", 1, cache.get("/array/0"));
        assertEquals("Array element at index 1 incorrect after add(path, value);", "5", cache.getString("/array/1"));
        assertEquals("Array element at index 2 incorrect after add(path, value);", "2", cache.get("/array/2"));

        cache.add("/object/nestedArray/5", "6");
        assertEquals("Array size incorrect after add(path, value)", 6, cache.size("/object/nestedArray"));
        assertEquals("Array element at index 4 incorrect after add(path, value);", null,
                cache.get("/object/nestedArray/4"));
        assertEquals("Array element at index 1 incorrect after add(path, value);", "6",
                cache.getString("/object/nestedArray/5"));
    }

    @Test
    public void testDelete() throws Exception {
        assertTrue("existing root element is not an object;", cache.get("") instanceof ObjectNode);

        cache.delete("/array/2");
        assertEquals("Array size incorrect after delete element;", 3, cache.size("/array"));
        assertEquals("Array element at index 1 incorrect after delete;", "2", cache.getString("/array/1"));
        assertEquals("Array element at index 2 incorrect after delete;", 4.0D, cache.getDouble("/array/2"),
                Double.MIN_VALUE);

        cache.delete("/object/nestedArray/2");
        assertEquals("Array size incorrect after delete element;", 3, cache.size("/object/nestedArray"));
        assertEquals("Array element at index 1 incorrect after delete;", "b", cache.getString("/object/nestedArray/1"));
        assertEquals("Array element at index 2 incorrect after delete;", "d", cache.getString("/object/nestedArray/2"));

        cache.delete("/object");
        assertFalse("delete object failed;", cache.exists("/object"));

        cache.delete("");
        assertFalse("delete root failed;", cache.exists(""));

        // This call should have reinstated the missing root as an array.
        cache.set("/0", true);
        assertTrue("new root element is not an array;", cache.get("") instanceof ArrayNode);
        assertEquals("Failed to set root array element 0 when root is null;", true, cache.getBoolean("/0"));

        cache.set("/1", 3.14);
        assertEquals("Failed to set root array element 1;", 3.14, cache.getDouble("/1"), Double.MIN_VALUE);

        cache.set("/2", "a string element");
        assertEquals("Failed to set root array element 2;", "a string element", cache.getString("/2"));

        // Now that root has been reallocated as an array, try and set a non-integer property on it.
        try {
            cache.set("/astring", "won't work!");
            fail("attempting to set a non-integer property on an array should have failed");
        } catch (NumberFormatException e) {
            assertFalse("setting a non-numeric property on an array worked!;", cache.exists("/astring"));
        }
    }

    @Test
    public void testFlushSaveLoad() throws Exception {
        ByteArrayOutputStream out = new ByteArrayOutputStream();

        root.flush(out);
        assertTrue("non-dirty flush() should not have written bytes;", out.size() == 0);
        assertFalse("cache should not be dirty after no-op flush();", root.isDirty());

        root.save(out);
        assertTrue("save() wrote no bytes;", out.size() > 0);
        assertFalse("cache should not be dirty after save();", root.isDirty());

        root.delete("");
        assertFalse("cache not empty;", cache.exists(""));

        root.unload();
        assertFalse("cache should not be dirty after unload();", root.isDirty());

        root.load(new ByteArrayInputStream(out.toByteArray()));
        assertFalse("cache should not be dirty after load();", root.isDirty());

        testGet();

        assertFalse("cache should not be dirty after reads;", root.isDirty());
    }

    @Test
    public void testGet() throws Exception {
        assertEquals("boolean get() returned incorrect result;", true, cache.get("/boolean"));
        assertEquals("number get() returned incorrect result;", 3.14, cache.get("/number"));
        assertEquals("string get() returned incorrect result;", "a string", cache.get("/string"));
        Object array = cache.get("/array");
        assertNotNull("array get() returned null;", array);
        assertTrue("array get() returned incorrect type;", array instanceof ArrayNode);
        assertEquals("array size() returned incorrect result;", 4, cache.size("/array"));
        assertEquals("array get element 0 returned incorrect result;", 1, cache.get("/array/0"));
        assertEquals("array get element 2 returned incorrect result;", JsonNodeType.NUMBER,
                cache.getNodeType("/array/0"));
        assertEquals("array get element 1 returned incorrect result;", "2", cache.get("/array/1"));
        assertEquals("array get element 2 returned incorrect result;", 3.0, cache.get("/array/2"));
        assertEquals("array get element 2 returned incorrect result;", JsonNodeType.NUMBER,
                cache.getNodeType("/array/2"));
        assertEquals("array get element 3 returned incorrect result;", 4.0D, cache.get("/array/3"));
        Object object = cache.get("/object");
        assertNotNull("object get() returned null;", object);
        assertTrue("object get() returned incorrect type;", object instanceof ObjectNode);

        assertEquals("nested boolean get() returned incorrect result;", true, cache.get("/object/nestedBoolean"));
        assertEquals("nested number get() returned incorrect result;", 2.72, cache.get("/object/nestedNumber"));
        assertEquals("nested string get() returned incorrect result;", "a nested string",
                cache.get("/object/nestedString"));
        Object nestedArray = cache.get("/object/nestedArray");
        assertNotNull("nested array get() returned null;", nestedArray);
        assertTrue("nested array get() returned incorrect type;", nestedArray instanceof ArrayNode);
        assertEquals("nested array size() returned incorrect result;", 4, cache.size("/object/nestedArray"));
        assertEquals("nested array get element returned incorrect result;", "a", cache.get("/object/nestedArray/0"));
        assertEquals("nested array get element returned incorrect result;", "d", cache.get("/object/nestedArray/3"));
        Object nestedObject = cache.get("/object/nestedObject");
        assertNotNull("object get() returned null;", nestedObject);
        assertTrue("object get() returned incorrect type;", nestedObject instanceof ObjectNode);

        assertEquals("test object 0 booleanField is incorrect;", true, cache.get("/testObjects/0/booleanField"));
        assertEquals("test object 0 shortField is incorrect;", 1, cache.get("/testObjects/0/shortField"));
        assertEquals("test object 0 intField is incorrect;", 2, cache.get("/testObjects/0/intField"));
        assertEquals("test object 0 longField is incorrect;", 3, cache.get("/testObjects/0/longField"));
        assertEquals("test object 0 floatField is incorrect;", 1.23, cache.get("/testObjects/0/floatField"));
        assertEquals("test object 0 doubleField is incorrect;", 4.56, cache.get("/testObjects/0/doubleField"));
        assertEquals("test object 0 stringField is incorrect;", "a test string field",
                cache.get("/testObjects/0/stringField"));

        assertEquals("test object 1 booleanField is incorrect;", false, cache.get("/testObjects/1/booleanField"));
        assertEquals("test object 1 shortField is incorrect;", 4, cache.get("/testObjects/1/shortField"));
        assertEquals("test object 1 intField is incorrect;", 5, cache.get("/testObjects/1/intField"));
        assertEquals("test object 1 longField is incorrect;", 6, cache.get("/testObjects/1/longField"));
        assertEquals("test object 1 floatField is incorrect;", 8.23, cache.get("/testObjects/1/floatField"));
        assertEquals("test object 1 doubleField is incorrect;", 9.56, cache.get("/testObjects/1/doubleField"));
        assertEquals("test object 1 stringField is incorrect;", "another test string field",
                cache.get("/testObjects/1/stringField"));
    }

    @Test
    public void testGetBigDecimal() throws Exception {
        assertEquals("getBigDecimal(path) returned incorrect result;", new BigDecimal("3.14"),
                cache.getBigDecimal("/number"));
        assertEquals("getBigDecimal(nestedPath) returned incorrect result;", new BigDecimal("2.72"),
                cache.getBigDecimal("/object/nestedNumber"));
        assertFalse("cache should not be dirty after getBigDecimal(path);", root.isDirty());
    }

    @Test
    public void testGetBigDecimalWithDefault() throws Exception {
        BigDecimal bd162 = new BigDecimal("1.62");
        BigDecimal bd272 = new BigDecimal("2.72");
        BigDecimal bd314 = new BigDecimal("3.14");
        BigDecimal bd628 = new BigDecimal("3.14");

        assertEquals("getBigDecimal(path, default) returned incorrect result;", bd314,
                cache.getBigDecimal("/number", bd162));
        assertEquals("getBigDecimal(nestedPath, default) returned incorrect result;", bd272,
                cache.getBigDecimal("/object/nestedNumber", bd628));
        assertFalse("cache should not be dirty after getBigDecimal(path, default);", root.isDirty());

        assertEquals("getBigDecimal(nonExistentPath, default) returned incorrect result;", bd162,
                cache.getBigDecimal("/nonExistentNumber", bd162));
        assertEquals("nested getBigDecimal(nonExistentNestedPath, default) returned incorrect result;", bd628,
                cache.getBigDecimal("/object/nonExistentNestedNumber", bd628));
        assertTrue("cache should be dirty after getBigDecimal(nonExistentPath, default);", root.isDirty());

        assertEquals("getBigDecimal(path) returned incorrect result after update;", bd162,
                cache.getBigDecimal("/nonExistentNumber"));
        assertEquals("nested getBigDecimal(path) returned incorrect result after update;", bd628,
                cache.getBigDecimal("/object/nonExistentNestedNumber"));
    }

    @Test
    public void testGetBigInteger() throws Exception {
        BigInteger bi2 = new BigInteger("2");
        BigInteger bi3 = new BigInteger("3");

        assertEquals("getBigInteger(path) returned incorrect result;", bi3, cache.getBigInteger("/number"));
        assertEquals("getBigInteger(nestedPath) returned incorrect result;", bi2,
                cache.getBigInteger("/object/nestedNumber"));
        assertFalse("cache should not be dirty after getBigInteger(path);", root.isDirty());
    }

    @Test
    public void testGetBigIntegerWithDefault() throws Exception {
        BigInteger bi2 = new BigInteger("2");
        BigInteger bi3 = new BigInteger("3");
        BigInteger bi4 = new BigInteger("4");
        BigInteger bi5 = new BigInteger("5");

        assertEquals("getBigInteger(path, default) returned incorrect result;", bi3,
                cache.getBigInteger("/number", bi4));
        assertEquals("getBigInteger(nestedPath, default) returned incorrect result;", bi2,
                cache.getBigInteger("/object/nestedNumber", bi5));
        assertFalse("cache should not be dirty after getBigInteger(path, default);", root.isDirty());

        assertEquals("getBigInteger(nonExistentPath, default) returned incorrect result;", bi4,
                cache.getBigInteger("/nonExistentNumber", bi4));
        assertEquals("getBigInteger(nonExistentNestedPath, default) returned incorrect result;", bi5,
                cache.getBigInteger("/object/nonExistentNestedNumber", bi5));
        assertTrue("cache should be dirty after getBigInteger(nonExistentPath, default);", root.isDirty());

        assertEquals("getBigInteger(path) returned incorrect result after update;", bi4,
                cache.getBigInteger("/nonExistentNumber"));
        assertEquals("getBigInteger(nestedPath) returned incorrect result after update;", bi5,
                cache.getBigInteger("/object/nonExistentNestedNumber"));
    }

    @Test
    public void testGetBoolean() throws Exception {
        assertEquals("getBoolean(path) returned incorrect result;", true, cache.getBoolean("/boolean"));
        assertEquals("getBoolean(nestedPath) returned incorrect result;", true,
                cache.getBoolean("/object/nestedBoolean"));
        assertFalse("cache should not be dirty after getBoolean(path);", root.isDirty());
    }

    @Test
    public void testGetBooleanWithDefault() throws Exception {
        assertEquals("getBoolean(path, default) returned incorrect result;", true, cache.getBoolean("/boolean", false));
        assertEquals("getBoolean(nestedPath, default) returned incorrect result;", true,
                cache.getBoolean("/object/nestedBoolean", false));
        assertFalse("cache should not be dirty after getBoolean(path, default);", root.isDirty());

        assertEquals("getBoolean(nonExistentPath, default) returned incorrect result;", true,
                cache.getBoolean("/nonExistentBoolean", true));
        assertEquals("getBoolean(nonExistentNestedPath, default) returned incorrect result;", true,
                cache.getBoolean("/object/nonExistentNestedBoolean", true));
        assertTrue("cache should be dirty after getBoolean(nonExistentPath, default);", root.isDirty());

        assertEquals("getBoolean(nonExistentPath, default) returned incorrect result;", true,
                cache.getBoolean("/nonExistentBoolean"));
        assertEquals("getBoolean(nonExistentNestedPath, default) returned incorrect result;", true,
                cache.getBoolean("/object/nonExistentNestedBoolean"));
    }

    @Test
    public void testGetDate() throws Exception {
        assertEquals("getDate(path) returned incorrect result;", EXPECTED_DATE_JAVA,
                cache.getObject("/dateTimeZ", Date.class));
        assertEquals("getDate(nestedPath) returned incorrect result;", EXPECTED_DATE_JAVA,
                cache.getObject("/object/nestedDateTimeZ", Date.class));
        assertFalse("cache should not be dirty after getDate(path);", root.isDirty());
    }

    @Test
    public void testGetDateWithDefault() throws Exception {
        Date defaultDate = new Date();
        assertEquals("getDate(path, default) returned incorrect result;", EXPECTED_DATE_JAVA,
                cache.getObject("/dateTimeZ", defaultDate));
        assertEquals("getDate(nestedPath, default) returned incorrect result;", EXPECTED_DATE_JAVA,
                cache.getObject("/object/nestedDateTimeZ", defaultDate));
        assertFalse("cache should not be dirty after getDate(path, default);", root.isDirty());

        assertEquals("getDate(nonExistentPath, default) returned incorrect result;", defaultDate,
                cache.getObject("/nonExistentDate", defaultDate));
        assertEquals("getDate(nonExistentNestedPath, default) returned incorrect result;", defaultDate,
                cache.getObject("/object/nonExistentNestedDate", defaultDate));
        assertTrue("cache should be dirty after getDate(nonExistentPath, default);", root.isDirty());

        assertEquals("getDate(path) returned incorrect result after update;", defaultDate,
                cache.getObject("/nonExistentDate", Date.class));
        assertEquals("getDate(nestedPath) returned incorrect result after update;", defaultDate,
                cache.getObject("/object/nonExistentNestedDate", Date.class));
    }

    @Test
    public void testGetDouble() throws Exception {
        assertEquals("getDouble(path) returned incorrect result;", 3.14D, cache.getDouble("/number"), Double.MIN_VALUE);
        assertEquals("getDouble(nestedPath) returned incorrect result;", 2.72D, cache.getDouble("/object/nestedNumber"),
                Double.MIN_VALUE);
        assertFalse("cache should not be dirty after getDouble(path);", root.isDirty());
    }

    @Test
    public void testGetDoubleWithDefault() throws Exception {
        assertEquals("getDouble(path, default) returned incorrect result;", 3.14D, cache.getDouble("/number", 7.77D),
                Double.MIN_VALUE);
        assertEquals("getDouble(nestedPath, default) returned incorrect result;", 2.72D,
                cache.getDouble("/object/nestedNumber", 8.88D), Double.MIN_VALUE);
        assertFalse("cache should not be dirty after getDouble(path, default);", root.isDirty());

        assertEquals("getDouble(nonExistentPath, default) returned incorrect result;", 7.77D,
                cache.getDouble("/nonExistentNumber", 7.77D), Double.MIN_VALUE);
        assertEquals("getDouble(nonExistentNestedPath, default) returned incorrect result;", 8.88D,
                cache.getDouble("/object/nonExistentNestedNumber", 8.88D), Double.MIN_VALUE);
        assertTrue("cache should be dirty after getDouble(nonExistentPath, default);", root.isDirty());

        assertEquals("getDouble(path) returned incorrect result after update;", 7.77D,
                cache.getDouble("/nonExistentNumber"), Double.MIN_VALUE);
        assertEquals("getDouble(nestedPath) returned incorrect result after update;", 8.88D,
                cache.getDouble("/object/nonExistentNestedNumber"), Double.MIN_VALUE);
    }

    @Test
    public void testGetFloat() throws Exception {
        assertEquals("getFloat(path) returned incorrect result;", 3.14F, cache.getFloat("/number"), Float.MIN_VALUE);
        assertEquals("nested getFloat(nestedPath) returned incorrect result;", 2.72F,
                cache.getFloat("/object/nestedNumber"), Float.MIN_VALUE);
        assertFalse("cache should not be dirty after getFloat(path);", root.isDirty());
    }

    @Test
    public void testGetFloatWithDefault() throws Exception {
        assertEquals("getFloat(path, default) returned incorrect result;", 3.14F, cache.getFloat("/number", 7.77F),
                Float.MIN_VALUE);
        assertEquals("nested getFloat(nestedPath, default) returned incorrect result;", 2.72F,
                cache.getFloat("/object/nestedNumber", 8.88F), Float.MIN_VALUE);
        assertFalse("cache should not be dirty after getFloat(path, default);", root.isDirty());

        assertEquals("getFloat(nonExistentPath, default) returned incorrect result;", 7.77F,
                cache.getFloat("/nonExistentNumber", 7.77F), Float.MIN_VALUE);
        assertEquals("nested getFloat(nonExistentNestedPath, default) returned incorrect result;", 8.88F,
                cache.getFloat("/object/nonExistentNestedNumber", 8.88F), Float.MIN_VALUE);
        assertTrue("cache should be dirty after getFloat(nonExistentPath, default);", root.isDirty());

        assertEquals("getFloat(path) returned incorrect result after update;", 7.77F,
                cache.getFloat("/nonExistentNumber"), Float.MIN_VALUE);
        assertEquals("nested getFloat(nestedPath) returned incorrect result after update;", 8.88F,
                cache.getFloat("/object/nonExistentNestedNumber"), Float.MIN_VALUE);
    }

    @Test
    public void testGetInt() throws Exception {
        assertEquals("getInt(path) returned incorrect result;", 3, cache.getInt("/number"));
        assertEquals("getInt(nestedPath) returned incorrect result;", 2, cache.getInt("/object/nestedNumber"));
        assertFalse("cache should not be dirty after getInt(path);", root.isDirty());
    }

    @Test
    public void testGetIntWithDefault() throws Exception {
        assertEquals("getInt(path, default) returned incorrect result;", 3, cache.getInt("/number", 5));
        assertEquals("getInt(nestedPath, default) returned incorrect result;", 4, cache.getInt("/object/nestedNumber"),
                2);
        assertFalse("cache should not be dirty after getInt(path, default);", root.isDirty());

        assertEquals("getInt(nonExistentPath, default) returned incorrect result;", 5,
                cache.getInt("/nonExistentNumber", 5));
        assertEquals("getInt(nonExistentNestedPath, default) returned incorrect result;", 4,
                cache.getInt("/object/nonExistentNestedNumber", 4));
        assertTrue("cache should be dirty after getInt(nonExistentPath, default);", root.isDirty());

        assertEquals("getInt(path) returned incorrect result after update;", 5, cache.getInt("/nonExistentNumber"));
        assertEquals("getInt(nestedPath) returned incorrect result after update;", 4,
                cache.getInt("/object/nonExistentNestedNumber"));
    }

    @Test
    public void testGetLocalDate() throws Exception {
        assertEquals("getLocalDate(path) returned incorrect result;", EXPECTED_DATE_JODA,
                cache.getObject("/date", org.joda.time.LocalDate.class));
        assertFalse("cache should not be dirty after getLocalDate(path);", root.isDirty());
    }

    public void testGetLocalDateWithDefault() throws Exception {
        org.joda.time.LocalDate defaultDate = new org.joda.time.LocalDate();
        assertEquals("getLocalDate(path, default) returned incorrect result;", EXPECTED_DATE_JODA,
                cache.getObject("/dateTimeZ", defaultDate));
        assertEquals("getLocalDate(nestedPath, default) returned incorrect result;", EXPECTED_DATE_JODA,
                cache.getObject("/object/nestedLocalDate", defaultDate));
        assertFalse("cache should not be dirty after getLocalDate(path, default);", root.isDirty());

        assertEquals("getLocalDate(nonExistentPath, default) returned incorrect result;", defaultDate,
                cache.getObject("/nonExistentLocalDate", defaultDate));
        assertEquals("getLocalDate(nonExistentNestedPath, default) returned incorrect result;", defaultDate,
                cache.getObject("/object/nonExistentNestedLocalDate", defaultDate));
        assertTrue("cache should be dirty after getLocalDate(nonExistentPath, default);", root.isDirty());

        assertEquals("getLocalDate(path) returned incorrect result after update;", defaultDate,
                cache.getObject("/nonExistentLocalDate", org.joda.time.LocalDate.class));
        assertEquals("getLocalDate(nestedPath) returned incorrect result after update;", defaultDate,
                cache.getObject("/object/nonExistentNestedLocalDate", org.joda.time.LocalDate.class));
    }

    @Test
    public void testGetLong() throws Exception {
        assertEquals("getLong(path) returned incorrect result;", 3L, cache.getLong("/number"));
        assertEquals("getLong(nestedPath) returned incorrect result;", 2L, cache.getLong("/object/nestedNumber"));
        assertFalse("cache should not be dirty after getLong(path);", root.isDirty());
    }

    @Test
    public void testGetLongWithDefault() throws Exception {
        assertEquals("getLong(path, default) returned incorrect result;", 3L, cache.getLong("/number", 5L));
        assertEquals("getLong(nestedPath, default) returned incorrect result;", 2L,
                cache.getLong("/object/nestedNumber", 4L));
        assertFalse("cache should not be dirty after getLong(path, default);", root.isDirty());

        assertEquals("getLong(nonExistentPath, default) returned incorrect result;", 5L,
                cache.getLong("/nonExistentNumber", 5L));
        assertEquals("getLong(nonExistentNestedPath, default) returned incorrect result;", 4L,
                cache.getLong("/object/nonExistentNestedNumber", 4L));
        assertTrue("cache should be dirty after getLong(nonExistentPath, default);", root.isDirty());

        assertEquals("getLong(path, default) returned incorrect result after update;", 5L,
                cache.getLong("/nonExistentNumber"));
        assertEquals("getLong(nestedPath, default) returned incorrect result after update;", 4L,
                cache.getLong("/object/nonExistentNestedNumber"));
    }

    @Test
    public void testGetNumber() throws Exception {
        Number number = cache.getNumber("/number");
        assertTrue("getNumber(path) returned incorrect type;", number instanceof Double);
        assertEquals("getNumber(path) returned incorrect result;", 3.14D, number);
        Number nestedNumber = cache.getNumber("/object/nestedNumber");
        assertTrue("getNumber(nestedPath) returned incorrect type;", nestedNumber instanceof Double);
        assertEquals("nested getNumber(nestedPath) returned incorrect result;", 2.72D, nestedNumber);
        assertFalse("cache should not be dirty after getNumber(path);", root.isDirty());
    }

    @Test
    public void testGetNumberWithDefault() throws Exception {
        assertEquals("getNumber(path, default) returned incorrect result;", 3.14D, cache.getNumber("/number", 7.77D));
        assertEquals("getNumber(nestedPath, default) returned incorrect result;", 2.72D,
                cache.getNumber("/object/nestedNumber", 8.88D));
        assertFalse("cache should not be dirty after getNumber(path, default);", root.isDirty());

        assertEquals("getNumber(nonExistentPath, default) returned incorrect result;", 7.77D,
                cache.getNumber("/nonExistentNumber", 7.77D));
        assertEquals("getNumber(nonExistentNestedPath, default) returned incorrect result;", 8.88D,
                cache.getNumber("/object/nonExistentNestedNumber", 8.88D));
        assertTrue("cache should be dirty after getNumber(nonExistentPath, default);", root.isDirty());

        assertEquals("getNumber(path, default) returned incorrect result after update;", 7.77D,
                cache.getNumber("/nonExistentNumber"));
        assertEquals("getNumber(nestedPath, default) returned incorrect result after update;", 8.88D,
                cache.getNumber("/object/nonExistentNestedNumber"));
    }

    @Test
    public void testGetObject() throws Exception {
        TestObject testObject0 = cache.getObject("/testObjects/0", TestObject.class);
        assertEquals("getObject(path) returned incorrect result;", TEST_OBJECT_0, testObject0);
        TestObject testObject1 = cache.getObject("/testObjects/1", TestObject.class);
        assertEquals("getObject(path) returned incorrect result;", TEST_OBJECT_1, testObject1);
        assertFalse("cache should not be dirty after getObject(path, type);", root.isDirty());
    }

    @Test
    public void testGetObjects() throws Exception {
        List<TestObject> testObjects = cache.getObjects("/testObjects", TestObject.class);
        assertEquals("getObjects(path, type) returned incorrect result;", TEST_OBJECT_0, testObjects.get(0));
        assertEquals("getObjects(path, type) returned incorrect result;", TEST_OBJECT_1, testObjects.get(1));
        assertFalse("cache should not be dirty after getObjects(path, type);", root.isDirty());
    }

    @Test
    public void testGetObjectsWithDefault() throws Exception {
        List<TestObject> defaultValue = Arrays.asList(TEST_OBJECT_1, TEST_OBJECT_0);
        List<TestObject> testObjects = cache.getObjects("/testObjects", TestObject.class, defaultValue);
        assertEquals("getObjects(path, type, defaultValue) returned incorrect result;", TEST_OBJECT_0,
                testObjects.get(0));
        assertEquals("getObjects(path, type, defaultValue) returned incorrect result;", TEST_OBJECT_1,
                testObjects.get(1));
        assertFalse("cache should not be dirty after getObjects(path, type, defaultValue);", root.isDirty());

        testObjects = cache.getObjects("/nonExistentTestObjects", TestObject.class, defaultValue);
        assertEquals("getObjects(nonExistentPath, type, defaultValue) returned incorrect result;", TEST_OBJECT_1,
                testObjects.get(0));
        assertEquals("getObjects(nonExistentPath, type, defaultValue) returned incorrect result;", TEST_OBJECT_0,
                testObjects.get(1));
        assertTrue("cache should be dirty after getObjects(nonExistentPath, type, defaultValue);", root.isDirty());

        testObjects = cache.getObjects("/nonExistentTestObjects", TestObject.class);
        assertEquals("getObjects(path, type) returned incorrect result after update;", TEST_OBJECT_1,
                testObjects.get(0));
        assertEquals("getObjects(path, type) returned incorrect result after update;", TEST_OBJECT_0,
                testObjects.get(1));
    }

    @Test
    public void testGetObjectWithDefault() throws Exception {
        assertEquals("getObject(path, default) returned incorrect result;", TEST_OBJECT_0,
                cache.getObject("/testObjects/0", TEST_OBJECT_1));
        assertFalse("cache should not be dirty after getObject(path, default);", root.isDirty());

        assertSame("getObject(nonExistentPath, default) returned incorrect result;", TEST_OBJECT_1,
                cache.getObject("/testObjects/2", TEST_OBJECT_1));
        assertTrue("cache should be dirty after getObject(nonExistentPath, default);", root.isDirty());

        assertSame("getObject(path, type) returned incorrect result after update;", TEST_OBJECT_1,
                cache.getObject("/testObjects/2", TestObject.class));
    }

    @Test
    public void testGetShort() throws Exception {
        assertEquals("getShort(path) returned incorrect result;", 3, cache.getShort("/number"));
        assertEquals("nested getShort(nestedPath) returned incorrect result;", 2,
                cache.getShort("/object/nestedNumber"));
        assertFalse("cache should not be dirty after getShort(path);", root.isDirty());
    }

    @Test
    public void testGetShortWithDefault() throws Exception {
        assertEquals("getShort(path, default) returned incorrect result;", 3, cache.getShort("/number", (short) 5));
        assertEquals("nested getShort(nestedPath, default) returned incorrect result;", 2,
                cache.getShort("/object/nestedNumber", (short) 4));
        assertFalse("cache should not be dirty after getShort(path, default);", root.isDirty());

        assertEquals("getShort(nonExistentPath, default) returned incorrect result;", 5,
                cache.getShort("/nonExistentNumber", (short) 5));
        assertEquals("nested getShort(nonExistentPathNestedPath, default) returned incorrect result;", 4,
                cache.getShort("/object/nonExistentNestedNumber", (short) 4));
        assertTrue("cache should be dirty after getShort(nonExistentPath, default);", root.isDirty());

        assertEquals("getShort(path) returned incorrect result after update;", 5, cache.getShort("/nonExistentNumber"));
        assertEquals("nested getShort(nestedPath) returned incorrect result after update;", 4,
                cache.getShort("/object/nonExistentNestedNumber"));
    }

    @Test
    public void testGetString() throws Exception {
        assertEquals("getString(path) returned incorrect result;", "a string", cache.getString("/string"));
        assertEquals("getString(nestedPath) returned incorrect result;", "a nested string",
                cache.getString("/object/nestedString"));
        assertFalse("cache should not be dirty after getString(path);", root.isDirty());
    }

    @Test
    public void testGetStringWithDefault() throws Exception {
        assertEquals("getString(path, default) returned incorrect result;", "a string",
                cache.getString("/string", "a different string"));
        assertEquals("getString(nestedPath, default) returned incorrect result;", "a nested string",
                cache.getString("/object/nestedString", "a different nested string"));
        assertFalse("cache should not be dirty after getString(path, default);", root.isDirty());

        assertEquals("getString(nonExistentPath, default) returned incorrect result;", "a different string",
                cache.getString("/nonExistentString", "a different string"));
        assertEquals("getString(nonExistentNestedPath, default) returned incorrect result;",
                "a different nested string",
                cache.getString("/object/nonExistentNestedString", "a different nested string"));
        assertTrue("cache should be dirty after getString(nonExistentPath, default);", root.isDirty());

        assertEquals("getString(path) returned incorrect result after update;", "a different string",
                cache.getString("/nonExistentString"));
        assertEquals("getString(nestedPath) returned incorrect result after update;", "a different nested string",
                cache.getString("/object/nonExistentNestedString"));
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
        ByteArrayInputStream in = new ByteArrayInputStream(incoming.getBytes("UTF-8"));

        root.mergePolicy(MergePolicy.NO_MERGE).load(in);
        assertFalse("cache should not be dirty after second load() in NO_MERGE mode", root.isDirty());
        assertEquals("root size incorrect after second load() in NO_MERGE mode;", 1, root.size(""));
        assertEquals("cache size incorrect after second load() in NO_MERGE mode;", 8, cache.size(""));
        testGet();

        in.reset();
        root.mergePolicy(MergePolicy.KEEP_EXISTING).load(in);
        assertTrue("cache should be dirty after second load() in KEEP_EXISTING mode", root.isDirty());
        assertEquals("root size incorrect after second load() in KEEP_EXISTING mode;", 2, root.size(""));
        assertEquals("cache size incorrect after second load() in KEEP_EXISTING mode;", 8, cache.size(""));
        assertFalse("descendent elements should not be merged in KEEP_EXISTING mode", cache.exists("/anotherString"));
        assertFalse("descendent elements should not be merged in KEEP_EXISTING mode",
                cache.exists("/object/nestedObject/nestedNested"));
        assertEquals("added root property not merged in KEEP_EXISTING mode", 10, root.get("/JsonCacheMerge/number"));
        assertEquals("added array elements merged in KEEP_EXISTING mode", 4, cache.size("/array"));
        assertEquals("added array elements merged in KEEP_EXISTING mode", 4, cache.size("/object/nestedArray"));
        testGet();

        reload();
        in.reset();
        root.mergePolicy(MergePolicy.MERGE_RECURSIVE).load(in);
        assertTrue("cache should be dirty after second load() in MERGE_RECURSIVE mode", root.isDirty());
        assertEquals("root size incorrect after second load() in MERGE_RECURSIVE mode;", 2, root.size(""));
        assertEquals("added root property not merged in MERGE_RECURSIVE mode", 10, root.get("/JsonCacheMerge/number"));
        assertEquals("added string property not merged in MERGE_RECURSIVE mode", "another string",
                cache.get("/anotherString"));
        assertEquals("added child array elements not merged in MERGE_RECURSIVE mode", 7, cache.size("/array"));
        assertEquals("added child array element 4 not merged in MERGE_RECURSIVE mode", JsonNodeType.BOOLEAN,
                cache.getNodeType("/array/4"));
        assertEquals("added child array element 5 not merged in MERGE_RECURSIVE mode", JsonNodeType.OBJECT,
                cache.getNodeType("/array/5"));
        assertEquals("added child array element 6 not merged in MERGE_RECURSIVE mode", JsonNodeType.ARRAY,
                cache.getNodeType("/array/6"));
        assertEquals("array not merged correctly in MERGE_RECURSIVE mode",
                Arrays.asList("a", "b", "c", "d", "e", "f", "g"),
                cache.getObjects("/object/nestedArray", String.class));
        assertEquals("nested object not merged correctly in MERGE_RECURSIVE mode", "waz",
                cache.get("/object/nestedObject/nestedNested/d"));

        reload();
        in.reset();
        root.mergePolicy(MergePolicy.OVERWRITE_EXISTING).load(in);
        assertTrue("cache should be dirty after second load() in OVERWRITE_EXISTING mode", root.isDirty());
        assertEquals("existing boolean not overwritten by load() in OVERWRITE_EXISTING mode;", false,
                cache.get("/boolean"));
        assertFalse("existing number not deleted by load() in OVERWRITE_EXISTING mode;", cache.exists("/number"));
        assertEquals("existing string not overwritten by load() in OVERWRITE_EXISTING mode;", "a different string",
                cache.get("/string"));
        assertEquals("added string not merged in OVERWRITE_EXISTING mode;", "another string",
                cache.get("/anotherString"));
        assertEquals("added array size incorrect in OVERWRITE_EXISTING mode;", 7, cache.size("/array"));
        assertEquals("added object size incorrect in OVERWRITE_EXISTING mode;", 3, cache.size("/object"));
        assertEquals("added nested string incorrect in OVERWRITE_EXISTING mode;", "a different nested string",
                cache.get("/object/nestedString"));
        assertEquals("added nested array size incorrect in OVERWRITE_EXISTING mode;", 5,
                cache.size("/object/nestedArray"));
        assertFalse("added nested string incorrect in OVERWRITE_EXISTING mode;",
                cache.exists("/object/nestedObject/a"));
        assertEquals("added nested string incorrect in OVERWRITE_EXISTING mode;", "baz",
                cache.get("/object/nestedObject/b"));
        assertEquals("added deep nested string incorrect in OVERWRITE_EXISTING mode;", "waz",
                cache.get("/object/nestedObject/nestedNested/d"));
        assertEquals("added root property not merged in OVERWRITE_EXISTING mode", 10,
                root.get("/JsonCacheMerge/number"));
    }

    @Test
    public void testPojoSerialization() throws Exception {
        cache.set("/testObjects/0", TEST_OBJECT_0);
        cache.set("/testObjects/1", TEST_OBJECT_1);
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        root.save(out);
        cache.delete("");
        assertNull("root is not null;", cache.get(""));

        root.unload();
        root.load(new ByteArrayInputStream(out.toByteArray()));

        testGet();

        assertFalse("cache should not be dirty after reads;", root.isDirty());
    }

    @Test
    public void testSet() throws Exception {
        cache.set("/boolean", false);
        assertEquals("boolean get() returned incorrect result;", false, cache.get("/boolean"));

        cache.set("/number", 6.28);
        assertEquals("number get() returned incorrect result;", 6.28, cache.get("/number"));

        cache.set("/string", "another string");
        assertEquals("string get() returned incorrect result;", "another string", cache.get("/string"));

        Object array = cache.get("/array");
        assertNotNull("array get() returned null;", array);
        assertTrue("array get() returned incorrect type;", array instanceof ArrayNode);

        cache.set("/array/0", 2);
        assertEquals("array get element returned incorrect result;", 2, cache.get("/array/0"));
        cache.set("/array/3", 8.0D);
        assertEquals("array get element returned incorrect result;", 8.0D, cache.get("/array/3"));

        Object object = cache.get("/object");
        assertNotNull("object get() returned null;", object);
        assertTrue("object get() returned incorrect type;", object instanceof ObjectNode);

        cache.set("/object/nestedBoolean", false);
        assertEquals("nested boolean get() returned incorrect result;", false, cache.get("/object/nestedBoolean"));

        cache.set("/object/nestedNumber", 1.618);
        assertEquals("nested number get() returned incorrect result;", 1.618, cache.get("/object/nestedNumber"));

        cache.set("/object/nestedString", "another nested string");
        assertEquals("nested string get() returned incorrect result;", "another nested string",
                cache.get("/object/nestedString"));

        Object nestedArray = cache.get("/object/nestedArray");
        assertNotNull("nested array get() returned null;", nestedArray);
        assertTrue("nested array get() returned incorrect type;", nestedArray instanceof ArrayNode);

        cache.set("/object/nestedArray/0", "x");
        assertEquals("nested array get element returned incorrect result;", "x", cache.get("/object/nestedArray/0"));
        cache.set("/object/nestedArray/3", "y");
        assertEquals("nested array get element returned incorrect result;", "y", cache.get("/object/nestedArray/3"));

        Object nestedObject = cache.get("/object/nestedObject");
        assertNotNull("object get() returned null;", nestedObject);
        assertTrue("object get() returned incorrect type;", nestedObject instanceof ObjectNode);

        assertTrue("cache should be dirty after writes;", root.isDirty());
    }

    @Test
    public void testSetBigDecimal() throws Exception {
        BigDecimal bd628 = new BigDecimal("6.28");
        cache.set("/number", bd628);
        assertEquals("getBigInteger() returned incorrect result;", bd628, cache.getBigDecimal("/number"));

        BigDecimal bd1618 = new BigDecimal("1.618");
        cache.set("/object/nestedNumber", bd1618);
        assertEquals("nested getBigInteger() returned incorrect result;", bd1618,
                cache.getBigDecimal("/object/nestedNumber"));

        assertTrue("cache should be dirty after BigDecimal writes;", root.isDirty());
    }

    @Test
    public void testSetBigInteger() throws Exception {
        BigInteger bi4 = new BigInteger("4");
        cache.set("/number", bi4);
        assertEquals("getBigInteger() returned incorrect result;", bi4, cache.getBigInteger("/number"));

        BigInteger bi6 = new BigInteger("6");
        cache.set("/object/nestedNumber", bi6);
        assertEquals("nested getBigInteger() returned incorrect result;", bi6,
                cache.getBigInteger("/object/nestedNumber"));

        assertTrue("cache should be dirty after BigInteger writes;", root.isDirty());
    }

    @Test
    public void testSetBoolean() throws Exception {
        cache.set("/boolean", false);
        assertEquals("getBoolean returned incorrect result;", false, cache.getBoolean("/boolean"));

        cache.set("/object/nestedBoolean", false);
        assertEquals("nested getBoolean returned incorrect result;", false, cache.getBoolean("/object/nestedBoolean"));

        assertTrue("cache should be dirty after boolean writes;", root.isDirty());
    }

    @Test
    public void testSetDate() throws Exception {
        Date date = new Date();
        cache.set("/dateTimeZ", date);
        assertEquals("getDate() returned incorrect result;", date, cache.getObject("/dateTimeZ", Date.class));

        cache.set("/object/nestedDateTimeZ", date);
        assertEquals("nested getDate() returned incorrect result;", date,
                cache.getObject("/object/nestedDateTimeZ", Date.class));

        assertTrue("cache should be dirty after date writes;", root.isDirty());
    }

    private void testSetDate0(String dateStr, Object date) throws CacheException {
        cache.set("/dateStr", dateStr);
        assertEquals("getObject(\"/dateStr\", " + date.getClass().getName() + ".class) returned incorrect result;",
                date, cache.getObject("/dateStr", date.getClass()));
        cache.set("/date", date);
        assertEquals("getObject(\"/date\", " + date.getClass().getName() + ".class) returned incorrect result;",
                date, cache.getObject("/dateStr", date.getClass()));
        assertTrue("cache should be dirty after date writes;", root.isDirty());
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
        assertEquals("getDouble() returned incorrect result;", 6.28D, cache.getDouble("/number"), Double.MIN_VALUE);

        cache.set("/object/nestedNumber", 1.618D);
        assertEquals("nested getDouble() returned incorrect result;", 1.618D, cache.getDouble("/object/nestedNumber"),
                Double.MIN_VALUE);

        assertTrue("cache should be dirty after double writes;", root.isDirty());
    }

    @Test
    public void testSetFloat() throws Exception {
        cache.set("/number", 6.28F);
        assertEquals("getFloat() returned incorrect result;", 6.28F, cache.getFloat("/number"), Float.MIN_VALUE);

        cache.set("/object/nestedNumber", 1.618F);
        assertEquals("nested getFloat() returned incorrect result;", 1.618F, cache.getFloat("/object/nestedNumber"),
                Float.MIN_VALUE);

        assertTrue("cache should be dirty after float writes;", root.isDirty());
    }

    @Test
    public void testSetInt() throws Exception {
        cache.set("/number", 23);
        assertEquals("getInt() returned incorrect result;", 23, cache.getInt("/number"));

        cache.set("/object/nestedNumber", 22);
        assertEquals("nested getInt() returned incorrect result;", 22, cache.getInt("/object/nestedNumber"));

        assertTrue("cache should be dirty after integer writes;", root.isDirty());
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
        assertEquals("getLong() returned incorrect result;", 33L, cache.getLong("/number"));

        cache.set("/object/nestedNumber", 32L);
        assertEquals("nested getLong() returned incorrect result;", 32L, cache.getLong("/object/nestedNumber"));

        assertTrue("cache should be dirty after long writes;", root.isDirty());
    }

    @Test
    public void testSetNumber() throws Exception {
        cache.set("/number", 6.28F);
        assertEquals("getNumber() returned incorrect result;", 6.28F, cache.getNumber("/number"));

        cache.set("/object/nestedNumber", 1.618D);
        assertEquals("nested getNumber() returned incorrect result;", 1.618D, cache.getNumber("/object/nestedNumber"));

        assertTrue("cache should be dirty after number writes;", root.isDirty());
    }

    @Test
    public void testSetObject() throws Exception {
        cache.set("/testObjects/0", TEST_OBJECT_1);
        TestObject testObject0 = cache.getObject("/testObjects/0", TestObject.class);
        assertSame("test object 0 was not set correctly", TEST_OBJECT_1, testObject0);
        assertNull("POJO field did not return null;", cache.get("/testObjects/0/booleanField"));

        cache.set("/testObjects/1", TEST_OBJECT_0);
        TestObject testObject1 = cache.getObject("/testObjects/1", TestObject.class);
        assertSame("test object 1 was not deserialized correctly", TEST_OBJECT_0, testObject1);
        assertNull("POJO field did not return null;", cache.get("/testObjects/1/stringField"));

        cache.set("/boolean", Boolean.FALSE);
        assertEquals("set(Boolean) did not work;", false, cache.getBoolean("/boolean"));

        cache.set("/number", Integer.valueOf(666));
        assertEquals("set(Integer) did not work;", 666, cache.getInt("/number"));

        cache.set("/string", (Object) "a replacement string");
        assertEquals("set(Object) did not work;", "a replacement string", cache.getString("/string"));

        // TODO: should we be able to set ArrayNode and ObjectNode values in this fashion?

        assertTrue("cache should be dirty after object writes;", root.isDirty());
    }

    @Test
    public void testSetObjects() throws Exception {
        List<?> testObjects = Arrays.asList(TEST_OBJECT_1, TEST_OBJECT_0);
        cache.set("/testObjects", testObjects);
        List<TestObject> result = cache.getObjects("/testObjects", TestObject.class);
        assertEquals("set(path, list) did not work", testObjects, result);
        assertTrue("cache should be dirty after set(path, list);", root.isDirty());
    }

    @Test
    public void testSetShort() throws Exception {
        cache.set("/number", 13);
        assertEquals("getShort() returned incorrect result;", 13, cache.getShort("/number"));

        cache.set("/object/nestedNumber", 12);
        assertEquals("nested getShort() returned incorrect result;", 12, cache.getShort("/object/nestedNumber"));

        assertTrue("cache should be dirty after short writes;", root.isDirty());
    }

    @Test
    public void testSetString() throws Exception {
        cache.set("/string", "a different string");
        assertEquals("getString returned incorrect result;", "a different string", cache.getString("/string"));

        cache.set("/object/nestedString", "a different nested string");
        assertEquals("nested getString returned incorrect result;", "a different nested string",
                cache.getString("/object/nestedString"));

        assertTrue("cache should be dirty after string writes;", root.isDirty());
    }

    @Test
    public void testSetWithMissingAncestors() throws Exception {
        JsonPointer ptr = JsonPointer.compile("/nonExistentArray/0/nonExistentObject/stringProperty");
        assertFalse("exists(ptr) returned incorrect value;", cache.exists(ptr));
        Object value = cache.get(ptr);
        assertNull("stringProperty is non-null;", value);
        cache.set(ptr, "string value");
        assertTrue("exists(ptr) returned incorrect value;", cache.exists(ptr));
        value = cache.get(ptr);
        assertEquals("stringProperty is null after being set;", "string value", value);
    }

    // TODO: get/set POJO
}
