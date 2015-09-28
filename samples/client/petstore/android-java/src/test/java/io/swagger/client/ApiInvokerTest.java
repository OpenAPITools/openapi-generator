package io.swagger.client;

import org.junit.*;
import static org.junit.Assert.*;

import java.util.*;


public class ApiInvokerTest {

    @Test
    public void testParameterToPairsWhenNameIsInvalid() throws Exception {
        List<Pair> pairs_a = ApiInvoker.parameterToPairs("csv", null, new Integer(1));
        List<Pair> pairs_b = ApiInvoker.parameterToPairs("csv", "", new Integer(1));

        assertTrue(pairs_a.isEmpty());
        assertTrue(pairs_b.isEmpty());
    }

    @Test
    public void testParameterToPairsWhenValueIsNull() throws Exception {
        List<Pair> pairs = ApiInvoker.parameterToPairs("csv", "param-a", null);

        assertTrue(pairs.isEmpty());
    }

    @Test
    public void testParameterToPairsWhenValueIsEmptyStrings() throws Exception {

        // single empty string
        List<Pair> pairs = ApiInvoker.parameterToPairs("csv", "param-a", " ");
        assertEquals(1, pairs.size());

        // list of empty strings
        List<String> strs = new ArrayList<String>();
        strs.add(" ");
        strs.add(" ");
        strs.add(" ");

        List<Pair> concatStrings = ApiInvoker.parameterToPairs("csv", "param-a", strs);

        assertEquals(1, concatStrings.size());
        assertFalse(concatStrings.get(0).getValue().isEmpty()); // should contain some delimiters
    }

    @Test
    public void testParameterToPairsWhenValueIsNotCollection() throws Exception {
        String name = "param-a";
        Integer value = 1;

        List<Pair> pairs = ApiInvoker.parameterToPairs("csv", name, value);

        assertEquals(1, pairs.size());
        assertEquals(value, Integer.valueOf(pairs.get(0).getValue()));
    }

    @Test
    public void testParameterToPairsWhenValueIsCollection() throws Exception {
        Map<String, String> collectionFormatMap = new HashMap<String, String>();
        collectionFormatMap.put("csv", ",");
        collectionFormatMap.put("tsv", "\t");
        collectionFormatMap.put("ssv", " ");
        collectionFormatMap.put("pipes", "\\|");
        collectionFormatMap.put("", ","); // no format, must default to csv
        collectionFormatMap.put("unknown", ","); // all other formats, must default to csv

        String name = "param-a";

        List<Object> values = new ArrayList<Object>();
        values.add("value-a");
        values.add(123);
        values.add(new Date());

        // check for multi separately
        List<Pair> multiPairs = ApiInvoker.parameterToPairs("multi", name, values);
        assertEquals(values.size(), multiPairs.size());

        // all other formats
        for (String collectionFormat : collectionFormatMap.keySet()) {
            List<Pair> pairs = ApiInvoker.parameterToPairs(collectionFormat, name, values);

            assertEquals(1, pairs.size());

            String delimiter = collectionFormatMap.get(collectionFormat);
            String[] pairValueSplit = pairs.get(0).getValue().split(delimiter);

            // must equal input values
            assertEquals(values.size(), pairValueSplit.length);
        }
    }
}
