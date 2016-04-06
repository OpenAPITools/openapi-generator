package io.swagger.codegen.cmd.utils;

import io.swagger.codegen.utils.OptionUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotNull;

@SuppressWarnings("static-method")
public class OptionUtilsTest {

    @Test
    public void splitCommaSeparatedList() throws Exception {
        doCommaSeparatedListTest("a,b,c", Arrays.asList("a", "b", "c"));
        doCommaSeparatedListTest("a,,c", Arrays.asList("a", "c"));
        doCommaSeparatedListTest("", new ArrayList<String>());
        doCommaSeparatedListTest(null, new ArrayList<String>());
    }

    @Test
    public void testParseCommaSeparatedTuples() throws Exception {
        doTupleListTest("a=1,b=2,c=3", Arrays.asList(Pair.of("a", "1"), Pair.of("b", "2"), Pair.of("c", "3")));
        doTupleListTest("a=1,,c=3", Arrays.asList(Pair.of("a", "1"), Pair.of("c", "3")));
        doTupleListTest("a=1,xyz,c=3", Arrays.asList(Pair.of("a", "1"), Pair.of("c", "3")));
        doTupleListTest("a=1,=,c=3", Arrays.asList(Pair.of("a", "1"), Pair.of("c", "3")));
        doTupleListTest("", new ArrayList<Pair<String, String>>());
        doTupleListTest(null, new ArrayList<Pair<String, String>>());
    }

    private static void doTupleListTest(String input, List<Pair<String, String>> expectedResults) {
        final List<Pair<String, String>> result = OptionUtils.parseCommaSeparatedTuples(input);
        assertNotNull(result);
        assertEquals(result.size(), expectedResults.size());
        for (int i = 0; i < expectedResults.size(); i++) {
            final Pair<String, String> actualPair = result.get(i);
            final Pair<String, String> expected = expectedResults.get(i);
            assertEquals(actualPair, expected);
        }
    }

    private static void doCommaSeparatedListTest(String csvStr, List<String> expectedResults) {
        final List<String> result = OptionUtils.splitCommaSeparatedList(csvStr);
        assertNotNull(result);
        assertEquals(result.size(), expectedResults.size());
        for (int i = 0; i < expectedResults.size(); i++) {
            assertEquals(result.get(i), expectedResults.get(i));
        }
    }
}
