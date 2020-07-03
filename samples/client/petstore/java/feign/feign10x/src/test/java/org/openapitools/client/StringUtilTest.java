package org.openapitools.client;

import org.junit.*;
import static org.junit.Assert.*;


public class StringUtilTest {
    @Test
    public void testContainsIgnoreCase() {
        assertTrue(StringUtil.containsIgnoreCase(new String[]{"abc"}, "abc"));
        assertTrue(StringUtil.containsIgnoreCase(new String[]{"abc"}, "ABC"));
        assertTrue(StringUtil.containsIgnoreCase(new String[]{"ABC"}, "abc"));
        assertTrue(StringUtil.containsIgnoreCase(new String[]{null, "abc"}, "ABC"));
        assertTrue(StringUtil.containsIgnoreCase(new String[]{null, "abc"}, null));

        assertFalse(StringUtil.containsIgnoreCase(new String[]{"abc"}, "def"));
        assertFalse(StringUtil.containsIgnoreCase(new String[]{}, "ABC"));
        assertFalse(StringUtil.containsIgnoreCase(new String[]{}, null));
    }

    @Test
    public void testJoin() {
        String[] array = {"aa", "bb", "cc"};
        assertEquals("aa,bb,cc", StringUtil.join(array, ","));
        assertEquals("aa, bb, cc", StringUtil.join(array, ", "));
        assertEquals("aabbcc", StringUtil.join(array, ""));
        assertEquals("aa bb cc", StringUtil.join(array, " "));
        assertEquals("aa\nbb\ncc", StringUtil.join(array, "\n"));

        assertEquals("", StringUtil.join(new String[]{}, ","));
        assertEquals("abc", StringUtil.join(new String[]{"abc"}, ","));
    }
}
