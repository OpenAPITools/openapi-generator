package org.openapitools.codegen.utils;

import org.testng.Assert;
import org.testng.annotations.Test;

import static org.openapitools.codegen.utils.StringUtils.*;

import java.util.HashMap;
import java.util.Map;

public class StringUtilsTest {
    // we'll assume that <i>underscore</i> (Twitter elephant bird) works fine
    @Test
    public void testUnderscore() {
        Assert.assertEquals(underscore("abcd"), "abcd");
        Assert.assertEquals(underscore("abCd"), "ab_cd");
    }

    @Test
    public void testCamelize() throws Exception {
        Assert.assertEquals(camelize("abcd"), "Abcd");
        Assert.assertEquals(camelize("some-value"), "SomeValue");
        Assert.assertEquals(camelize("some_value"), "SomeValue");
        Assert.assertEquals(camelize("$type"), "$Type");

        Assert.assertEquals(camelize("abcd", true), "abcd");
        Assert.assertEquals(camelize("some-value", true), "someValue");
        Assert.assertEquals(camelize("some_value", true), "someValue");
        Assert.assertEquals(camelize("Abcd", true), "abcd");
        Assert.assertEquals(camelize("$type", true), "$type");

        Assert.assertEquals(camelize("123", true), "123");
        Assert.assertEquals(camelize("$123", true), "$123");

        Map<String, String> exc = new HashMap<String, String>();
        exc.put("Id", "ID");
        exc.put("Api", "API");
        exc.put("Xml", "XML");
        exc.put("Http", "HTTP");
        Assert.assertEquals(camelize("name_id", exc), "NameID");
        Assert.assertEquals(camelize("some-api", exc), "SomeAPI");
        Assert.assertEquals(camelize("id-name", exc), "IDName");
        Assert.assertEquals(camelize("id-name", true, exc), "idName");
        Assert.assertEquals(camelize("xml-http-request", exc), "XMLHTTPRequest");
        Assert.assertEquals(camelize("xml-http-request", true, exc), "xmlHTTPRequest");
    }

    @Test
    public void testDashize() {
        Assert.assertEquals(dashize("abcd"), "abcd");
        Assert.assertEquals(dashize("some-value"), "some-value");
        Assert.assertEquals(dashize("some_value"), "some-value");
    }
}
