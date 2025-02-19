package org.openapitools.codegen.utils;

import org.testng.Assert;
import org.testng.annotations.Test;

import static org.openapitools.codegen.utils.CamelizeOption.LOWERCASE_FIRST_CHAR;
import static org.openapitools.codegen.utils.CamelizeOption.LOWERCASE_FIRST_LETTER;
import static org.openapitools.codegen.utils.StringUtils.*;

public class StringUtilsTest {
    // we'll assume that <i>underscore</i> (Twitter elephant bird) works fine
    @Test
    public void testUnderscore() {
        Assert.assertEquals(underscore("abcd"), "abcd");
        Assert.assertEquals(underscore("abCd"), "ab_cd");
        Assert.assertEquals(underscore("ListABCs"), "list_abcs");
    }

    @Test
    public void testCamelize() throws Exception {
        Assert.assertEquals(camelize("abcd"), "Abcd");
        Assert.assertEquals(camelize("some-value"), "SomeValue");
        Assert.assertEquals(camelize("some_value"), "SomeValue");
        Assert.assertEquals(camelize("$type"), "$Type");

        Assert.assertEquals(camelize("abcd", LOWERCASE_FIRST_LETTER), "abcd");
        Assert.assertEquals(camelize("some-value", LOWERCASE_FIRST_LETTER), "someValue");
        Assert.assertEquals(camelize("some_value", LOWERCASE_FIRST_LETTER), "someValue");
        Assert.assertEquals(camelize("Abcd", LOWERCASE_FIRST_LETTER), "abcd");
        Assert.assertEquals(camelize("$type", LOWERCASE_FIRST_LETTER), "$type");

        Assert.assertEquals(camelize("123", LOWERCASE_FIRST_LETTER), "123");
        Assert.assertEquals(camelize("$123", LOWERCASE_FIRST_LETTER), "$123");


        Assert.assertEquals(camelize("some-value", LOWERCASE_FIRST_CHAR), "someValue");
        Assert.assertEquals(camelize("$type", LOWERCASE_FIRST_CHAR), "$Type");
    }

    @Test
    public void testDashize() {
        Assert.assertEquals(dashize("abcd"), "abcd");
        Assert.assertEquals(dashize("some-value"), "some-value");
        Assert.assertEquals(dashize("some_value"), "some-value");
        Assert.assertEquals(dashize("Foo_Response__links"), "foo-response-links");
        Assert.assertEquals(dashize("Foo Response _links"), "foo-response-links");
    }
}
