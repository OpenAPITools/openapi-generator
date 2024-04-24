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
        Assertions.assertEquals(underscore("abcd"), "abcd");
        Assertions.assertEquals(underscore("abCd"), "ab_cd");
        Assertions.assertEquals(underscore("ListABCs"), "list_abcs");
    }

    @Test
    public void testCamelize() throws Exception {
        Assertions.assertEquals(camelize("abcd"), "Abcd");
        Assertions.assertEquals(camelize("some-value"), "SomeValue");
        Assertions.assertEquals(camelize("some_value"), "SomeValue");
        Assertions.assertEquals(camelize("$type"), "$Type");

        Assertions.assertEquals(camelize("abcd", LOWERCASE_FIRST_LETTER), "abcd");
        Assertions.assertEquals(camelize("some-value", LOWERCASE_FIRST_LETTER), "someValue");
        Assertions.assertEquals(camelize("some_value", LOWERCASE_FIRST_LETTER), "someValue");
        Assertions.assertEquals(camelize("Abcd", LOWERCASE_FIRST_LETTER), "abcd");
        Assertions.assertEquals(camelize("$type", LOWERCASE_FIRST_LETTER), "$type");

        Assertions.assertEquals(camelize("123", LOWERCASE_FIRST_LETTER), "123");
        Assertions.assertEquals(camelize("$123", LOWERCASE_FIRST_LETTER), "$123");


        Assertions.assertEquals(camelize("some-value", LOWERCASE_FIRST_CHAR), "someValue");
        Assertions.assertEquals(camelize("$type", LOWERCASE_FIRST_CHAR), "$Type");
    }

    @Test
    public void testDashize() {
        Assertions.assertEquals(dashize("abcd"), "abcd");
        Assertions.assertEquals(dashize("some-value"), "some-value");
        Assertions.assertEquals(dashize("some_value"), "some-value");
        Assertions.assertEquals(dashize("Foo_Response__links"), "foo-response-links");
        Assertions.assertEquals(dashize("Foo Response _links"), "foo-response-links");
    }
}
