package org.openapitools.model;

import org.junit.Test;

import static org.junit.Assert.assertSame;

public class EnumTestTest {

    @Test
    public void enumStringFromValueUsesCaseInsensitiveMatching() {
        assertSame(EnumTest.EnumStringEnum.UPPER, EnumTest.EnumStringEnum.fromValue("UPPER"));
        assertSame(EnumTest.EnumStringEnum.LOWER, EnumTest.EnumStringEnum.fromValue("LoWeR"));
        assertSame(EnumTest.EnumStringEnum.EMPTY, EnumTest.EnumStringEnum.fromValue(""));
    }
}
