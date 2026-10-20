package org.openapitools.model;

import org.junit.Test;

import static org.junit.Assert.assertSame;

public class StringEnumTest {

    @Test
    public void fromValueUsesCaseInsensitiveMatching() {
        assertSame(StringEnum.FOO, StringEnum.fromValue("FOO"));
        assertSame(StringEnum.BAR, StringEnum.fromValue("BaR"));
        assertSame(StringEnum.BAZ, StringEnum.fromValue("baz"));
    }
}
