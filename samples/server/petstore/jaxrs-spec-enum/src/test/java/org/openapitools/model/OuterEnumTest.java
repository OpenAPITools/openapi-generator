package org.openapitools.model;

import org.junit.Test;

import static org.junit.Assert.assertSame;

public class OuterEnumTest {

    @Test
    public void fromValueUsesCaseInsensitiveMatching() {
        assertSame(OuterEnum.PLACED, OuterEnum.fromValue("PLACED"));
        assertSame(OuterEnum.APPROVED, OuterEnum.fromValue("ApPrOvEd"));
        assertSame(OuterEnum.DELIVERED, OuterEnum.fromValue("delivered"));
    }
}
