package org.openapitools.codegen;

import org.openapitools.codegen.utils.MetariscVisibilityTrait;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.HashMap;
import java.util.Map;

@Test()
public class MetariscVisibilityTraitTest {
    @Test
    public void visibilityCheckPublicTrueTest() {
        Map<String, Object> vendorExtensions = new HashMap<>();
        vendorExtensions.put("x-extension-metarisc-codegen-visibility", "public");

        String[] visibilty = new String[]{"public"};

        Assert.assertTrue(MetariscVisibilityTrait.visibilityCheck(vendorExtensions, visibilty));
    }

    @Test
    public void visibilityCheckPublicFalseTest() {
        Map<String, Object> vendorExtensions = new HashMap<>();
        vendorExtensions.put("x-extension-metarisc-codegen-visibility", "public");

        String[] visibilty = new String[]{"interne"};

        Assert.assertFalse(MetariscVisibilityTrait.visibilityCheck(vendorExtensions, visibilty));
    }

    @Test
    public void visibilityCheckInterneFalseTest() {
        Map<String, Object> vendorExtensions = new HashMap<>();
        vendorExtensions.put("x-extension-metarisc-codegen-visibility", "interne");

        String[] visibilty = new String[]{"public"};

        Assert.assertFalse(MetariscVisibilityTrait.visibilityCheck(vendorExtensions, visibilty));
    }

    @Test
    public void visibilityCheckInterneTrueTest() {
        Map<String, Object> vendorExtensions = new HashMap<>();
        vendorExtensions.put("x-extension-metarisc-codegen-visibility", "interne");

        String[] visibilty = new String[]{"interne"};

        Assert.assertTrue(MetariscVisibilityTrait.visibilityCheck(vendorExtensions, visibilty));
    }

    @Test
    public void visibilityCheckDefaultPublicTest() {
        Map<String, Object> vendorExtensions = new HashMap<>();
        vendorExtensions.put("x-extension-metarisc-codegen-visibility", "");

        String[] visibilty = new String[]{""};

        Assert.assertTrue(MetariscVisibilityTrait.visibilityCheck(vendorExtensions, visibilty));
    }
}
