package org.openapitools.codegen.utils;

import java.util.Arrays;
import java.util.Map;

public class MetariscVisibilityTrait {
    public static boolean visibilityCheck(Map<String, Object> vendorExtensions, String[] visibility) {
        String extensionName = "x-extension-metarisc-codegen-visibility";
        Object extensionValue = vendorExtensions.get(extensionName);
        String extensionValueString;

        extensionValueString = extensionValue == null ? "public" : Arrays.toString(extensionValue.toString().split("\\|"));

        for (String visibilityItem : visibility) {
            if (extensionValueString.contains(visibilityItem)) {
                return true;
            }
        }

        return false;
    }
}
