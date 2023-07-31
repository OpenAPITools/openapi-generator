package org.openapitools.server.model;



import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

/**
 * Gets or Sets EnumClass
 */

public enum EnumClass {

    _ABC("_abc"),
    _EFG("-efg"),
    _XYZ_("(xyz)");

    private String value;

    EnumClass(String value) {
        this.value = value;
    }

    @JsonValue
    public String getValue() {
        return value;
    }

    @Override
    public String toString() {
        return String.valueOf(value);
    }

    @JsonCreator
    public static EnumClass fromValue(String text) {
        for (EnumClass b : EnumClass.values()) {
            if (String.valueOf(b.value).equals(text)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected value '" + text + "'");
    }
}

