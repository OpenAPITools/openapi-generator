package org.openapitools.server.model;



import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

/**
 * Gets or Sets OuterEnumDefaultValue
 */

public enum OuterEnumDefaultValue {

    PLACED("placed"),
    APPROVED("approved"),
    DELIVERED("delivered");

    private String value;

    OuterEnumDefaultValue(String value) {
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
    public static OuterEnumDefaultValue fromValue(String text) {
        for (OuterEnumDefaultValue b : OuterEnumDefaultValue.values()) {
            if (String.valueOf(b.value).equals(text)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected value '" + text + "'");
    }
}

