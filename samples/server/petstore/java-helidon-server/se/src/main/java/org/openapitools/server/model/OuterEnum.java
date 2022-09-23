package org.openapitools.server.model;



import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

/**
 * Gets or Sets OuterEnum
 */

public enum OuterEnum {

    PLACED("placed"),
    APPROVED("approved"),
    DELIVERED("delivered");

    private String value;

    OuterEnum(String value) {
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
    public static OuterEnum fromValue(String text) {
        for (OuterEnum b : OuterEnum.values()) {
            if (String.valueOf(b.value).equals(text)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected value '" + text + "'");
    }
}

