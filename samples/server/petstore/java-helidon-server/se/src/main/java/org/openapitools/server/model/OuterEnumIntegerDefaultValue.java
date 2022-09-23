package org.openapitools.server.model;



import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

/**
 * Gets or Sets OuterEnumIntegerDefaultValue
 */

public enum OuterEnumIntegerDefaultValue {

    NUMBER_0(0),
    NUMBER_1(1),
    NUMBER_2(2);

    private Integer value;

    OuterEnumIntegerDefaultValue(Integer value) {
        this.value = value;
    }

    @JsonValue
    public Integer getValue() {
        return value;
    }

    @Override
    public String toString() {
        return String.valueOf(value);
    }

    @JsonCreator
    public static OuterEnumIntegerDefaultValue fromValue(String text) {
        for (OuterEnumIntegerDefaultValue b : OuterEnumIntegerDefaultValue.values()) {
            if (String.valueOf(b.value).equals(text)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected value '" + text + "'");
    }
}

