package org.openapitools.server.model;



import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

/**
 * Gets or Sets SingleRefType
 */

public enum SingleRefType {

    ADMIN("admin"),
    USER("user");

    private String value;

    SingleRefType(String value) {
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
    public static SingleRefType fromValue(String text) {
        for (SingleRefType b : SingleRefType.values()) {
            if (String.valueOf(b.value).equals(text)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected value '" + text + "'");
    }
}

