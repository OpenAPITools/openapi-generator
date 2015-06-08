package io.swagger.codegen;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

import java.util.HashMap;
import java.util.Map;

public enum CodegenType {
    CLIENT, SERVER, DOCUMENTATION, OTHER;

    private static Map<String, CodegenType> names = new HashMap<String, CodegenType>();

    @JsonCreator
    public static CodegenType forValue(String value) {
        return names.get(value.toLowerCase());
    }

    @JsonValue
    public String toValue() {
        for (Map.Entry<String, CodegenType> entry : names.entrySet()) {
            if (entry.getValue() == this) {
                return entry.getKey();
            }
        }

        return null; // or fail
    }

    static {
        names.put("client", CLIENT);
        names.put("server", SERVER);
        names.put("documentation", DOCUMENTATION);
        names.put("other", OTHER);
    }
}