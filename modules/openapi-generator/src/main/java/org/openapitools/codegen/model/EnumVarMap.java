package org.openapitools.codegen.model;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

public class EnumVarMap extends HashMap<String, Object> {

    // The raw enum values from the OpenAPI specification
    public static final String ENUM_VALUES = "values";
    // The map that stores all enum values and their metadata (name, value, enumDescription...)
    public static final String ENUM_VARS = "enumVars";
    // The name of the enum, for example NAME("value") in Java
    public static final String ENUM_NAME = "name";
    // The on-the-line value, i.e., the one present in the "values"
    public static final String ENUM_VALUE = "value";
    // If the enum is typed as a string
    public static final String ENUM_IS_STRING = "isString";
    // The description that should be attached to an entry in "enumVars"
    public static final String ENUM_DESCRIPTION = "enumDescription";

    public EnumVarMap() {

    }

    public EnumVarMap(EnumVarMap init) {
        putAll(init);
    }

    public EnumVarMap(Map<String, String> init) {
        putAll(init);
    }

    public void enumVar(String enumName, String enumValue, boolean isString) {
        put(ENUM_NAME, enumName);
        put(ENUM_VALUE, enumValue);
        put(ENUM_IS_STRING, isString);
    }

    public void setEnumName(String name) {
        put(ENUM_NAME, name);
    }

    public Object getEnumName() {
        return get(ENUM_NAME);
    }

    public void setEnumValue(String value) {
        put(ENUM_VALUE, value);
    }

    public Object getEnumValue() {
        return get(ENUM_VALUE);
    }

    public void isString(boolean isString) {
        put(ENUM_IS_STRING, isString);
    }

    public boolean isString() {
        Object value = get(ENUM_IS_STRING);
        if (!(value instanceof Boolean)) {
            throw new IllegalStateException(ENUM_IS_STRING + " is not a boolean: " + value);
        }
        return (Boolean) value;
    }

}
