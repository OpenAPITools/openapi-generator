package org.openapitools.codegen;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.Map;

public class ObjectWithTypeBooleans {
    public boolean isUnboundedInteger;
    public boolean isNumber;
    public boolean isString;
    public boolean isMultilineString;
    public boolean isMap;
    public boolean isArray;
    public boolean isBoolean;
    public boolean isNull;
    public Object value;

    public ObjectWithTypeBooleans(Object value) {
        Object usedValue = null;
        if (value instanceof Integer){
            this.isUnboundedInteger = true;
            this.value = value;
        } else if (value instanceof Double || value instanceof Float){
            this.isNumber = true;
            this.value = value;
        } else if (value instanceof String) {
            this.isString =  true;
            this.value = value;
            if (((String) value).contains("\n") || ((String) value).contains("\r")) {
                this.isMultilineString = true;
            }
        } else if (value instanceof LinkedHashMap) {
            LinkedHashMap<String, Object> castValue = (LinkedHashMap<String, Object>) value;
            LinkedHashMap<String, ObjectWithTypeBooleans> castMap = new LinkedHashMap<>();
            for (Map.Entry entry: castValue.entrySet()) {
                String entryKey = (String) entry.getKey();
                ObjectWithTypeBooleans entryValue = new ObjectWithTypeBooleans(entry.getValue());
                castMap.put(entryKey, entryValue);
            }
            this.value = castMap;
            this.isMap = true;
        } else if (value instanceof ArrayList) {
            ArrayList<ObjectWithTypeBooleans> castList = new ArrayList<>();
            for (Object item: (ArrayList<Object>) value) {
                castList.add(new ObjectWithTypeBooleans(item));
            }
            this.value = castList;
            this.isArray = true;
        } else if (value instanceof Boolean) {
            this.isBoolean = true;
            this.value = value;
        } else if (value == null) {
            this.isNull = true;
            this.value = value;
        }
    }
}

