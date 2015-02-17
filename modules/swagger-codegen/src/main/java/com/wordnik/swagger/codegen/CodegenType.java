package com.wordnik.swagger.codegen;

import com.fasterxml.jackson.annotation.*;

import java.util.Map;
import java.util.HashMap;

public enum CodegenType {
  CLIENT, SERVER, DOCUMENTATION, OTHER;

  private static Map<String, CodegenType> names = new HashMap<String, CodegenType>();

  static {
    names.put("client", CLIENT);
    names.put("server", SERVER);
    names.put("documentation", DOCUMENTATION);
    names.put("other", OTHER);
  }

  @JsonCreator
  public static CodegenType forValue(String value) {
    return names.get(value.toLowerCase());
  }

  @JsonValue
  public String toValue() {
    for (Map.Entry<String, CodegenType> entry : names.entrySet()) {
      if (entry.getValue() == this)
        return entry.getKey();
    }

    return null; // or fail
  }
}