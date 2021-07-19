package org.openapitools.model;


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

  @Override
  @JsonValue
  public String toString() {
    return String.valueOf(value);
  }

  @JsonCreator
  public static OuterEnumDefaultValue fromValue(String value) {
    for (OuterEnumDefaultValue b : OuterEnumDefaultValue.values()) {
      if (b.value.equals(value)) {
        return b;
      }
    }
    throw new IllegalArgumentException("Unexpected value '" + value + "'");
  }
  
}

