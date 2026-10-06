package org.openapitools.model;

import com.fasterxml.jackson.annotation.JsonValue;
import jakarta.validation.constraints.*;

import java.util.*;
import jakarta.annotation.Generated;

import com.fasterxml.jackson.annotation.JsonCreator;

/**
 * Gets or Sets OuterEnumDefaultValue
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public enum OuterEnumDefaultValueDto {
  
  PLACED("placed"),
  
  APPROVED("approved"),
  
  DELIVERED("delivered");

  private final String value;

  OuterEnumDefaultValueDto(String value) {
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
  public static OuterEnumDefaultValueDto fromValue(String value) {
    for (OuterEnumDefaultValueDto b : OuterEnumDefaultValueDto.values()) {
      if (b.value.equals(value)) {
        return b;
      }
    }
    throw new IllegalArgumentException("Unexpected value '" + value + "'");
  }
}

