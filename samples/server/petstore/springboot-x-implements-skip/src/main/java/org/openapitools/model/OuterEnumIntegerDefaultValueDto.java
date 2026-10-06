package org.openapitools.model;

import com.fasterxml.jackson.annotation.JsonValue;
import jakarta.validation.constraints.*;

import java.util.*;
import jakarta.annotation.Generated;

import com.fasterxml.jackson.annotation.JsonCreator;

/**
 * Gets or Sets OuterEnumIntegerDefaultValue
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public enum OuterEnumIntegerDefaultValueDto {
  
  NUMBER_0(0),
  
  NUMBER_1(1),
  
  NUMBER_2(2);

  private final Integer value;

  OuterEnumIntegerDefaultValueDto(Integer value) {
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
  public static OuterEnumIntegerDefaultValueDto fromValue(Integer value) {
    for (OuterEnumIntegerDefaultValueDto b : OuterEnumIntegerDefaultValueDto.values()) {
      if (b.value.equals(value)) {
        return b;
      }
    }
    throw new IllegalArgumentException("Unexpected value '" + value + "'");
  }
}

