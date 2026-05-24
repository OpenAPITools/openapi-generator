package org.openapitools.model;

import javax.validation.constraints.*;
import javax.validation.Valid;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

/**
 * Gets or Sets OuterEnum
 */
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaCXFServerCodegen", comments = "Generator version: 7.23.0-SNAPSHOT")
public enum OuterEnum {
  
  PLACED("placed"),
  
  APPROVED("approved"),
  
  DELIVERED("delivered");

  private String value;

  OuterEnum(String value) {
    this.value = value;
  }

  @Override
  @JsonValue
  public String toString() {
    return String.valueOf(value);
  }

  @JsonCreator
  public static OuterEnum fromValue(String value) {
    for (OuterEnum b : OuterEnum.values()) {
      if (b.value.equals(value)) {
        return b;
      }
    }
    throw new IllegalArgumentException("Unexpected value '" + value + "'");
  }

}

