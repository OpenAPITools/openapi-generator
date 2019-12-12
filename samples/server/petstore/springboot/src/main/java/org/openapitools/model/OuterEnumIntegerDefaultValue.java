package org.openapitools.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonValue;
import org.openapitools.jackson.nullable.JsonNullable;
import javax.validation.Valid;
import javax.validation.constraints.*;

import com.fasterxml.jackson.annotation.JsonCreator;

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

  @Override
  @JsonValue
  public String toString() {
    return String.valueOf(value);
  }

  @JsonCreator
  public static OuterEnumIntegerDefaultValue fromValue(Integer value) {
    for (OuterEnumIntegerDefaultValue b : OuterEnumIntegerDefaultValue.values()) {
      if (b.value.equals(value)) {
        return b;
      }
    }
    throw new IllegalArgumentException("Unexpected value '" + value + "'");
  }
}

