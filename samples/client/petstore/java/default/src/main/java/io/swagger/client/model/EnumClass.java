package io.swagger.client.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonValue;


/**
 * Gets or Sets EnumClass
 */
public enum EnumClass {
  _ABC("_abc"),
  _EFG("-efg"),
  _XYZ_("(xyz)");

  private String value;

  EnumClass(String value) {
    this.value = value;
  }

  @Override
  @JsonValue
  public String toString() {
    return String.valueOf(value);
  }
}

