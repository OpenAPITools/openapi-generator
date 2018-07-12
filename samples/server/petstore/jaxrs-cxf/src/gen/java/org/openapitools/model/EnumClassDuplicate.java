package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import javax.validation.constraints.*;
import javax.validation.Valid;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

/**
 * This schema is to test duplicate constant names. To be used with &#x60;EnumClass&#x60;.
 */
public enum EnumClassDuplicate {
  
  _ABC("_abc"),
  
  _EFG("-efg"),
  
  _XYZ_("(xyz)");

  private String value;

  EnumClassDuplicate(String value) {
    this.value = value;
  }

  @Override
  @JsonValue
  public String toString() {
    return String.valueOf(value);
  }

  @JsonCreator
  public static EnumClassDuplicate fromValue(String text) {
    for (EnumClassDuplicate b : EnumClassDuplicate.values()) {
      if (String.valueOf(b.value).equals(text)) {
        return b;
      }
    }
    return null;
  }
  
}

