package org.openapitools.model;

import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;


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
  public String toString() {
    return String.valueOf(value);
  }

}


