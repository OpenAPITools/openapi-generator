package io.swagger.client.model;

import java.util.Objects;

import com.google.gson.annotations.SerializedName;


/**
 * Gets or Sets EnumClass
 */
public enum EnumClass {
  @SerializedName("_abc")
  _ABC("_abc"),

  @SerializedName("-efg")
  _EFG("-efg"),

  @SerializedName("(xyz)")
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

