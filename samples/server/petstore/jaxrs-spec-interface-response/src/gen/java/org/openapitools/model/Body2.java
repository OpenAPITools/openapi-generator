package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.ArrayList;
import java.util.List;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;



public class Body2  implements Serializable {
  
 
public enum EnumFormStringArrayEnum {

    GREATER_THAN(String.valueOf(">")), DOLLAR(String.valueOf("$"));


    private String value;

    EnumFormStringArrayEnum (String v) {
        value = v;
    }

    public String value() {
        return value;
    }

    @Override
    @JsonValue
    public String toString() {
        return String.valueOf(value);
    }

    @JsonCreator
    public static EnumFormStringArrayEnum fromValue(String v) {
        for (EnumFormStringArrayEnum b : EnumFormStringArrayEnum.values()) {
            if (String.valueOf(b.value).equals(v)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected value '" + v + "'");
    }
}

  private @Valid List<EnumFormStringArrayEnum> enumFormStringArray = new ArrayList<EnumFormStringArrayEnum>();

public enum EnumFormStringEnum {

    _ABC(String.valueOf("_abc")), _EFG(String.valueOf("-efg")), _XYZ_(String.valueOf("(xyz)"));


    private String value;

    EnumFormStringEnum (String v) {
        value = v;
    }

    public String value() {
        return value;
    }

    @Override
    @JsonValue
    public String toString() {
        return String.valueOf(value);
    }

    @JsonCreator
    public static EnumFormStringEnum fromValue(String v) {
        for (EnumFormStringEnum b : EnumFormStringEnum.values()) {
            if (String.valueOf(b.value).equals(v)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected value '" + v + "'");
    }
}

  private @Valid EnumFormStringEnum enumFormString = EnumFormStringEnum._EFG;

  /**
   * Form parameter enum test (string array)
   **/
  public Body2 enumFormStringArray(List<EnumFormStringArrayEnum> enumFormStringArray) {
    this.enumFormStringArray = enumFormStringArray;
    return this;
  }

  
  @ApiModelProperty(value = "Form parameter enum test (string array)")
  @JsonProperty("enum_form_string_array")
  public List<EnumFormStringArrayEnum> getEnumFormStringArray() {
    return enumFormStringArray;
  }
  public void setEnumFormStringArray(List<EnumFormStringArrayEnum> enumFormStringArray) {
    this.enumFormStringArray = enumFormStringArray;
  }

  /**
   * Form parameter enum test (string)
   **/
  public Body2 enumFormString(EnumFormStringEnum enumFormString) {
    this.enumFormString = enumFormString;
    return this;
  }

  
  @ApiModelProperty(value = "Form parameter enum test (string)")
  @JsonProperty("enum_form_string")
  public EnumFormStringEnum getEnumFormString() {
    return enumFormString;
  }
  public void setEnumFormString(EnumFormStringEnum enumFormString) {
    this.enumFormString = enumFormString;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Body2 body2 = (Body2) o;
    return Objects.equals(enumFormStringArray, body2.enumFormStringArray) &&
        Objects.equals(enumFormString, body2.enumFormString);
  }

  @Override
  public int hashCode() {
    return Objects.hash(enumFormStringArray, enumFormString);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Body2 {\n");
    
    sb.append("    enumFormStringArray: ").append(toIndentedString(enumFormStringArray)).append("\n");
    sb.append("    enumFormString: ").append(toIndentedString(enumFormString)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

