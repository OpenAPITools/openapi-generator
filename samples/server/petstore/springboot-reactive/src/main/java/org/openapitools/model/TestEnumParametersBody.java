package org.openapitools.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.ArrayList;
import java.util.List;
import org.openapitools.jackson.nullable.JsonNullable;
import javax.validation.Valid;
import javax.validation.constraints.*;

/**
 * TestEnumParametersBody
 */

public class TestEnumParametersBody   {
  /**
   * Gets or Sets enumFormStringArray
   */
  public enum EnumFormStringArrayEnum {
    GREATER_THAN(">"),
    
    DOLLAR("$");

    private String value;

    EnumFormStringArrayEnum(String value) {
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
    public static EnumFormStringArrayEnum fromValue(String value) {
      for (EnumFormStringArrayEnum b : EnumFormStringArrayEnum.values()) {
        if (b.value.equals(value)) {
          return b;
        }
      }
      throw new IllegalArgumentException("Unexpected value '" + value + "'");
    }
  }

  @JsonProperty("enum_form_string_array")
  @Valid
  private List<EnumFormStringArrayEnum> enumFormStringArray = null;

  /**
   * Form parameter enum test (string)
   */
  public enum EnumFormStringEnum {
    _ABC("_abc"),
    
    _EFG("-efg"),
    
    _XYZ_("(xyz)");

    private String value;

    EnumFormStringEnum(String value) {
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
    public static EnumFormStringEnum fromValue(String value) {
      for (EnumFormStringEnum b : EnumFormStringEnum.values()) {
        if (b.value.equals(value)) {
          return b;
        }
      }
      throw new IllegalArgumentException("Unexpected value '" + value + "'");
    }
  }

  @JsonProperty("enum_form_string")
  private EnumFormStringEnum enumFormString = EnumFormStringEnum._EFG;

  public TestEnumParametersBody enumFormStringArray(List<EnumFormStringArrayEnum> enumFormStringArray) {
    this.enumFormStringArray = enumFormStringArray;
    return this;
  }

  public TestEnumParametersBody addEnumFormStringArrayItem(EnumFormStringArrayEnum enumFormStringArrayItem) {
    if (this.enumFormStringArray == null) {
      this.enumFormStringArray = new ArrayList<>();
    }
    this.enumFormStringArray.add(enumFormStringArrayItem);
    return this;
  }

  /**
   * Form parameter enum test (string array)
   * @return enumFormStringArray
  */
  @ApiModelProperty(value = "Form parameter enum test (string array)")


  public List<EnumFormStringArrayEnum> getEnumFormStringArray() {
    return enumFormStringArray;
  }

  public void setEnumFormStringArray(List<EnumFormStringArrayEnum> enumFormStringArray) {
    this.enumFormStringArray = enumFormStringArray;
  }

  public TestEnumParametersBody enumFormString(EnumFormStringEnum enumFormString) {
    this.enumFormString = enumFormString;
    return this;
  }

  /**
   * Form parameter enum test (string)
   * @return enumFormString
  */
  @ApiModelProperty(value = "Form parameter enum test (string)")


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
    TestEnumParametersBody testEnumParametersBody = (TestEnumParametersBody) o;
    return Objects.equals(this.enumFormStringArray, testEnumParametersBody.enumFormStringArray) &&
        Objects.equals(this.enumFormString, testEnumParametersBody.enumFormString);
  }

  @Override
  public int hashCode() {
    return Objects.hash(enumFormStringArray, enumFormString);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class TestEnumParametersBody {\n");
    
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

