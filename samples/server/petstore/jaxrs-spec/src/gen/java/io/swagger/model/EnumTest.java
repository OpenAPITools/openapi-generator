package io.swagger.model;

import io.swagger.model.OuterEnum;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;


import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;


public class EnumTest  implements Serializable {
  

public enum EnumStringEnum {

    UPPER(String.valueOf("UPPER")), LOWER(String.valueOf("lower")), EMPTY(String.valueOf(""));


    private String value;

    EnumStringEnum (String v) {
        value = v;
    }

    public String value() {
        return value;
    }

    @Override
    public String toString() {
        return String.valueOf(value);
    }

    public static EnumStringEnum fromValue(String v) {
        for (EnumStringEnum b : EnumStringEnum.values()) {
            if (String.valueOf(b.value).equals(v)) {
                return b;
            }
        }
        return null;
    }
}

  private @Valid EnumStringEnum enumString = null;

public enum EnumIntegerEnum {

    NUMBER_1(Integer.valueOf(1)), NUMBER_MINUS_1(Integer.valueOf(-1));


    private Integer value;

    EnumIntegerEnum (Integer v) {
        value = v;
    }

    public String value() {
        return value;
    }

    @Override
    public String toString() {
        return String.valueOf(value);
    }

    public static EnumIntegerEnum fromValue(String v) {
        for (EnumIntegerEnum b : EnumIntegerEnum.values()) {
            if (String.valueOf(b.value).equals(v)) {
                return b;
            }
        }
        return null;
    }
}

  private @Valid EnumIntegerEnum enumInteger = null;

public enum EnumNumberEnum {

    NUMBER_1_DOT_1(Double.valueOf(1.1)), NUMBER_MINUS_1_DOT_2(Double.valueOf(-1.2));


    private Double value;

    EnumNumberEnum (Double v) {
        value = v;
    }

    public String value() {
        return value;
    }

    @Override
    public String toString() {
        return String.valueOf(value);
    }

    public static EnumNumberEnum fromValue(String v) {
        for (EnumNumberEnum b : EnumNumberEnum.values()) {
            if (String.valueOf(b.value).equals(v)) {
                return b;
            }
        }
        return null;
    }
}

  private @Valid EnumNumberEnum enumNumber = null;
  private @Valid OuterEnum outerEnum = null;

  /**
   **/
  public EnumTest enumString(EnumStringEnum enumString) {
    this.enumString = enumString;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("enum_string")
  public EnumStringEnum getEnumString() {
    return enumString;
  }
  public void setEnumString(EnumStringEnum enumString) {
    this.enumString = enumString;
  }

  /**
   **/
  public EnumTest enumInteger(EnumIntegerEnum enumInteger) {
    this.enumInteger = enumInteger;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("enum_integer")
  public EnumIntegerEnum getEnumInteger() {
    return enumInteger;
  }
  public void setEnumInteger(EnumIntegerEnum enumInteger) {
    this.enumInteger = enumInteger;
  }

  /**
   **/
  public EnumTest enumNumber(EnumNumberEnum enumNumber) {
    this.enumNumber = enumNumber;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("enum_number")
  public EnumNumberEnum getEnumNumber() {
    return enumNumber;
  }
  public void setEnumNumber(EnumNumberEnum enumNumber) {
    this.enumNumber = enumNumber;
  }

  /**
   **/
  public EnumTest outerEnum(OuterEnum outerEnum) {
    this.outerEnum = outerEnum;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("outerEnum")
  public OuterEnum getOuterEnum() {
    return outerEnum;
  }
  public void setOuterEnum(OuterEnum outerEnum) {
    this.outerEnum = outerEnum;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    EnumTest enumTest = (EnumTest) o;
    return Objects.equals(enumString, enumTest.enumString) &&
        Objects.equals(enumInteger, enumTest.enumInteger) &&
        Objects.equals(enumNumber, enumTest.enumNumber) &&
        Objects.equals(outerEnum, enumTest.outerEnum);
  }

  @Override
  public int hashCode() {
    return Objects.hash(enumString, enumInteger, enumNumber, outerEnum);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class EnumTest {\n");
    
    sb.append("    enumString: ").append(toIndentedString(enumString)).append("\n");
    sb.append("    enumInteger: ").append(toIndentedString(enumInteger)).append("\n");
    sb.append("    enumNumber: ").append(toIndentedString(enumNumber)).append("\n");
    sb.append("    outerEnum: ").append(toIndentedString(outerEnum)).append("\n");
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

