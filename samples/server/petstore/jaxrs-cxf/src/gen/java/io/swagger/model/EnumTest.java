package io.swagger.model;

import io.swagger.model.OuterEnum;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.ApiModelProperty;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
import com.fasterxml.jackson.annotation.JsonProperty;

public class EnumTest  {
  

@XmlType(name="EnumStringEnum")
@XmlEnum(String.class)
public enum EnumStringEnum {

@XmlEnumValue("UPPER") UPPER(String.valueOf("UPPER")), @XmlEnumValue("lower") LOWER(String.valueOf("lower")), @XmlEnumValue("") EMPTY(String.valueOf(""));


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

  @ApiModelProperty(value = "")
  private EnumStringEnum enumString = null;


@XmlType(name="EnumStringRequiredEnum")
@XmlEnum(String.class)
public enum EnumStringRequiredEnum {

@XmlEnumValue("UPPER") UPPER(String.valueOf("UPPER")), @XmlEnumValue("lower") LOWER(String.valueOf("lower")), @XmlEnumValue("") EMPTY(String.valueOf(""));


    private String value;

    EnumStringRequiredEnum (String v) {
        value = v;
    }

    public String value() {
        return value;
    }

    @Override
    public String toString() {
        return String.valueOf(value);
    }

    public static EnumStringRequiredEnum fromValue(String v) {
        for (EnumStringRequiredEnum b : EnumStringRequiredEnum.values()) {
            if (String.valueOf(b.value).equals(v)) {
                return b;
            }
        }
        return null;
    }
}

  @ApiModelProperty(required = true, value = "")
  private EnumStringRequiredEnum enumStringRequired = null;


@XmlType(name="EnumIntegerEnum")
@XmlEnum(Integer.class)
public enum EnumIntegerEnum {

@XmlEnumValue("1") NUMBER_1(Integer.valueOf(1)), @XmlEnumValue("-1") NUMBER_MINUS_1(Integer.valueOf(-1));


    private Integer value;

    EnumIntegerEnum (Integer v) {
        value = v;
    }

    public Integer value() {
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

  @ApiModelProperty(value = "")
  private EnumIntegerEnum enumInteger = null;


@XmlType(name="EnumNumberEnum")
@XmlEnum(Double.class)
public enum EnumNumberEnum {

@XmlEnumValue("1.1") NUMBER_1_DOT_1(Double.valueOf(1.1)), @XmlEnumValue("-1.2") NUMBER_MINUS_1_DOT_2(Double.valueOf(-1.2));


    private Double value;

    EnumNumberEnum (Double v) {
        value = v;
    }

    public Double value() {
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

  @ApiModelProperty(value = "")
  private EnumNumberEnum enumNumber = null;

  @ApiModelProperty(value = "")
  @Valid
  private OuterEnum outerEnum = null;
 /**
   * Get enumString
   * @return enumString
  **/
  @JsonProperty("enum_string")
  public String getEnumString() {
    if (enumString == null) {
      return null;
    }
    return enumString.value();
  }

  public void setEnumString(EnumStringEnum enumString) {
    this.enumString = enumString;
  }

  public EnumTest enumString(EnumStringEnum enumString) {
    this.enumString = enumString;
    return this;
  }

 /**
   * Get enumStringRequired
   * @return enumStringRequired
  **/
  @JsonProperty("enum_string_required")
  @NotNull
  public String getEnumStringRequired() {
    if (enumStringRequired == null) {
      return null;
    }
    return enumStringRequired.value();
  }

  public void setEnumStringRequired(EnumStringRequiredEnum enumStringRequired) {
    this.enumStringRequired = enumStringRequired;
  }

  public EnumTest enumStringRequired(EnumStringRequiredEnum enumStringRequired) {
    this.enumStringRequired = enumStringRequired;
    return this;
  }

 /**
   * Get enumInteger
   * @return enumInteger
  **/
  @JsonProperty("enum_integer")
  public Integer getEnumInteger() {
    if (enumInteger == null) {
      return null;
    }
    return enumInteger.value();
  }

  public void setEnumInteger(EnumIntegerEnum enumInteger) {
    this.enumInteger = enumInteger;
  }

  public EnumTest enumInteger(EnumIntegerEnum enumInteger) {
    this.enumInteger = enumInteger;
    return this;
  }

 /**
   * Get enumNumber
   * @return enumNumber
  **/
  @JsonProperty("enum_number")
  public Double getEnumNumber() {
    if (enumNumber == null) {
      return null;
    }
    return enumNumber.value();
  }

  public void setEnumNumber(EnumNumberEnum enumNumber) {
    this.enumNumber = enumNumber;
  }

  public EnumTest enumNumber(EnumNumberEnum enumNumber) {
    this.enumNumber = enumNumber;
    return this;
  }

 /**
   * Get outerEnum
   * @return outerEnum
  **/
  @JsonProperty("outerEnum")
  public OuterEnum getOuterEnum() {
    return outerEnum;
  }

  public void setOuterEnum(OuterEnum outerEnum) {
    this.outerEnum = outerEnum;
  }

  public EnumTest outerEnum(OuterEnum outerEnum) {
    this.outerEnum = outerEnum;
    return this;
  }


  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class EnumTest {\n");
    
    sb.append("    enumString: ").append(toIndentedString(enumString)).append("\n");
    sb.append("    enumStringRequired: ").append(toIndentedString(enumStringRequired)).append("\n");
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
  private static String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

