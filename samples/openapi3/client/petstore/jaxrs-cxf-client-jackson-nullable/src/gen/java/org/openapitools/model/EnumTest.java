package org.openapitools.model;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonValue;
import org.openapitools.jackson.nullable.JsonNullable;
import org.openapitools.model.OuterEnum;
import org.openapitools.model.OuterEnumDefaultValue;
import org.openapitools.model.OuterEnumInteger;
import org.openapitools.model.OuterEnumIntegerDefaultValue;

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
    @JsonValue
    public String toString() {
        return String.valueOf(value);
    }

    @JsonCreator
    public static EnumStringEnum fromValue(String value) {
        for (EnumStringEnum b : EnumStringEnum.values()) {
            if (b.value.equals(value)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected value '" + value + "'");
    }
}

  @ApiModelProperty(value = "")
  private EnumStringEnum enumString;

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
    @JsonValue
    public String toString() {
        return String.valueOf(value);
    }

    @JsonCreator
    public static EnumStringRequiredEnum fromValue(String value) {
        for (EnumStringRequiredEnum b : EnumStringRequiredEnum.values()) {
            if (b.value.equals(value)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected value '" + value + "'");
    }
}

  @ApiModelProperty(required = true, value = "")
  private EnumStringRequiredEnum enumStringRequired;

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
    @JsonValue
    public String toString() {
        return String.valueOf(value);
    }

    @JsonCreator
    public static EnumIntegerEnum fromValue(Integer value) {
        for (EnumIntegerEnum b : EnumIntegerEnum.values()) {
            if (b.value.equals(value)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected value '" + value + "'");
    }
}

  @ApiModelProperty(value = "")
  private EnumIntegerEnum enumInteger;

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
    @JsonValue
    public String toString() {
        return String.valueOf(value);
    }

    @JsonCreator
    public static EnumNumberEnum fromValue(Double value) {
        for (EnumNumberEnum b : EnumNumberEnum.values()) {
            if (b.value.equals(value)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected value '" + value + "'");
    }
}

  @ApiModelProperty(value = "")
  private EnumNumberEnum enumNumber;

  @ApiModelProperty(value = "")
  private JsonNullable<OuterEnum> outerEnum = JsonNullable.<OuterEnum>undefined();

  @ApiModelProperty(value = "")
  private OuterEnumInteger outerEnumInteger;

  @ApiModelProperty(value = "")
  private OuterEnumDefaultValue outerEnumDefaultValue = OuterEnumDefaultValue.PLACED;

  @ApiModelProperty(value = "")
  private OuterEnumIntegerDefaultValue outerEnumIntegerDefaultValue = OuterEnumIntegerDefaultValue.NUMBER_0;
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
  @JsonIgnore
  public OuterEnum getOuterEnum() {
    if (outerEnum == null) {
      return null;
    }
    return outerEnum.orElse(null);
  }

  @JsonProperty("outerEnum")
  public JsonNullable<OuterEnum> getOuterEnum_JsonNullable() {
    return outerEnum;
  }

  public void setOuterEnum(OuterEnum outerEnum) {
      this.outerEnum = JsonNullable.<OuterEnum>of(outerEnum);
  }

  @JsonProperty("outerEnum")
  public void setOuterEnum_JsonNullable(JsonNullable<OuterEnum> outerEnum) {
    this.outerEnum = outerEnum;
  }

  public EnumTest outerEnum(OuterEnum outerEnum) {
    this.outerEnum = JsonNullable.<OuterEnum>of(outerEnum);
    return this;
  }

 /**
   * Get outerEnumInteger
   * @return outerEnumInteger
  **/
  @JsonProperty("outerEnumInteger")
  public OuterEnumInteger getOuterEnumInteger() {
    return outerEnumInteger;
  }

  public void setOuterEnumInteger(OuterEnumInteger outerEnumInteger) {
    this.outerEnumInteger = outerEnumInteger;
  }

  public EnumTest outerEnumInteger(OuterEnumInteger outerEnumInteger) {
    this.outerEnumInteger = outerEnumInteger;
    return this;
  }

 /**
   * Get outerEnumDefaultValue
   * @return outerEnumDefaultValue
  **/
  @JsonProperty("outerEnumDefaultValue")
  public OuterEnumDefaultValue getOuterEnumDefaultValue() {
    return outerEnumDefaultValue;
  }

  public void setOuterEnumDefaultValue(OuterEnumDefaultValue outerEnumDefaultValue) {
    this.outerEnumDefaultValue = outerEnumDefaultValue;
  }

  public EnumTest outerEnumDefaultValue(OuterEnumDefaultValue outerEnumDefaultValue) {
    this.outerEnumDefaultValue = outerEnumDefaultValue;
    return this;
  }

 /**
   * Get outerEnumIntegerDefaultValue
   * @return outerEnumIntegerDefaultValue
  **/
  @JsonProperty("outerEnumIntegerDefaultValue")
  public OuterEnumIntegerDefaultValue getOuterEnumIntegerDefaultValue() {
    return outerEnumIntegerDefaultValue;
  }

  public void setOuterEnumIntegerDefaultValue(OuterEnumIntegerDefaultValue outerEnumIntegerDefaultValue) {
    this.outerEnumIntegerDefaultValue = outerEnumIntegerDefaultValue;
  }

  public EnumTest outerEnumIntegerDefaultValue(OuterEnumIntegerDefaultValue outerEnumIntegerDefaultValue) {
    this.outerEnumIntegerDefaultValue = outerEnumIntegerDefaultValue;
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
    sb.append("    outerEnumInteger: ").append(toIndentedString(outerEnumInteger)).append("\n");
    sb.append("    outerEnumDefaultValue: ").append(toIndentedString(outerEnumDefaultValue)).append("\n");
    sb.append("    outerEnumIntegerDefaultValue: ").append(toIndentedString(outerEnumIntegerDefaultValue)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private static String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

