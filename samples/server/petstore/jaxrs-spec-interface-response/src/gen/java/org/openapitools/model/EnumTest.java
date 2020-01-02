package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.openapitools.model.OuterEnum;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;



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

  private @Valid EnumStringEnum enumString;

public enum EnumStringRequiredEnum {

    UPPER(String.valueOf("UPPER")), LOWER(String.valueOf("lower")), EMPTY(String.valueOf(""));


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

  private @Valid EnumStringRequiredEnum enumStringRequired;

public enum EnumIntegerEnum {

    NUMBER_1(Integer.valueOf(1)), NUMBER_MINUS_1(Integer.valueOf(-1));


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

  private @Valid EnumIntegerEnum enumInteger;

public enum EnumNumberEnum {

    NUMBER_1_DOT_1(Double.valueOf(1.1)), NUMBER_MINUS_1_DOT_2(Double.valueOf(-1.2));


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

  private @Valid EnumNumberEnum enumNumber;
  private @Valid OuterEnum outerEnum;

  public EnumTest(EnumStringEnum enumString, EnumStringRequiredEnum enumStringRequired, EnumIntegerEnum enumInteger, EnumNumberEnum enumNumber, OuterEnum outerEnum) {
    this.enumString = enumString;
    this.enumStringRequired = enumStringRequired;
    this.enumInteger = enumInteger;
    this.enumNumber = enumNumber;
    this.outerEnum = outerEnum;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("enum_string")
  public EnumStringEnum getEnumString() {
    return enumString;
  }

  public void setEnumString(EnumStringEnum enumString) {
    this.enumString = enumString;
  }
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("enum_string_required")
  @NotNull
  public EnumStringRequiredEnum getEnumStringRequired() {
    return enumStringRequired;
  }

  public void setEnumStringRequired(EnumStringRequiredEnum enumStringRequired) {
    this.enumStringRequired = enumStringRequired;
  }
  @ApiModelProperty(value = "")
  @JsonProperty("enum_integer")
  public EnumIntegerEnum getEnumInteger() {
    return enumInteger;
  }

  public void setEnumInteger(EnumIntegerEnum enumInteger) {
    this.enumInteger = enumInteger;
  }
  @ApiModelProperty(value = "")
  @JsonProperty("enum_number")
  public EnumNumberEnum getEnumNumber() {
    return enumNumber;
  }

  public void setEnumNumber(EnumNumberEnum enumNumber) {
    this.enumNumber = enumNumber;
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
    return Objects.equals(this.enumString, enumTest.enumString) &&
        Objects.equals(this.enumStringRequired, enumTest.enumStringRequired) &&
        Objects.equals(this.enumInteger, enumTest.enumInteger) &&
        Objects.equals(this.enumNumber, enumTest.enumNumber) &&
        Objects.equals(this.outerEnum, enumTest.outerEnum);
  }

  @Override
  public int hashCode() {
    return Objects.hash(enumString, enumStringRequired, enumInteger, enumNumber, outerEnum);
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
  private String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }

  public static Builder builder() {
    return new Builder();
  }

  public static class Builder {
    private EnumStringEnum enumString;
    private EnumStringRequiredEnum enumStringRequired;
    private EnumIntegerEnum enumInteger;
    private EnumNumberEnum enumNumber;
    private OuterEnum outerEnum;

    /**
      **/
    public Builder enumString(EnumStringEnum enumString) {
      this.enumString = enumString;
      return this;
    }
    /**
      **/
    public Builder enumStringRequired(EnumStringRequiredEnum enumStringRequired) {
      this.enumStringRequired = enumStringRequired;
      return this;
    }
    /**
      **/
    public Builder enumInteger(EnumIntegerEnum enumInteger) {
      this.enumInteger = enumInteger;
      return this;
    }
    /**
      **/
    public Builder enumNumber(EnumNumberEnum enumNumber) {
      this.enumNumber = enumNumber;
      return this;
    }
    /**
      **/
    public Builder outerEnum(OuterEnum outerEnum) {
      this.outerEnum = outerEnum;
      return this;
    }

    public EnumTest build() {
      return new EnumTest(enumString, enumStringRequired, enumInteger, enumNumber, outerEnum);
    }
  }
}

