package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import com.fasterxml.jackson.annotation.JsonValue;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.openapitools.model.OuterEnum;
import org.springframework.lang.Nullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * EnumTest
 */

@JsonTypeName("Enum_Test")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
public class EnumTest {

  /**
   * Gets or Sets enumString
   */
  public enum EnumStringEnum {
    UPPER("UPPER"),
    
    LOWER("lower"),
    
    EMPTY("");

    private final String value;

    EnumStringEnum(String value) {
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
    public static EnumStringEnum fromValue(String value) {
      for (EnumStringEnum b : EnumStringEnum.values()) {
        if (b.value.equals(value)) {
          return b;
        }
      }
      throw new IllegalArgumentException("Unexpected value '" + value + "'");
    }
  }

  private @Nullable EnumStringEnum enumString;

  /**
   * Gets or Sets enumStringRequired
   */
  public enum EnumStringRequiredEnum {
    UPPER("UPPER"),
    
    LOWER("lower"),
    
    EMPTY("");

    private final String value;

    EnumStringRequiredEnum(String value) {
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
    public static EnumStringRequiredEnum fromValue(String value) {
      for (EnumStringRequiredEnum b : EnumStringRequiredEnum.values()) {
        if (b.value.equals(value)) {
          return b;
        }
      }
      throw new IllegalArgumentException("Unexpected value '" + value + "'");
    }
  }

  private EnumStringRequiredEnum enumStringRequired;

  /**
   * Gets or Sets enumInteger
   */
  public enum EnumIntegerEnum {
    NUMBER_1(1),
    
    NUMBER_MINUS_1(-1);

    private final Integer value;

    EnumIntegerEnum(Integer value) {
      this.value = value;
    }

    @JsonValue
    public Integer getValue() {
      return value;
    }

    @Override
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

  private @Nullable EnumIntegerEnum enumInteger;

  /**
   * Gets or Sets enumNumber
   */
  public enum EnumNumberEnum {
    NUMBER_1_DOT_1(1.1),
    
    NUMBER_MINUS_1_DOT_2(-1.2);

    private final Double value;

    EnumNumberEnum(Double value) {
      this.value = value;
    }

    @JsonValue
    public Double getValue() {
      return value;
    }

    @Override
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

  private @Nullable EnumNumberEnum enumNumber;

  private @Nullable OuterEnum outerEnum;

  public EnumTest() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public EnumTest(EnumStringRequiredEnum enumStringRequired) {
    this.enumStringRequired = enumStringRequired;
  }

  public EnumTest enumString(@Nullable EnumStringEnum enumString) {
    this.enumString = enumString;
    return this;
  }

  /**
   * Get enumString
   * @return enumString
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("enum_string")
  public @Nullable EnumStringEnum getEnumString() {
    return enumString;
  }

  public void setEnumString(@Nullable EnumStringEnum enumString) {
    this.enumString = enumString;
  }

  public EnumTest enumStringRequired(EnumStringRequiredEnum enumStringRequired) {
    this.enumStringRequired = enumStringRequired;
    return this;
  }

  /**
   * Get enumStringRequired
   * @return enumStringRequired
   */
  @NotNull 
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("enum_string_required")
  public EnumStringRequiredEnum getEnumStringRequired() {
    return enumStringRequired;
  }

  public void setEnumStringRequired(EnumStringRequiredEnum enumStringRequired) {
    this.enumStringRequired = enumStringRequired;
  }

  public EnumTest enumInteger(@Nullable EnumIntegerEnum enumInteger) {
    this.enumInteger = enumInteger;
    return this;
  }

  /**
   * Get enumInteger
   * @return enumInteger
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("enum_integer")
  public @Nullable EnumIntegerEnum getEnumInteger() {
    return enumInteger;
  }

  public void setEnumInteger(@Nullable EnumIntegerEnum enumInteger) {
    this.enumInteger = enumInteger;
  }

  public EnumTest enumNumber(@Nullable EnumNumberEnum enumNumber) {
    this.enumNumber = enumNumber;
    return this;
  }

  /**
   * Get enumNumber
   * @return enumNumber
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("enum_number")
  public @Nullable EnumNumberEnum getEnumNumber() {
    return enumNumber;
  }

  public void setEnumNumber(@Nullable EnumNumberEnum enumNumber) {
    this.enumNumber = enumNumber;
  }

  public EnumTest outerEnum(@Nullable OuterEnum outerEnum) {
    this.outerEnum = outerEnum;
    return this;
  }

  /**
   * Get outerEnum
   * @return outerEnum
   */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("outerEnum")
  public @Nullable OuterEnum getOuterEnum() {
    return outerEnum;
  }

  public void setOuterEnum(@Nullable OuterEnum outerEnum) {
    this.outerEnum = outerEnum;
  }

  @Override
  public boolean equals(Object o) {
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
  private String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

