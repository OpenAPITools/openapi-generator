package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import com.fasterxml.jackson.annotation.JsonValue;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.Arrays;
import org.openapitools.jackson.nullable.JsonNullable;
import org.openapitools.model.OuterEnumDefaultValueDto;
import org.openapitools.model.OuterEnumDto;
import org.openapitools.model.OuterEnumIntegerDefaultValueDto;
import org.openapitools.model.OuterEnumIntegerDto;
import org.springframework.lang.Nullable;
import java.util.NoSuchElementException;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * EnumTestDto
 */

@JsonTypeName("Enum_Test")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.20.0-SNAPSHOT")
public class EnumTestDto {

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

  private JsonNullable<OuterEnumDto> outerEnum = JsonNullable.<OuterEnumDto>undefined();

  private @Nullable OuterEnumIntegerDto outerEnumInteger;

  private OuterEnumDefaultValueDto outerEnumDefaultValue = OuterEnumDefaultValueDto.PLACED;

  private OuterEnumIntegerDefaultValueDto outerEnumIntegerDefaultValue = OuterEnumIntegerDefaultValueDto.NUMBER_0;

  public EnumTestDto() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public EnumTestDto(EnumStringRequiredEnum enumStringRequired) {
    this.enumStringRequired = enumStringRequired;
  }

  public EnumTestDto enumString(@Nullable EnumStringEnum enumString) {
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

  public EnumTestDto enumStringRequired(EnumStringRequiredEnum enumStringRequired) {
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

  public EnumTestDto enumInteger(@Nullable EnumIntegerEnum enumInteger) {
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

  public EnumTestDto enumNumber(@Nullable EnumNumberEnum enumNumber) {
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

  public EnumTestDto outerEnum(OuterEnumDto outerEnum) {
    this.outerEnum = JsonNullable.of(outerEnum);
    return this;
  }

  /**
   * Get outerEnum
   * @return outerEnum
   */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("outerEnum")
  public JsonNullable<OuterEnumDto> getOuterEnum() {
    return outerEnum;
  }

  public void setOuterEnum(JsonNullable<OuterEnumDto> outerEnum) {
    this.outerEnum = outerEnum;
  }

  public EnumTestDto outerEnumInteger(@Nullable OuterEnumIntegerDto outerEnumInteger) {
    this.outerEnumInteger = outerEnumInteger;
    return this;
  }

  /**
   * Get outerEnumInteger
   * @return outerEnumInteger
   */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("outerEnumInteger")
  public @Nullable OuterEnumIntegerDto getOuterEnumInteger() {
    return outerEnumInteger;
  }

  public void setOuterEnumInteger(@Nullable OuterEnumIntegerDto outerEnumInteger) {
    this.outerEnumInteger = outerEnumInteger;
  }

  public EnumTestDto outerEnumDefaultValue(OuterEnumDefaultValueDto outerEnumDefaultValue) {
    this.outerEnumDefaultValue = outerEnumDefaultValue;
    return this;
  }

  /**
   * Get outerEnumDefaultValue
   * @return outerEnumDefaultValue
   */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("outerEnumDefaultValue")
  public OuterEnumDefaultValueDto getOuterEnumDefaultValue() {
    return outerEnumDefaultValue;
  }

  public void setOuterEnumDefaultValue(OuterEnumDefaultValueDto outerEnumDefaultValue) {
    this.outerEnumDefaultValue = outerEnumDefaultValue;
  }

  public EnumTestDto outerEnumIntegerDefaultValue(OuterEnumIntegerDefaultValueDto outerEnumIntegerDefaultValue) {
    this.outerEnumIntegerDefaultValue = outerEnumIntegerDefaultValue;
    return this;
  }

  /**
   * Get outerEnumIntegerDefaultValue
   * @return outerEnumIntegerDefaultValue
   */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("outerEnumIntegerDefaultValue")
  public OuterEnumIntegerDefaultValueDto getOuterEnumIntegerDefaultValue() {
    return outerEnumIntegerDefaultValue;
  }

  public void setOuterEnumIntegerDefaultValue(OuterEnumIntegerDefaultValueDto outerEnumIntegerDefaultValue) {
    this.outerEnumIntegerDefaultValue = outerEnumIntegerDefaultValue;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    EnumTestDto enumTest = (EnumTestDto) o;
    return Objects.equals(this.enumString, enumTest.enumString) &&
        Objects.equals(this.enumStringRequired, enumTest.enumStringRequired) &&
        Objects.equals(this.enumInteger, enumTest.enumInteger) &&
        Objects.equals(this.enumNumber, enumTest.enumNumber) &&
        equalsNullable(this.outerEnum, enumTest.outerEnum) &&
        Objects.equals(this.outerEnumInteger, enumTest.outerEnumInteger) &&
        Objects.equals(this.outerEnumDefaultValue, enumTest.outerEnumDefaultValue) &&
        Objects.equals(this.outerEnumIntegerDefaultValue, enumTest.outerEnumIntegerDefaultValue);
  }

  private static <T> boolean equalsNullable(JsonNullable<T> a, JsonNullable<T> b) {
    return a == b || (a != null && b != null && a.isPresent() && b.isPresent() && Objects.deepEquals(a.get(), b.get()));
  }

  @Override
  public int hashCode() {
    return Objects.hash(enumString, enumStringRequired, enumInteger, enumNumber, hashCodeNullable(outerEnum), outerEnumInteger, outerEnumDefaultValue, outerEnumIntegerDefaultValue);
  }

  private static <T> int hashCodeNullable(JsonNullable<T> a) {
    if (a == null) {
      return 1;
    }
    return a.isPresent() ? Arrays.deepHashCode(new Object[]{a.get()}) : 31;
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class EnumTestDto {\n");
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
  private String toIndentedString(@Nullable Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

