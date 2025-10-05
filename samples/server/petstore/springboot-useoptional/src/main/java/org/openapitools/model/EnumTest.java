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
import org.openapitools.jackson.nullable.JsonNullable;
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

  private Optional<EnumStringEnum> enumString = Optional.empty();

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

  private Optional<EnumIntegerEnum> enumInteger = Optional.empty();

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

  private Optional<EnumNumberEnum> enumNumber = Optional.empty();

  private Optional<OuterEnum> outerEnum = Optional.empty();

  public EnumTest() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public EnumTest(EnumStringRequiredEnum enumStringRequired) {
    this.enumStringRequired = enumStringRequired;
  }

  public EnumTest enumString(EnumStringEnum enumString) {
    this.enumString = Optional.ofNullable(enumString);
    return this;
  }

  /**
   * Get enumString
   * @return enumString
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("enum_string")
  public Optional<EnumStringEnum> getEnumString() {
    return enumString;
  }

  public void setEnumString(Optional<EnumStringEnum> enumString) {
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

  public EnumTest enumInteger(EnumIntegerEnum enumInteger) {
    this.enumInteger = Optional.ofNullable(enumInteger);
    return this;
  }

  /**
   * Get enumInteger
   * @return enumInteger
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("enum_integer")
  public Optional<EnumIntegerEnum> getEnumInteger() {
    return enumInteger;
  }

  public void setEnumInteger(Optional<EnumIntegerEnum> enumInteger) {
    this.enumInteger = enumInteger;
  }

  public EnumTest enumNumber(EnumNumberEnum enumNumber) {
    this.enumNumber = Optional.ofNullable(enumNumber);
    return this;
  }

  /**
   * Get enumNumber
   * @return enumNumber
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("enum_number")
  public Optional<EnumNumberEnum> getEnumNumber() {
    return enumNumber;
  }

  public void setEnumNumber(Optional<EnumNumberEnum> enumNumber) {
    this.enumNumber = enumNumber;
  }

  public EnumTest outerEnum(OuterEnum outerEnum) {
    this.outerEnum = Optional.ofNullable(outerEnum);
    return this;
  }

  /**
   * Get outerEnum
   * @return outerEnum
   */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("outerEnum")
  public Optional<OuterEnum> getOuterEnum() {
    return outerEnum;
  }

  public void setOuterEnum(Optional<OuterEnum> outerEnum) {
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
  
  public static class Builder {

    private EnumTest instance;

    public Builder() {
      this(new EnumTest());
    }

    protected Builder(EnumTest instance) {
      this.instance = instance;
    }

    protected Builder copyOf(EnumTest value) { 
      this.instance.setEnumString(value.enumString);
      this.instance.setEnumStringRequired(value.enumStringRequired);
      this.instance.setEnumInteger(value.enumInteger);
      this.instance.setEnumNumber(value.enumNumber);
      this.instance.setOuterEnum(value.outerEnum);
      return this;
    }

    public EnumTest.Builder enumString(EnumStringEnum enumString) {
      this.instance.enumString(enumString);
      return this;
    }
    
    public EnumTest.Builder enumStringRequired(EnumStringRequiredEnum enumStringRequired) {
      this.instance.enumStringRequired(enumStringRequired);
      return this;
    }
    
    public EnumTest.Builder enumInteger(EnumIntegerEnum enumInteger) {
      this.instance.enumInteger(enumInteger);
      return this;
    }
    
    public EnumTest.Builder enumNumber(EnumNumberEnum enumNumber) {
      this.instance.enumNumber(enumNumber);
      return this;
    }
    
    public EnumTest.Builder outerEnum(OuterEnum outerEnum) {
      this.instance.outerEnum(outerEnum);
      return this;
    }
    
    /**
    * returns a built EnumTest instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public EnumTest build() {
      try {
        return this.instance;
      } finally {
        // ensure that this.instance is not reused
        this.instance = null;
      }
    }

    @Override
    public String toString() {
      return getClass() + "=(" + instance + ")";
    }
  }

  /**
  * Create a builder with no initialized field (except for the default values).
  */
  public static EnumTest.Builder builder() {
    return new EnumTest.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public EnumTest.Builder toBuilder() {
    EnumTest.Builder builder = new EnumTest.Builder();
    return builder.copyOf(this);
  }

}

