package apimodels;

import apimodels.OuterEnum;
import com.fasterxml.jackson.annotation.JsonTypeName;
import com.fasterxml.jackson.annotation.*;
import java.util.Set;
import javax.validation.*;
import java.util.Objects;
import javax.validation.constraints.*;
/**
 * EnumTest
 */
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaPlayFrameworkCodegen")
@SuppressWarnings({"UnusedReturnValue", "WeakerAccess"})
public class EnumTest   {
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

  @JsonProperty("enum_string")
  
  private EnumStringEnum enumString;

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

  @JsonProperty("enum_string_required")
  @NotNull

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

  @JsonProperty("enum_integer")
  
  private EnumIntegerEnum enumInteger;

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

  @JsonProperty("enum_number")
  
  private EnumNumberEnum enumNumber;

  @JsonProperty("outerEnum")
  @Valid

  private OuterEnum outerEnum;

  public EnumTest enumString(EnumStringEnum enumString) {
    this.enumString = enumString;
    return this;
  }

   /**
   * Get enumString
   * @return enumString
  **/
  public EnumStringEnum getEnumString() {
    return enumString;
  }

  public void setEnumString(EnumStringEnum enumString) {
    this.enumString = enumString;
  }

  public EnumTest enumStringRequired(EnumStringRequiredEnum enumStringRequired) {
    this.enumStringRequired = enumStringRequired;
    return this;
  }

   /**
   * Get enumStringRequired
   * @return enumStringRequired
  **/
  public EnumStringRequiredEnum getEnumStringRequired() {
    return enumStringRequired;
  }

  public void setEnumStringRequired(EnumStringRequiredEnum enumStringRequired) {
    this.enumStringRequired = enumStringRequired;
  }

  public EnumTest enumInteger(EnumIntegerEnum enumInteger) {
    this.enumInteger = enumInteger;
    return this;
  }

   /**
   * Get enumInteger
   * @return enumInteger
  **/
  public EnumIntegerEnum getEnumInteger() {
    return enumInteger;
  }

  public void setEnumInteger(EnumIntegerEnum enumInteger) {
    this.enumInteger = enumInteger;
  }

  public EnumTest enumNumber(EnumNumberEnum enumNumber) {
    this.enumNumber = enumNumber;
    return this;
  }

   /**
   * Get enumNumber
   * @return enumNumber
  **/
  public EnumNumberEnum getEnumNumber() {
    return enumNumber;
  }

  public void setEnumNumber(EnumNumberEnum enumNumber) {
    this.enumNumber = enumNumber;
  }

  public EnumTest outerEnum(OuterEnum outerEnum) {
    this.outerEnum = outerEnum;
    return this;
  }

   /**
   * Get outerEnum
   * @return outerEnum
  **/
  public OuterEnum getOuterEnum() {
    return outerEnum;
  }

  public void setOuterEnum(OuterEnum outerEnum) {
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
    return Objects.equals(enumString, enumTest.enumString) &&
        Objects.equals(enumStringRequired, enumTest.enumStringRequired) &&
        Objects.equals(enumInteger, enumTest.enumInteger) &&
        Objects.equals(enumNumber, enumTest.enumNumber) &&
        Objects.equals(outerEnum, enumTest.outerEnum);
  }

  @Override
  public int hashCode() {
    return Objects.hash(enumString, enumStringRequired, enumInteger, enumNumber, outerEnum);
  }

  @SuppressWarnings("StringBufferReplaceableByString")
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

