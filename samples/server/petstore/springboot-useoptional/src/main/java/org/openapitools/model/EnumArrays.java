package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * EnumArrays
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.7.0-SNAPSHOT")
public class EnumArrays {

  /**
   * Gets or Sets justSymbol
   */
  public enum JustSymbolEnum {
    GREATER_THAN_OR_EQUAL_TO(">="),
    
    DOLLAR("$");

    private String value;

    JustSymbolEnum(String value) {
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
    public static JustSymbolEnum fromValue(String value) {
      for (JustSymbolEnum b : JustSymbolEnum.values()) {
        if (b.value.equals(value)) {
          return b;
        }
      }
      throw new IllegalArgumentException("Unexpected value '" + value + "'");
    }
  }

  private Optional<JustSymbolEnum> justSymbol = Optional.empty();

  /**
   * Gets or Sets arrayEnum
   */
  public enum ArrayEnumEnum {
    FISH("fish"),
    
    CRAB("crab");

    private String value;

    ArrayEnumEnum(String value) {
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
    public static ArrayEnumEnum fromValue(String value) {
      for (ArrayEnumEnum b : ArrayEnumEnum.values()) {
        if (b.value.equals(value)) {
          return b;
        }
      }
      throw new IllegalArgumentException("Unexpected value '" + value + "'");
    }
  }

  @Valid
  private List<ArrayEnumEnum> arrayEnum = new ArrayList<>();

  public EnumArrays justSymbol(JustSymbolEnum justSymbol) {
    this.justSymbol = Optional.of(justSymbol);
    return this;
  }

  /**
   * Get justSymbol
   * @return justSymbol
  */
  
  @ApiModelProperty(value = "")
  @JsonProperty("just_symbol")
  public Optional<JustSymbolEnum> getJustSymbol() {
    return justSymbol;
  }

  public void setJustSymbol(Optional<JustSymbolEnum> justSymbol) {
    this.justSymbol = justSymbol;
  }

  public EnumArrays arrayEnum(List<ArrayEnumEnum> arrayEnum) {
    this.arrayEnum = arrayEnum;
    return this;
  }

  public EnumArrays addArrayEnumItem(ArrayEnumEnum arrayEnumItem) {
    if (this.arrayEnum == null) {
      this.arrayEnum = new ArrayList<>();
    }
    this.arrayEnum.add(arrayEnumItem);
    return this;
  }

  /**
   * Get arrayEnum
   * @return arrayEnum
  */
  
  @ApiModelProperty(value = "")
  @JsonProperty("array_enum")
  public List<ArrayEnumEnum> getArrayEnum() {
    return arrayEnum;
  }

  public void setArrayEnum(List<ArrayEnumEnum> arrayEnum) {
    this.arrayEnum = arrayEnum;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    EnumArrays enumArrays = (EnumArrays) o;
    return Objects.equals(this.justSymbol, enumArrays.justSymbol) &&
        Objects.equals(this.arrayEnum, enumArrays.arrayEnum);
  }

  @Override
  public int hashCode() {
    return Objects.hash(justSymbol, arrayEnum);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class EnumArrays {\n");
    sb.append("    justSymbol: ").append(toIndentedString(justSymbol)).append("\n");
    sb.append("    arrayEnum: ").append(toIndentedString(arrayEnum)).append("\n");
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

    private EnumArrays instance;

    public Builder() {
      this(new EnumArrays());
    }

    protected Builder(EnumArrays instance) {
      this.instance = instance;
    }

    protected Builder copyOf(EnumArrays value) { 
      this.instance.setJustSymbol(value.justSymbol);
      this.instance.setArrayEnum(value.arrayEnum);
      return this;
    }

    public EnumArrays.Builder justSymbol(JustSymbolEnum justSymbol) {
      this.instance.justSymbol(justSymbol);
      return this;
    }
    
    public EnumArrays.Builder arrayEnum(List<ArrayEnumEnum> arrayEnum) {
      this.instance.arrayEnum(arrayEnum);
      return this;
    }
    
    /**
    * returns a built EnumArrays instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public EnumArrays build() {
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
  public static EnumArrays.Builder builder() {
    return new EnumArrays.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public EnumArrays.Builder toBuilder() {
    EnumArrays.Builder builder = new EnumArrays.Builder();
    return builder.copyOf(this);
  }

}

