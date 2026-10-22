package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import org.hibernate.validator.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * EnumArrays
 */
@lombok.Getter
@lombok.Setter
@lombok.ToString
@lombok.EqualsAndHashCode

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public class EnumArrays {

  /**
   * Gets or Sets justSymbol
   */
  public enum JustSymbolEnum {
    GREATER_THAN_OR_EQUAL_TO(">="),
    
    DOLLAR("$");

    private final String value;

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

  
  @Schema(name = "just_symbol", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("just_symbol")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable JustSymbolEnum justSymbol;

  /**
   * Gets or Sets arrayEnum
   */
  public enum ArrayEnumEnum {
    FISH("fish"),
    
    CRAB("crab");

    private final String value;

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

  
  @Schema(name = "array_enum", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("array_enum")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private List<ArrayEnumEnum> arrayEnum = new ArrayList<>();

  public EnumArrays justSymbol(@Nullable JustSymbolEnum justSymbol) {
    this.justSymbol = justSymbol;
    return this;
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



}

