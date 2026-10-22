package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import java.math.BigDecimal;
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
 * TypeHolderExample
 */
@lombok.Getter
@lombok.Setter
@lombok.ToString
@lombok.EqualsAndHashCode

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public class TypeHolderExample {

  @NotNull 
  @Schema(name = "string_item", example = "what", requiredMode = Schema.RequiredMode.REQUIRED)
  @JsonProperty("string_item")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private String stringItem;

  @NotNull @Valid 
  @Schema(name = "number_item", example = "1.234", requiredMode = Schema.RequiredMode.REQUIRED)
  @JsonProperty("number_item")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private BigDecimal numberItem;

  @NotNull 
  @Schema(name = "float_item", example = "1.234", requiredMode = Schema.RequiredMode.REQUIRED)
  @JsonProperty("float_item")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private Float floatItem;

  @NotNull 
  @Schema(name = "integer_item", example = "-2", requiredMode = Schema.RequiredMode.REQUIRED)
  @JsonProperty("integer_item")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private Integer integerItem;

  @NotNull 
  @Schema(name = "bool_item", example = "true", requiredMode = Schema.RequiredMode.REQUIRED)
  @JsonProperty("bool_item")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private Boolean boolItem;

  @NotNull 
  @Schema(name = "array_item", example = "[0,1,2,3]", requiredMode = Schema.RequiredMode.REQUIRED)
  @JsonProperty("array_item")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private List<Integer> arrayItem = new ArrayList<>();

  public TypeHolderExample() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public TypeHolderExample(String stringItem, BigDecimal numberItem, Float floatItem, Integer integerItem, Boolean boolItem, List<Integer> arrayItem) {
    this.stringItem = stringItem;
    this.numberItem = numberItem;
    this.floatItem = floatItem;
    this.integerItem = integerItem;
    this.boolItem = boolItem;
    this.arrayItem = arrayItem;
  }

  public TypeHolderExample stringItem(String stringItem) {
    this.stringItem = stringItem;
    return this;
  }


  public TypeHolderExample numberItem(BigDecimal numberItem) {
    this.numberItem = numberItem;
    return this;
  }


  public TypeHolderExample floatItem(Float floatItem) {
    this.floatItem = floatItem;
    return this;
  }


  public TypeHolderExample integerItem(Integer integerItem) {
    this.integerItem = integerItem;
    return this;
  }


  public TypeHolderExample boolItem(Boolean boolItem) {
    this.boolItem = boolItem;
    return this;
  }


  public TypeHolderExample arrayItem(List<Integer> arrayItem) {
    this.arrayItem = arrayItem;
    return this;
  }

  public TypeHolderExample addArrayItemItem(Integer arrayItemItem) {
    if (this.arrayItem == null) {
      this.arrayItem = new ArrayList<>();
    }
    this.arrayItem.add(arrayItemItem);
    return this;
  }



}

