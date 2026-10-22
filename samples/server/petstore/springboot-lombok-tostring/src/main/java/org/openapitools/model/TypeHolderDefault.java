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
 * TypeHolderDefault
 */
@lombok.Getter
@lombok.Setter
@lombok.ToString
@lombok.EqualsAndHashCode

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public class TypeHolderDefault {

  @NotNull 
  @Schema(name = "string_item", requiredMode = Schema.RequiredMode.REQUIRED)
  @JsonProperty("string_item")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private String stringItem = "what";

  @NotNull @Valid 
  @Schema(name = "number_item", requiredMode = Schema.RequiredMode.REQUIRED)
  @JsonProperty("number_item")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private BigDecimal numberItem = new BigDecimal("1.234");

  @NotNull 
  @Schema(name = "integer_item", requiredMode = Schema.RequiredMode.REQUIRED)
  @JsonProperty("integer_item")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private Integer integerItem = -2;

  @NotNull 
  @Schema(name = "bool_item", requiredMode = Schema.RequiredMode.REQUIRED)
  @JsonProperty("bool_item")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private Boolean boolItem = true;

  @NotNull 
  @Schema(name = "array_item", requiredMode = Schema.RequiredMode.REQUIRED)
  @JsonProperty("array_item")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private List<Integer> arrayItem = new ArrayList<>(Arrays.asList(0, 1, 2, 3));

  public TypeHolderDefault() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public TypeHolderDefault(String stringItem, BigDecimal numberItem, Integer integerItem, Boolean boolItem, List<Integer> arrayItem) {
    this.stringItem = stringItem;
    this.numberItem = numberItem;
    this.integerItem = integerItem;
    this.boolItem = boolItem;
    this.arrayItem = arrayItem;
  }

  public TypeHolderDefault stringItem(String stringItem) {
    this.stringItem = stringItem;
    return this;
  }


  public TypeHolderDefault numberItem(BigDecimal numberItem) {
    this.numberItem = numberItem;
    return this;
  }


  public TypeHolderDefault integerItem(Integer integerItem) {
    this.integerItem = integerItem;
    return this;
  }


  public TypeHolderDefault boolItem(Boolean boolItem) {
    this.boolItem = boolItem;
    return this;
  }


  public TypeHolderDefault arrayItem(List<Integer> arrayItem) {
    this.arrayItem = arrayItem;
    return this;
  }

  public TypeHolderDefault addArrayItemItem(Integer arrayItemItem) {
    if (this.arrayItem == null) {
      this.arrayItem = new ArrayList<>();
    }
    this.arrayItem.add(arrayItemItem);
    return this;
  }



}

