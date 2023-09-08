package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.math.BigDecimal;
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
 * TypeHolderDefaultDto
 */

@JsonTypeName("TypeHolderDefault")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class TypeHolderDefaultDto {

  private String stringItem = "what";

  private BigDecimal numberItem = new BigDecimal("1.234");

  private Integer integerItem = -2;

  private Boolean boolItem = true;

  @Valid
  private List<Integer> arrayItem = new ArrayList<>(Arrays.asList(0, 1, 2, 3));

  public TypeHolderDefaultDto() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public TypeHolderDefaultDto(String stringItem, BigDecimal numberItem, Integer integerItem, Boolean boolItem, List<Integer> arrayItem) {
    this.stringItem = stringItem;
    this.numberItem = numberItem;
    this.integerItem = integerItem;
    this.boolItem = boolItem;
    this.arrayItem = arrayItem;
  }

  public TypeHolderDefaultDto stringItem(String stringItem) {
    this.stringItem = stringItem;
    return this;
  }

  /**
   * Get stringItem
   * @return stringItem
  */
  @NotNull 
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("string_item")
  public String getStringItem() {
    return stringItem;
  }

  public void setStringItem(String stringItem) {
    this.stringItem = stringItem;
  }

  public TypeHolderDefaultDto numberItem(BigDecimal numberItem) {
    this.numberItem = numberItem;
    return this;
  }

  /**
   * Get numberItem
   * @return numberItem
  */
  @NotNull @Valid 
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("number_item")
  public BigDecimal getNumberItem() {
    return numberItem;
  }

  public void setNumberItem(BigDecimal numberItem) {
    this.numberItem = numberItem;
  }

  public TypeHolderDefaultDto integerItem(Integer integerItem) {
    this.integerItem = integerItem;
    return this;
  }

  /**
   * Get integerItem
   * @return integerItem
  */
  @NotNull 
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("integer_item")
  public Integer getIntegerItem() {
    return integerItem;
  }

  public void setIntegerItem(Integer integerItem) {
    this.integerItem = integerItem;
  }

  public TypeHolderDefaultDto boolItem(Boolean boolItem) {
    this.boolItem = boolItem;
    return this;
  }

  /**
   * Get boolItem
   * @return boolItem
  */
  @NotNull 
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("bool_item")
  public Boolean getBoolItem() {
    return boolItem;
  }

  public void setBoolItem(Boolean boolItem) {
    this.boolItem = boolItem;
  }

  public TypeHolderDefaultDto arrayItem(List<Integer> arrayItem) {
    this.arrayItem = arrayItem;
    return this;
  }

  public TypeHolderDefaultDto addArrayItemItem(Integer arrayItemItem) {
    if (this.arrayItem == null) {
      this.arrayItem = new ArrayList<>(Arrays.asList(0, 1, 2, 3));
    }
    this.arrayItem.add(arrayItemItem);
    return this;
  }

  /**
   * Get arrayItem
   * @return arrayItem
  */
  @NotNull 
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("array_item")
  public List<Integer> getArrayItem() {
    return arrayItem;
  }

  public void setArrayItem(List<Integer> arrayItem) {
    this.arrayItem = arrayItem;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    TypeHolderDefaultDto typeHolderDefault = (TypeHolderDefaultDto) o;
    return Objects.equals(this.stringItem, typeHolderDefault.stringItem) &&
        Objects.equals(this.numberItem, typeHolderDefault.numberItem) &&
        Objects.equals(this.integerItem, typeHolderDefault.integerItem) &&
        Objects.equals(this.boolItem, typeHolderDefault.boolItem) &&
        Objects.equals(this.arrayItem, typeHolderDefault.arrayItem);
  }

  @Override
  public int hashCode() {
    return Objects.hash(stringItem, numberItem, integerItem, boolItem, arrayItem);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class TypeHolderDefaultDto {\n");
    sb.append("    stringItem: ").append(toIndentedString(stringItem)).append("\n");
    sb.append("    numberItem: ").append(toIndentedString(numberItem)).append("\n");
    sb.append("    integerItem: ").append(toIndentedString(integerItem)).append("\n");
    sb.append("    boolItem: ").append(toIndentedString(boolItem)).append("\n");
    sb.append("    arrayItem: ").append(toIndentedString(arrayItem)).append("\n");
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

