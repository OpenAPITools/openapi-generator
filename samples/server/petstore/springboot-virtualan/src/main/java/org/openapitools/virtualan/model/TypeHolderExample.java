package org.openapitools.virtualan.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import org.openapitools.jackson.nullable.JsonNullable;
import javax.validation.Valid;
import javax.validation.constraints.*;

/**
 * TypeHolderExample
 */

public class TypeHolderExample   {
  @JsonProperty("string_item")
  private String stringItem;

  @JsonProperty("number_item")
  private BigDecimal numberItem;

  @JsonProperty("integer_item")
  private Integer integerItem;

  @JsonProperty("bool_item")
  private Boolean boolItem;

  @JsonProperty("array_item")
  @Valid
  private List<Integer> arrayItem = new ArrayList<>();

  public TypeHolderExample stringItem(String stringItem) {
    this.stringItem = stringItem;
    return this;
  }

  /**
   * Get stringItem
   * @return stringItem
  */
  @ApiModelProperty(example = "what", required = true, value = "")
  @NotNull


  public String getStringItem() {
    return stringItem;
  }

  public void setStringItem(String stringItem) {
    this.stringItem = stringItem;
  }

  public TypeHolderExample numberItem(BigDecimal numberItem) {
    this.numberItem = numberItem;
    return this;
  }

  /**
   * Get numberItem
   * @return numberItem
  */
  @ApiModelProperty(example = "1.234", required = true, value = "")
  @NotNull

  @Valid

  public BigDecimal getNumberItem() {
    return numberItem;
  }

  public void setNumberItem(BigDecimal numberItem) {
    this.numberItem = numberItem;
  }

  public TypeHolderExample integerItem(Integer integerItem) {
    this.integerItem = integerItem;
    return this;
  }

  /**
   * Get integerItem
   * @return integerItem
  */
  @ApiModelProperty(example = "-2", required = true, value = "")
  @NotNull


  public Integer getIntegerItem() {
    return integerItem;
  }

  public void setIntegerItem(Integer integerItem) {
    this.integerItem = integerItem;
  }

  public TypeHolderExample boolItem(Boolean boolItem) {
    this.boolItem = boolItem;
    return this;
  }

  /**
   * Get boolItem
   * @return boolItem
  */
  @ApiModelProperty(example = "true", required = true, value = "")
  @NotNull


  public Boolean getBoolItem() {
    return boolItem;
  }

  public void setBoolItem(Boolean boolItem) {
    this.boolItem = boolItem;
  }

  public TypeHolderExample arrayItem(List<Integer> arrayItem) {
    this.arrayItem = arrayItem;
    return this;
  }

  public TypeHolderExample addArrayItemItem(Integer arrayItemItem) {
    this.arrayItem.add(arrayItemItem);
    return this;
  }

  /**
   * Get arrayItem
   * @return arrayItem
  */
  @ApiModelProperty(example = "[0, 1, 2, 3]", required = true, value = "")
  @NotNull


  public List<Integer> getArrayItem() {
    return arrayItem;
  }

  public void setArrayItem(List<Integer> arrayItem) {
    this.arrayItem = arrayItem;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    TypeHolderExample typeHolderExample = (TypeHolderExample) o;
    return Objects.equals(this.stringItem, typeHolderExample.stringItem) &&
        Objects.equals(this.numberItem, typeHolderExample.numberItem) &&
        Objects.equals(this.integerItem, typeHolderExample.integerItem) &&
        Objects.equals(this.boolItem, typeHolderExample.boolItem) &&
        Objects.equals(this.arrayItem, typeHolderExample.arrayItem);
  }

  @Override
  public int hashCode() {
    return Objects.hash(stringItem, numberItem, integerItem, boolItem, arrayItem);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class TypeHolderExample {\n");
    
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
  private String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

