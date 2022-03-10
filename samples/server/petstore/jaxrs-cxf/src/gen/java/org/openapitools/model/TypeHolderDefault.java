package org.openapitools.model;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.ApiModelProperty;
import com.fasterxml.jackson.annotation.JsonProperty;

public class TypeHolderDefault  {
  
  @ApiModelProperty(required = true, value = "")
  private String stringItem = "what";

  @ApiModelProperty(required = true, value = "")
  @Valid
  private BigDecimal numberItem;

  @ApiModelProperty(required = true, value = "")
  private Integer integerItem;

  @ApiModelProperty(required = true, value = "")
  private Boolean boolItem = true;

  @ApiModelProperty(required = true, value = "")
  private List<Integer> arrayItem = new ArrayList<>();
 /**
   * Get stringItem
   * @return stringItem
  **/
  @JsonProperty("string_item")
  @NotNull
  public String getStringItem() {
    return stringItem;
  }

  public void setStringItem(String stringItem) {
    this.stringItem = stringItem;
  }

  public TypeHolderDefault stringItem(String stringItem) {
    this.stringItem = stringItem;
    return this;
  }

 /**
   * Get numberItem
   * @return numberItem
  **/
  @JsonProperty("number_item")
  @NotNull
  public BigDecimal getNumberItem() {
    return numberItem;
  }

  public void setNumberItem(BigDecimal numberItem) {
    this.numberItem = numberItem;
  }

  public TypeHolderDefault numberItem(BigDecimal numberItem) {
    this.numberItem = numberItem;
    return this;
  }

 /**
   * Get integerItem
   * @return integerItem
  **/
  @JsonProperty("integer_item")
  @NotNull
  public Integer getIntegerItem() {
    return integerItem;
  }

  public void setIntegerItem(Integer integerItem) {
    this.integerItem = integerItem;
  }

  public TypeHolderDefault integerItem(Integer integerItem) {
    this.integerItem = integerItem;
    return this;
  }

 /**
   * Get boolItem
   * @return boolItem
  **/
  @JsonProperty("bool_item")
  @NotNull
  public Boolean getBoolItem() {
    return boolItem;
  }

  public void setBoolItem(Boolean boolItem) {
    this.boolItem = boolItem;
  }

  public TypeHolderDefault boolItem(Boolean boolItem) {
    this.boolItem = boolItem;
    return this;
  }

 /**
   * Get arrayItem
   * @return arrayItem
  **/
  @JsonProperty("array_item")
  @NotNull
  public List<Integer> getArrayItem() {
    return arrayItem;
  }

  public void setArrayItem(List<Integer> arrayItem) {
    this.arrayItem = arrayItem;
  }

  public TypeHolderDefault arrayItem(List<Integer> arrayItem) {
    this.arrayItem = arrayItem;
    return this;
  }

  public TypeHolderDefault addArrayItemItem(Integer arrayItemItem) {
    this.arrayItem.add(arrayItemItem);
    return this;
  }


  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class TypeHolderDefault {\n");
    
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
  private static String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

