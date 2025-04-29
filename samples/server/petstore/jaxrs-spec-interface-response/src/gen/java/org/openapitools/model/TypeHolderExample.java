package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.annotation.JsonTypeName;



@JsonTypeName("TypeHolderExample")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.14.0-SNAPSHOT")
public class TypeHolderExample  implements Serializable {
  private String stringItem;
  private BigDecimal numberItem;
  private Float floatItem;
  private Integer integerItem;
  private Boolean boolItem;
  private @Valid List<Integer> arrayItem = new ArrayList<>();

  public TypeHolderExample() {
  }

  @JsonCreator
  public TypeHolderExample(
    @JsonProperty(required = true, value = "string_item") String stringItem,
    @JsonProperty(required = true, value = "number_item") BigDecimal numberItem,
    @JsonProperty(required = true, value = "float_item") Float floatItem,
    @JsonProperty(required = true, value = "integer_item") Integer integerItem,
    @JsonProperty(required = true, value = "bool_item") Boolean boolItem,
    @JsonProperty(required = true, value = "array_item") List<Integer> arrayItem
  ) {
    this.stringItem = stringItem;
    this.numberItem = numberItem;
    this.floatItem = floatItem;
    this.integerItem = integerItem;
    this.boolItem = boolItem;
    this.arrayItem = arrayItem;
  }

  /**
   **/
  public TypeHolderExample stringItem(String stringItem) {
    this.stringItem = stringItem;
    return this;
  }

  
  @ApiModelProperty(example = "what", required = true, value = "")
  @JsonProperty(required = true, value = "string_item")
  @NotNull public String getStringItem() {
    return stringItem;
  }

  @JsonProperty(required = true, value = "string_item")
  public void setStringItem(String stringItem) {
    this.stringItem = stringItem;
  }

  /**
   **/
  public TypeHolderExample numberItem(BigDecimal numberItem) {
    this.numberItem = numberItem;
    return this;
  }

  
  @ApiModelProperty(example = "1.234", required = true, value = "")
  @JsonProperty(required = true, value = "number_item")
  @NotNull @Valid public BigDecimal getNumberItem() {
    return numberItem;
  }

  @JsonProperty(required = true, value = "number_item")
  public void setNumberItem(BigDecimal numberItem) {
    this.numberItem = numberItem;
  }

  /**
   **/
  public TypeHolderExample floatItem(Float floatItem) {
    this.floatItem = floatItem;
    return this;
  }

  
  @ApiModelProperty(example = "1.234", required = true, value = "")
  @JsonProperty(required = true, value = "float_item")
  @NotNull public Float getFloatItem() {
    return floatItem;
  }

  @JsonProperty(required = true, value = "float_item")
  public void setFloatItem(Float floatItem) {
    this.floatItem = floatItem;
  }

  /**
   **/
  public TypeHolderExample integerItem(Integer integerItem) {
    this.integerItem = integerItem;
    return this;
  }

  
  @ApiModelProperty(example = "-2", required = true, value = "")
  @JsonProperty(required = true, value = "integer_item")
  @NotNull public Integer getIntegerItem() {
    return integerItem;
  }

  @JsonProperty(required = true, value = "integer_item")
  public void setIntegerItem(Integer integerItem) {
    this.integerItem = integerItem;
  }

  /**
   **/
  public TypeHolderExample boolItem(Boolean boolItem) {
    this.boolItem = boolItem;
    return this;
  }

  
  @ApiModelProperty(example = "true", required = true, value = "")
  @JsonProperty(required = true, value = "bool_item")
  @NotNull public Boolean getBoolItem() {
    return boolItem;
  }

  @JsonProperty(required = true, value = "bool_item")
  public void setBoolItem(Boolean boolItem) {
    this.boolItem = boolItem;
  }

  /**
   **/
  public TypeHolderExample arrayItem(List<Integer> arrayItem) {
    this.arrayItem = arrayItem;
    return this;
  }

  
  @ApiModelProperty(example = "[0, 1, 2, 3]", required = true, value = "")
  @JsonProperty(required = true, value = "array_item")
  @NotNull public List<Integer> getArrayItem() {
    return arrayItem;
  }

  @JsonProperty(required = true, value = "array_item")
  public void setArrayItem(List<Integer> arrayItem) {
    this.arrayItem = arrayItem;
  }

  public TypeHolderExample addArrayItemItem(Integer arrayItemItem) {
    if (this.arrayItem == null) {
      this.arrayItem = new ArrayList<>();
    }

    this.arrayItem.add(arrayItemItem);
    return this;
  }

  public TypeHolderExample removeArrayItemItem(Integer arrayItemItem) {
    if (arrayItemItem != null && this.arrayItem != null) {
      this.arrayItem.remove(arrayItemItem);
    }

    return this;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    TypeHolderExample typeHolderExample = (TypeHolderExample) o;
    return Objects.equals(this.stringItem, typeHolderExample.stringItem) &&
        Objects.equals(this.numberItem, typeHolderExample.numberItem) &&
        Objects.equals(this.floatItem, typeHolderExample.floatItem) &&
        Objects.equals(this.integerItem, typeHolderExample.integerItem) &&
        Objects.equals(this.boolItem, typeHolderExample.boolItem) &&
        Objects.equals(this.arrayItem, typeHolderExample.arrayItem);
  }

  @Override
  public int hashCode() {
    return Objects.hash(stringItem, numberItem, floatItem, integerItem, boolItem, arrayItem);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class TypeHolderExample {\n");
    
    sb.append("    stringItem: ").append(toIndentedString(stringItem)).append("\n");
    sb.append("    numberItem: ").append(toIndentedString(numberItem)).append("\n");
    sb.append("    floatItem: ").append(toIndentedString(floatItem)).append("\n");
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

