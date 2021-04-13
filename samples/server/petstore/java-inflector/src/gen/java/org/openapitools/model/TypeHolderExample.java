package org.openapitools.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;





@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaInflectorServerCodegen")
public class TypeHolderExample   {
  @JsonProperty("string_item")
  private String stringItem;

  @JsonProperty("number_item")
  private BigDecimal numberItem;

  @JsonProperty("float_item")
  private Float floatItem;

  @JsonProperty("integer_item")
  private Integer integerItem;

  @JsonProperty("bool_item")
  private Boolean boolItem;

  @JsonProperty("array_item")
  private List<Integer> arrayItem = new ArrayList<Integer>();

  /**
   **/
  public TypeHolderExample stringItem(String stringItem) {
    this.stringItem = stringItem;
    return this;
  }

  
  @ApiModelProperty(example = "what", required = true, value = "")
  @JsonProperty("string_item")
  public String getStringItem() {
    return stringItem;
  }
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
  @JsonProperty("number_item")
  public BigDecimal getNumberItem() {
    return numberItem;
  }
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
  @JsonProperty("float_item")
  public Float getFloatItem() {
    return floatItem;
  }
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
  @JsonProperty("integer_item")
  public Integer getIntegerItem() {
    return integerItem;
  }
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
  @JsonProperty("bool_item")
  public Boolean getBoolItem() {
    return boolItem;
  }
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
    TypeHolderExample typeHolderExample = (TypeHolderExample) o;
    return Objects.equals(stringItem, typeHolderExample.stringItem) &&
        Objects.equals(numberItem, typeHolderExample.numberItem) &&
        Objects.equals(floatItem, typeHolderExample.floatItem) &&
        Objects.equals(integerItem, typeHolderExample.integerItem) &&
        Objects.equals(boolItem, typeHolderExample.boolItem) &&
        Objects.equals(arrayItem, typeHolderExample.arrayItem);
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

