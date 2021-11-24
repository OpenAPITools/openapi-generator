package org.openapitools.model;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.ApiModelProperty;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;


public class TypeHolderExample  {
  
  @ApiModelProperty(example = "what", required = true, value = "")
  private String stringItem;

  @ApiModelProperty(example = "1.234", required = true, value = "")
  @Valid
  private BigDecimal numberItem;

  @ApiModelProperty(example = "1.234", required = true, value = "")
  private Float floatItem;

  @ApiModelProperty(example = "-2", required = true, value = "")
  private Integer integerItem;

  @ApiModelProperty(example = "true", required = true, value = "")
  private Boolean boolItem;

  @ApiModelProperty(example = "[0, 1, 2, 3]", required = true, value = "")
  private List<Integer> arrayItem = new ArrayList<>();
 /**
  * Get stringItem
  * @return stringItem
  */
  @JsonProperty("string_item")
  @NotNull
  public String getStringItem() {
    return stringItem;
  }

  /**
   * Sets the <code>stringItem</code> property.
   */
  public void setStringItem(String stringItem) {
    this.stringItem = stringItem;
  }

  /**
   * Sets the <code>stringItem</code> property.
   */
  public TypeHolderExample stringItem(String stringItem) {
    this.stringItem = stringItem;
    return this;
  }

 /**
  * Get numberItem
  * @return numberItem
  */
  @JsonProperty("number_item")
  @NotNull
  public BigDecimal getNumberItem() {
    return numberItem;
  }

  /**
   * Sets the <code>numberItem</code> property.
   */
  public void setNumberItem(BigDecimal numberItem) {
    this.numberItem = numberItem;
  }

  /**
   * Sets the <code>numberItem</code> property.
   */
  public TypeHolderExample numberItem(BigDecimal numberItem) {
    this.numberItem = numberItem;
    return this;
  }

 /**
  * Get floatItem
  * @return floatItem
  */
  @JsonProperty("float_item")
  @NotNull
  public Float getFloatItem() {
    return floatItem;
  }

  /**
   * Sets the <code>floatItem</code> property.
   */
  public void setFloatItem(Float floatItem) {
    this.floatItem = floatItem;
  }

  /**
   * Sets the <code>floatItem</code> property.
   */
  public TypeHolderExample floatItem(Float floatItem) {
    this.floatItem = floatItem;
    return this;
  }

 /**
  * Get integerItem
  * @return integerItem
  */
  @JsonProperty("integer_item")
  @NotNull
  public Integer getIntegerItem() {
    return integerItem;
  }

  /**
   * Sets the <code>integerItem</code> property.
   */
  public void setIntegerItem(Integer integerItem) {
    this.integerItem = integerItem;
  }

  /**
   * Sets the <code>integerItem</code> property.
   */
  public TypeHolderExample integerItem(Integer integerItem) {
    this.integerItem = integerItem;
    return this;
  }

 /**
  * Get boolItem
  * @return boolItem
  */
  @JsonProperty("bool_item")
  @NotNull
  public Boolean getBoolItem() {
    return boolItem;
  }

  /**
   * Sets the <code>boolItem</code> property.
   */
  public void setBoolItem(Boolean boolItem) {
    this.boolItem = boolItem;
  }

  /**
   * Sets the <code>boolItem</code> property.
   */
  public TypeHolderExample boolItem(Boolean boolItem) {
    this.boolItem = boolItem;
    return this;
  }

 /**
  * Get arrayItem
  * @return arrayItem
  */
  @JsonProperty("array_item")
  @NotNull
  public List<Integer> getArrayItem() {
    return arrayItem;
  }

  /**
   * Sets the <code>arrayItem</code> property.
   */
  public void setArrayItem(List<Integer> arrayItem) {
    this.arrayItem = arrayItem;
  }

  /**
   * Sets the <code>arrayItem</code> property.
   */
  public TypeHolderExample arrayItem(List<Integer> arrayItem) {
    this.arrayItem = arrayItem;
    return this;
  }

  /**
   * Adds a new item to the <code>arrayItem</code> list.
   */
  public TypeHolderExample addArrayItemItem(Integer arrayItemItem) {
    this.arrayItem.add(arrayItemItem);
    return this;
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
  private static String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

