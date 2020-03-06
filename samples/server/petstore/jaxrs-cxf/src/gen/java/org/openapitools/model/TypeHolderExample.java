package org.openapitools.model;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.v3.oas.annotations.media.Schema;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
import com.fasterxml.jackson.annotation.JsonProperty;

public class TypeHolderExample  {
  
  @Schema(example = "what", required = true, description = "")
  private String stringItem;

  @Schema(example = "1.234", required = true, description = "")
  @Valid
  private BigDecimal numberItem;

  @Schema(example = "1.234", required = true, description = "")
  private Float floatItem;

  @Schema(example = "-2", required = true, description = "")
  private Integer integerItem;

  @Schema(example = "true", required = true, description = "")
  private Boolean boolItem;

  @Schema(example = "[0, 1, 2, 3]", required = true, description = "")
  private List<Integer> arrayItem = new ArrayList<Integer>();
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

  public TypeHolderExample stringItem(String stringItem) {
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

  public TypeHolderExample numberItem(BigDecimal numberItem) {
    this.numberItem = numberItem;
    return this;
  }

 /**
   * Get floatItem
   * @return floatItem
  **/
  @JsonProperty("float_item")
  @NotNull
  public Float getFloatItem() {
    return floatItem;
  }

  public void setFloatItem(Float floatItem) {
    this.floatItem = floatItem;
  }

  public TypeHolderExample floatItem(Float floatItem) {
    this.floatItem = floatItem;
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

  public TypeHolderExample integerItem(Integer integerItem) {
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

  public TypeHolderExample boolItem(Boolean boolItem) {
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

  public TypeHolderExample arrayItem(List<Integer> arrayItem) {
    this.arrayItem = arrayItem;
    return this;
  }

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
  private static String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

