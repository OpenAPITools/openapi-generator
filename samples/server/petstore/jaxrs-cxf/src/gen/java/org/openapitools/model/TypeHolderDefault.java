package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.joda.time.LocalDate;
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
import com.fasterxml.jackson.annotation.JsonProperty;

/**
  * a model to test option properties with server defaults
 **/
@ApiModel(description="a model to test option properties with server defaults")
public class TypeHolderDefault  {
  
  @ApiModelProperty(value = "")
  private String stringItem = "what";

  @ApiModelProperty(value = "")
  private Float numberItem = 1.234f;

  @ApiModelProperty(value = "")
  private Integer integerItem = -2;

  @ApiModelProperty(value = "")
  private Boolean boolItem = true;

  @ApiModelProperty(value = "")
  private LocalDate dateItem;

  @ApiModelProperty(value = "")
  private Date datetimeItem;

  @ApiModelProperty(value = "")
  private List<Integer> arrayItem = null;
 /**
   * Get stringItem
   * @return stringItem
  **/
  @JsonProperty("string_item")
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
  public Float getNumberItem() {
    return numberItem;
  }

  public void setNumberItem(Float numberItem) {
    this.numberItem = numberItem;
  }

  public TypeHolderDefault numberItem(Float numberItem) {
    this.numberItem = numberItem;
    return this;
  }

 /**
   * Get integerItem
   * @return integerItem
  **/
  @JsonProperty("integer_item")
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
   * Get dateItem
   * @return dateItem
  **/
  @JsonProperty("date_item")
  public LocalDate getDateItem() {
    return dateItem;
  }

  public void setDateItem(LocalDate dateItem) {
    this.dateItem = dateItem;
  }

  public TypeHolderDefault dateItem(LocalDate dateItem) {
    this.dateItem = dateItem;
    return this;
  }

 /**
   * Get datetimeItem
   * @return datetimeItem
  **/
  @JsonProperty("datetime_item")
  public Date getDatetimeItem() {
    return datetimeItem;
  }

  public void setDatetimeItem(Date datetimeItem) {
    this.datetimeItem = datetimeItem;
  }

  public TypeHolderDefault datetimeItem(Date datetimeItem) {
    this.datetimeItem = datetimeItem;
    return this;
  }

 /**
   * Get arrayItem
   * @return arrayItem
  **/
  @JsonProperty("array_item")
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
    sb.append("    dateItem: ").append(toIndentedString(dateItem)).append("\n");
    sb.append("    datetimeItem: ").append(toIndentedString(datetimeItem)).append("\n");
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

