package org.openapitools.model;

import com.fasterxml.jackson.annotation.JsonIgnore;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.joda.time.LocalDate;
import org.openapitools.jackson.nullable.JsonNullable;

import io.swagger.annotations.ApiModelProperty;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
import com.fasterxml.jackson.annotation.JsonProperty;

public class NullableClass extends HashMap<String, Object> {
  
  @ApiModelProperty(value = "")
  private Integer integerProp;

  @ApiModelProperty(value = "")
  private BigDecimal numberProp;

  @ApiModelProperty(value = "")
  private Boolean booleanProp;

  @ApiModelProperty(value = "")
  private String stringProp;

  @ApiModelProperty(value = "")
  private LocalDate dateProp;

  @ApiModelProperty(value = "")
  private Date datetimeProp;

  @ApiModelProperty(value = "")
  private List<Object> arrayNullableProp = null;

  @ApiModelProperty(value = "")
  private List<Object> arrayAndItemsNullableProp = null;

  @ApiModelProperty(value = "")
  private List<Object> arrayItemsNullable = null;

  @ApiModelProperty(value = "")
  private Map<String, Object> objectNullableProp = null;

  @ApiModelProperty(value = "")
  private Map<String, Object> objectAndItemsNullableProp = null;

  @ApiModelProperty(value = "")
  private Map<String, Object> objectItemsNullable = null;
 /**
   * Get integerProp
   * @return integerProp
  **/
  @JsonProperty("integer_prop")
  public Integer getIntegerProp() {
    return integerProp;
  }

  public void setIntegerProp(Integer integerProp) {
    this.integerProp = integerProp;
  }

  public NullableClass integerProp(Integer integerProp) {
    this.integerProp = integerProp;
    return this;
  }

 /**
   * Get numberProp
   * @return numberProp
  **/
  @JsonProperty("number_prop")
  public BigDecimal getNumberProp() {
    return numberProp;
  }

  public void setNumberProp(BigDecimal numberProp) {
    this.numberProp = numberProp;
  }

  public NullableClass numberProp(BigDecimal numberProp) {
    this.numberProp = numberProp;
    return this;
  }

 /**
   * Get booleanProp
   * @return booleanProp
  **/
  @JsonProperty("boolean_prop")
  public Boolean getBooleanProp() {
    return booleanProp;
  }

  public void setBooleanProp(Boolean booleanProp) {
    this.booleanProp = booleanProp;
  }

  public NullableClass booleanProp(Boolean booleanProp) {
    this.booleanProp = booleanProp;
    return this;
  }

 /**
   * Get stringProp
   * @return stringProp
  **/
  @JsonProperty("string_prop")
  public String getStringProp() {
    return stringProp;
  }

  public void setStringProp(String stringProp) {
    this.stringProp = stringProp;
  }

  public NullableClass stringProp(String stringProp) {
    this.stringProp = stringProp;
    return this;
  }

 /**
   * Get dateProp
   * @return dateProp
  **/
  @JsonProperty("date_prop")
  public LocalDate getDateProp() {
    return dateProp;
  }

  public void setDateProp(LocalDate dateProp) {
    this.dateProp = dateProp;
  }

  public NullableClass dateProp(LocalDate dateProp) {
    this.dateProp = dateProp;
    return this;
  }

 /**
   * Get datetimeProp
   * @return datetimeProp
  **/
  @JsonProperty("datetime_prop")
  public Date getDatetimeProp() {
    return datetimeProp;
  }

  public void setDatetimeProp(Date datetimeProp) {
    this.datetimeProp = datetimeProp;
  }

  public NullableClass datetimeProp(Date datetimeProp) {
    this.datetimeProp = datetimeProp;
    return this;
  }

 /**
   * Get arrayNullableProp
   * @return arrayNullableProp
  **/
  @JsonProperty("array_nullable_prop")
  public List<Object> getArrayNullableProp() {
    return arrayNullableProp;
  }

  public void setArrayNullableProp(List<Object> arrayNullableProp) {
    this.arrayNullableProp = arrayNullableProp;
  }

  public NullableClass arrayNullableProp(List<Object> arrayNullableProp) {
    this.arrayNullableProp = arrayNullableProp;
    return this;
  }

  public NullableClass addArrayNullablePropItem(Object arrayNullablePropItem) {
    this.arrayNullableProp.add(arrayNullablePropItem);
    return this;
  }

 /**
   * Get arrayAndItemsNullableProp
   * @return arrayAndItemsNullableProp
  **/
  @JsonProperty("array_and_items_nullable_prop")
  public List<Object> getArrayAndItemsNullableProp() {
    return arrayAndItemsNullableProp;
  }

  public void setArrayAndItemsNullableProp(List<Object> arrayAndItemsNullableProp) {
    this.arrayAndItemsNullableProp = arrayAndItemsNullableProp;
  }

  public NullableClass arrayAndItemsNullableProp(List<Object> arrayAndItemsNullableProp) {
    this.arrayAndItemsNullableProp = arrayAndItemsNullableProp;
    return this;
  }

  public NullableClass addArrayAndItemsNullablePropItem(Object arrayAndItemsNullablePropItem) {
    this.arrayAndItemsNullableProp.add(arrayAndItemsNullablePropItem);
    return this;
  }

 /**
   * Get arrayItemsNullable
   * @return arrayItemsNullable
  **/
  @JsonProperty("array_items_nullable")
  public List<Object> getArrayItemsNullable() {
    return arrayItemsNullable;
  }

  public void setArrayItemsNullable(List<Object> arrayItemsNullable) {
    this.arrayItemsNullable = arrayItemsNullable;
  }

  public NullableClass arrayItemsNullable(List<Object> arrayItemsNullable) {
    this.arrayItemsNullable = arrayItemsNullable;
    return this;
  }

  public NullableClass addArrayItemsNullableItem(Object arrayItemsNullableItem) {
    this.arrayItemsNullable.add(arrayItemsNullableItem);
    return this;
  }

 /**
   * Get objectNullableProp
   * @return objectNullableProp
  **/
  @JsonProperty("object_nullable_prop")
  public Map<String, Object> getObjectNullableProp() {
    return objectNullableProp;
  }

  public void setObjectNullableProp(Map<String, Object> objectNullableProp) {
    this.objectNullableProp = objectNullableProp;
  }

  public NullableClass objectNullableProp(Map<String, Object> objectNullableProp) {
    this.objectNullableProp = objectNullableProp;
    return this;
  }

  public NullableClass putObjectNullablePropItem(String key, Object objectNullablePropItem) {
    this.objectNullableProp.put(key, objectNullablePropItem);
    return this;
  }

 /**
   * Get objectAndItemsNullableProp
   * @return objectAndItemsNullableProp
  **/
  @JsonProperty("object_and_items_nullable_prop")
  public Map<String, Object> getObjectAndItemsNullableProp() {
    return objectAndItemsNullableProp;
  }

  public void setObjectAndItemsNullableProp(Map<String, Object> objectAndItemsNullableProp) {
    this.objectAndItemsNullableProp = objectAndItemsNullableProp;
  }

  public NullableClass objectAndItemsNullableProp(Map<String, Object> objectAndItemsNullableProp) {
    this.objectAndItemsNullableProp = objectAndItemsNullableProp;
    return this;
  }

  public NullableClass putObjectAndItemsNullablePropItem(String key, Object objectAndItemsNullablePropItem) {
    this.objectAndItemsNullableProp.put(key, objectAndItemsNullablePropItem);
    return this;
  }

 /**
   * Get objectItemsNullable
   * @return objectItemsNullable
  **/
  @JsonProperty("object_items_nullable")
  public Map<String, Object> getObjectItemsNullable() {
    return objectItemsNullable;
  }

  public void setObjectItemsNullable(Map<String, Object> objectItemsNullable) {
    this.objectItemsNullable = objectItemsNullable;
  }

  public NullableClass objectItemsNullable(Map<String, Object> objectItemsNullable) {
    this.objectItemsNullable = objectItemsNullable;
    return this;
  }

  public NullableClass putObjectItemsNullableItem(String key, Object objectItemsNullableItem) {
    this.objectItemsNullable.put(key, objectItemsNullableItem);
    return this;
  }


  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class NullableClass {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    integerProp: ").append(toIndentedString(integerProp)).append("\n");
    sb.append("    numberProp: ").append(toIndentedString(numberProp)).append("\n");
    sb.append("    booleanProp: ").append(toIndentedString(booleanProp)).append("\n");
    sb.append("    stringProp: ").append(toIndentedString(stringProp)).append("\n");
    sb.append("    dateProp: ").append(toIndentedString(dateProp)).append("\n");
    sb.append("    datetimeProp: ").append(toIndentedString(datetimeProp)).append("\n");
    sb.append("    arrayNullableProp: ").append(toIndentedString(arrayNullableProp)).append("\n");
    sb.append("    arrayAndItemsNullableProp: ").append(toIndentedString(arrayAndItemsNullableProp)).append("\n");
    sb.append("    arrayItemsNullable: ").append(toIndentedString(arrayItemsNullable)).append("\n");
    sb.append("    objectNullableProp: ").append(toIndentedString(objectNullableProp)).append("\n");
    sb.append("    objectAndItemsNullableProp: ").append(toIndentedString(objectAndItemsNullableProp)).append("\n");
    sb.append("    objectItemsNullable: ").append(toIndentedString(objectItemsNullable)).append("\n");
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

