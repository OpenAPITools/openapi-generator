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
  private JsonNullable<Integer> integerProp = JsonNullable.<Integer>undefined();

  @ApiModelProperty(value = "")
  private JsonNullable<BigDecimal> numberProp = JsonNullable.<BigDecimal>undefined();

  @ApiModelProperty(value = "")
  private JsonNullable<Boolean> booleanProp = JsonNullable.<Boolean>undefined();

  @ApiModelProperty(value = "")
  private JsonNullable<String> stringProp = JsonNullable.<String>undefined();

  @ApiModelProperty(value = "")
  private JsonNullable<LocalDate> dateProp = JsonNullable.<LocalDate>undefined();

  @ApiModelProperty(value = "")
  private JsonNullable<Date> datetimeProp = JsonNullable.<Date>undefined();

  @ApiModelProperty(value = "")
  private JsonNullable<List<Object>> arrayNullableProp = JsonNullable.<List<Object>>undefined();

  @ApiModelProperty(value = "")
  private JsonNullable<List<Object>> arrayAndItemsNullableProp = JsonNullable.<List<Object>>undefined();

  @ApiModelProperty(value = "")
  private List<Object> arrayItemsNullable = null;

  @ApiModelProperty(value = "")
  private JsonNullable<Map<String, Object>> objectNullableProp = JsonNullable.<Map<String, Object>>undefined();

  @ApiModelProperty(value = "")
  private JsonNullable<Map<String, Object>> objectAndItemsNullableProp = JsonNullable.<Map<String, Object>>undefined();

  @ApiModelProperty(value = "")
  private Map<String, Object> objectItemsNullable = null;
 /**
   * Get integerProp
   * @return integerProp
  **/
  @JsonIgnore
  public Integer getIntegerProp() {
    if (integerProp == null) {
      return null;
    }
    return integerProp.orElse(null);
  }

  public void setIntegerProp(Integer integerProp) {
      this.integerProp = JsonNullable.<Integer>of(integerProp);
  }

  public NullableClass integerProp(Integer integerProp) {
    this.integerProp = integerProp;
    return this;
  }

 /**
   * Get numberProp
   * @return numberProp
  **/
  @JsonIgnore
  public BigDecimal getNumberProp() {
    if (numberProp == null) {
      return null;
    }
    return numberProp.orElse(null);
  }

  public void setNumberProp(BigDecimal numberProp) {
      this.numberProp = JsonNullable.<BigDecimal>of(numberProp);
  }

  public NullableClass numberProp(BigDecimal numberProp) {
    this.numberProp = numberProp;
    return this;
  }

 /**
   * Get booleanProp
   * @return booleanProp
  **/
  @JsonIgnore
  public Boolean getBooleanProp() {
    if (booleanProp == null) {
      return null;
    }
    return booleanProp.orElse(null);
  }

  public void setBooleanProp(Boolean booleanProp) {
      this.booleanProp = JsonNullable.<Boolean>of(booleanProp);
  }

  public NullableClass booleanProp(Boolean booleanProp) {
    this.booleanProp = booleanProp;
    return this;
  }

 /**
   * Get stringProp
   * @return stringProp
  **/
  @JsonIgnore
  public String getStringProp() {
    if (stringProp == null) {
      return null;
    }
    return stringProp.orElse(null);
  }

  public void setStringProp(String stringProp) {
      this.stringProp = JsonNullable.<String>of(stringProp);
  }

  public NullableClass stringProp(String stringProp) {
    this.stringProp = stringProp;
    return this;
  }

 /**
   * Get dateProp
   * @return dateProp
  **/
  @JsonIgnore
  public LocalDate getDateProp() {
    if (dateProp == null) {
      return null;
    }
    return dateProp.orElse(null);
  }

  public void setDateProp(LocalDate dateProp) {
      this.dateProp = JsonNullable.<LocalDate>of(dateProp);
  }

  public NullableClass dateProp(LocalDate dateProp) {
    this.dateProp = dateProp;
    return this;
  }

 /**
   * Get datetimeProp
   * @return datetimeProp
  **/
  @JsonIgnore
  public Date getDatetimeProp() {
    if (datetimeProp == null) {
      return null;
    }
    return datetimeProp.orElse(null);
  }

  public void setDatetimeProp(Date datetimeProp) {
      this.datetimeProp = JsonNullable.<Date>of(datetimeProp);
  }

  public NullableClass datetimeProp(Date datetimeProp) {
    this.datetimeProp = datetimeProp;
    return this;
  }

 /**
   * Get arrayNullableProp
   * @return arrayNullableProp
  **/
  @JsonIgnore
  public List<Object> getArrayNullableProp() {
    if (arrayNullableProp == null) {
      return null;
    }
    return arrayNullableProp.orElse(null);
  }

  public void setArrayNullableProp(List<Object> arrayNullableProp) {
      this.arrayNullableProp = JsonNullable.<List<Object>>of(arrayNullableProp);
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
  @JsonIgnore
  public List<Object> getArrayAndItemsNullableProp() {
    if (arrayAndItemsNullableProp == null) {
      return null;
    }
    return arrayAndItemsNullableProp.orElse(null);
  }

  public void setArrayAndItemsNullableProp(List<Object> arrayAndItemsNullableProp) {
      this.arrayAndItemsNullableProp = JsonNullable.<List<Object>>of(arrayAndItemsNullableProp);
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
  @JsonIgnore
  public Map<String, Object> getObjectNullableProp() {
    if (objectNullableProp == null) {
      return null;
    }
    return objectNullableProp.orElse(null);
  }

  public void setObjectNullableProp(Map<String, Object> objectNullableProp) {
      this.objectNullableProp = JsonNullable.<Map<String, Object>>of(objectNullableProp);
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
  @JsonIgnore
  public Map<String, Object> getObjectAndItemsNullableProp() {
    if (objectAndItemsNullableProp == null) {
      return null;
    }
    return objectAndItemsNullableProp.orElse(null);
  }

  public void setObjectAndItemsNullableProp(Map<String, Object> objectAndItemsNullableProp) {
      this.objectAndItemsNullableProp = JsonNullable.<Map<String, Object>>of(objectAndItemsNullableProp);
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

