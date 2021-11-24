package org.openapitools.model;

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
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


public class AdditionalPropertiesClass  {
  
  @ApiModelProperty(value = "")
  private Map<String, String> mapString = null;

  @ApiModelProperty(value = "")
  @Valid
  private Map<String, BigDecimal> mapNumber = null;

  @ApiModelProperty(value = "")
  private Map<String, Integer> mapInteger = null;

  @ApiModelProperty(value = "")
  private Map<String, Boolean> mapBoolean = null;

  @ApiModelProperty(value = "")
  @Valid
  private Map<String, List<Integer>> mapArrayInteger = null;

  @ApiModelProperty(value = "")
  @Valid
  private Map<String, List<Object>> mapArrayAnytype = null;

  @ApiModelProperty(value = "")
  @Valid
  private Map<String, Map<String, String>> mapMapString = null;

  @ApiModelProperty(value = "")
  @Valid
  private Map<String, Map<String, Object>> mapMapAnytype = null;

  @ApiModelProperty(value = "")
  private Object anytype1;

  @ApiModelProperty(value = "")
  private Object anytype2;

  @ApiModelProperty(value = "")
  private Object anytype3;
 /**
  * Get mapString
  * @return mapString
  */
  @JsonProperty("map_string")
  public Map<String, String> getMapString() {
    return mapString;
  }

  /**
   * Sets the <code>mapString</code> property.
   */
  public void setMapString(Map<String, String> mapString) {
    this.mapString = mapString;
  }

  /**
   * Sets the <code>mapString</code> property.
   */
  public AdditionalPropertiesClass mapString(Map<String, String> mapString) {
    this.mapString = mapString;
    return this;
  }

  /**
   * Puts a new item into the <code>mapString</code> map.
   */
  public AdditionalPropertiesClass putMapStringItem(String key, String mapStringItem) {
    this.mapString.put(key, mapStringItem);
    return this;
  }

 /**
  * Get mapNumber
  * @return mapNumber
  */
  @JsonProperty("map_number")
  public Map<String, BigDecimal> getMapNumber() {
    return mapNumber;
  }

  /**
   * Sets the <code>mapNumber</code> property.
   */
  public void setMapNumber(Map<String, BigDecimal> mapNumber) {
    this.mapNumber = mapNumber;
  }

  /**
   * Sets the <code>mapNumber</code> property.
   */
  public AdditionalPropertiesClass mapNumber(Map<String, BigDecimal> mapNumber) {
    this.mapNumber = mapNumber;
    return this;
  }

  /**
   * Puts a new item into the <code>mapNumber</code> map.
   */
  public AdditionalPropertiesClass putMapNumberItem(String key, BigDecimal mapNumberItem) {
    this.mapNumber.put(key, mapNumberItem);
    return this;
  }

 /**
  * Get mapInteger
  * @return mapInteger
  */
  @JsonProperty("map_integer")
  public Map<String, Integer> getMapInteger() {
    return mapInteger;
  }

  /**
   * Sets the <code>mapInteger</code> property.
   */
  public void setMapInteger(Map<String, Integer> mapInteger) {
    this.mapInteger = mapInteger;
  }

  /**
   * Sets the <code>mapInteger</code> property.
   */
  public AdditionalPropertiesClass mapInteger(Map<String, Integer> mapInteger) {
    this.mapInteger = mapInteger;
    return this;
  }

  /**
   * Puts a new item into the <code>mapInteger</code> map.
   */
  public AdditionalPropertiesClass putMapIntegerItem(String key, Integer mapIntegerItem) {
    this.mapInteger.put(key, mapIntegerItem);
    return this;
  }

 /**
  * Get mapBoolean
  * @return mapBoolean
  */
  @JsonProperty("map_boolean")
  public Map<String, Boolean> getMapBoolean() {
    return mapBoolean;
  }

  /**
   * Sets the <code>mapBoolean</code> property.
   */
  public void setMapBoolean(Map<String, Boolean> mapBoolean) {
    this.mapBoolean = mapBoolean;
  }

  /**
   * Sets the <code>mapBoolean</code> property.
   */
  public AdditionalPropertiesClass mapBoolean(Map<String, Boolean> mapBoolean) {
    this.mapBoolean = mapBoolean;
    return this;
  }

  /**
   * Puts a new item into the <code>mapBoolean</code> map.
   */
  public AdditionalPropertiesClass putMapBooleanItem(String key, Boolean mapBooleanItem) {
    this.mapBoolean.put(key, mapBooleanItem);
    return this;
  }

 /**
  * Get mapArrayInteger
  * @return mapArrayInteger
  */
  @JsonProperty("map_array_integer")
  public Map<String, List<Integer>> getMapArrayInteger() {
    return mapArrayInteger;
  }

  /**
   * Sets the <code>mapArrayInteger</code> property.
   */
  public void setMapArrayInteger(Map<String, List<Integer>> mapArrayInteger) {
    this.mapArrayInteger = mapArrayInteger;
  }

  /**
   * Sets the <code>mapArrayInteger</code> property.
   */
  public AdditionalPropertiesClass mapArrayInteger(Map<String, List<Integer>> mapArrayInteger) {
    this.mapArrayInteger = mapArrayInteger;
    return this;
  }

  /**
   * Puts a new item into the <code>mapArrayInteger</code> map.
   */
  public AdditionalPropertiesClass putMapArrayIntegerItem(String key, List<Integer> mapArrayIntegerItem) {
    this.mapArrayInteger.put(key, mapArrayIntegerItem);
    return this;
  }

 /**
  * Get mapArrayAnytype
  * @return mapArrayAnytype
  */
  @JsonProperty("map_array_anytype")
  public Map<String, List<Object>> getMapArrayAnytype() {
    return mapArrayAnytype;
  }

  /**
   * Sets the <code>mapArrayAnytype</code> property.
   */
  public void setMapArrayAnytype(Map<String, List<Object>> mapArrayAnytype) {
    this.mapArrayAnytype = mapArrayAnytype;
  }

  /**
   * Sets the <code>mapArrayAnytype</code> property.
   */
  public AdditionalPropertiesClass mapArrayAnytype(Map<String, List<Object>> mapArrayAnytype) {
    this.mapArrayAnytype = mapArrayAnytype;
    return this;
  }

  /**
   * Puts a new item into the <code>mapArrayAnytype</code> map.
   */
  public AdditionalPropertiesClass putMapArrayAnytypeItem(String key, List<Object> mapArrayAnytypeItem) {
    this.mapArrayAnytype.put(key, mapArrayAnytypeItem);
    return this;
  }

 /**
  * Get mapMapString
  * @return mapMapString
  */
  @JsonProperty("map_map_string")
  public Map<String, Map<String, String>> getMapMapString() {
    return mapMapString;
  }

  /**
   * Sets the <code>mapMapString</code> property.
   */
  public void setMapMapString(Map<String, Map<String, String>> mapMapString) {
    this.mapMapString = mapMapString;
  }

  /**
   * Sets the <code>mapMapString</code> property.
   */
  public AdditionalPropertiesClass mapMapString(Map<String, Map<String, String>> mapMapString) {
    this.mapMapString = mapMapString;
    return this;
  }

  /**
   * Puts a new item into the <code>mapMapString</code> map.
   */
  public AdditionalPropertiesClass putMapMapStringItem(String key, Map<String, String> mapMapStringItem) {
    this.mapMapString.put(key, mapMapStringItem);
    return this;
  }

 /**
  * Get mapMapAnytype
  * @return mapMapAnytype
  */
  @JsonProperty("map_map_anytype")
  public Map<String, Map<String, Object>> getMapMapAnytype() {
    return mapMapAnytype;
  }

  /**
   * Sets the <code>mapMapAnytype</code> property.
   */
  public void setMapMapAnytype(Map<String, Map<String, Object>> mapMapAnytype) {
    this.mapMapAnytype = mapMapAnytype;
  }

  /**
   * Sets the <code>mapMapAnytype</code> property.
   */
  public AdditionalPropertiesClass mapMapAnytype(Map<String, Map<String, Object>> mapMapAnytype) {
    this.mapMapAnytype = mapMapAnytype;
    return this;
  }

  /**
   * Puts a new item into the <code>mapMapAnytype</code> map.
   */
  public AdditionalPropertiesClass putMapMapAnytypeItem(String key, Map<String, Object> mapMapAnytypeItem) {
    this.mapMapAnytype.put(key, mapMapAnytypeItem);
    return this;
  }

 /**
  * Get anytype1
  * @return anytype1
  */
  @JsonProperty("anytype_1")
  public Object getAnytype1() {
    return anytype1;
  }

  /**
   * Sets the <code>anytype1</code> property.
   */
  public void setAnytype1(Object anytype1) {
    this.anytype1 = anytype1;
  }

  /**
   * Sets the <code>anytype1</code> property.
   */
  public AdditionalPropertiesClass anytype1(Object anytype1) {
    this.anytype1 = anytype1;
    return this;
  }

 /**
  * Get anytype2
  * @return anytype2
  */
  @JsonProperty("anytype_2")
  public Object getAnytype2() {
    return anytype2;
  }

  /**
   * Sets the <code>anytype2</code> property.
   */
  public void setAnytype2(Object anytype2) {
    this.anytype2 = anytype2;
  }

  /**
   * Sets the <code>anytype2</code> property.
   */
  public AdditionalPropertiesClass anytype2(Object anytype2) {
    this.anytype2 = anytype2;
    return this;
  }

 /**
  * Get anytype3
  * @return anytype3
  */
  @JsonProperty("anytype_3")
  public Object getAnytype3() {
    return anytype3;
  }

  /**
   * Sets the <code>anytype3</code> property.
   */
  public void setAnytype3(Object anytype3) {
    this.anytype3 = anytype3;
  }

  /**
   * Sets the <code>anytype3</code> property.
   */
  public AdditionalPropertiesClass anytype3(Object anytype3) {
    this.anytype3 = anytype3;
    return this;
  }


  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class AdditionalPropertiesClass {\n");
    
    sb.append("    mapString: ").append(toIndentedString(mapString)).append("\n");
    sb.append("    mapNumber: ").append(toIndentedString(mapNumber)).append("\n");
    sb.append("    mapInteger: ").append(toIndentedString(mapInteger)).append("\n");
    sb.append("    mapBoolean: ").append(toIndentedString(mapBoolean)).append("\n");
    sb.append("    mapArrayInteger: ").append(toIndentedString(mapArrayInteger)).append("\n");
    sb.append("    mapArrayAnytype: ").append(toIndentedString(mapArrayAnytype)).append("\n");
    sb.append("    mapMapString: ").append(toIndentedString(mapMapString)).append("\n");
    sb.append("    mapMapAnytype: ").append(toIndentedString(mapMapAnytype)).append("\n");
    sb.append("    anytype1: ").append(toIndentedString(anytype1)).append("\n");
    sb.append("    anytype2: ").append(toIndentedString(anytype2)).append("\n");
    sb.append("    anytype3: ").append(toIndentedString(anytype3)).append("\n");
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

