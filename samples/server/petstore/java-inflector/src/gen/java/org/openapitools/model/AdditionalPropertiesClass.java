package org.openapitools.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.math.BigDecimal;
import java.util.HashMap;
import java.util.List;
import java.util.Map;





@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaInflectorServerCodegen")
public class AdditionalPropertiesClass   {
  @JsonProperty("map_string")
  private Map<String, String> mapString = null;

  @JsonProperty("map_number")
  private Map<String, BigDecimal> mapNumber = null;

  @JsonProperty("map_integer")
  private Map<String, Integer> mapInteger = null;

  @JsonProperty("map_boolean")
  private Map<String, Boolean> mapBoolean = null;

  @JsonProperty("map_array_integer")
  private Map<String, List<Integer>> mapArrayInteger = null;

  @JsonProperty("map_array_anytype")
  private Map<String, List<Object>> mapArrayAnytype = null;

  @JsonProperty("map_map_string")
  private Map<String, Map<String, String>> mapMapString = null;

  @JsonProperty("map_map_anytype")
  private Map<String, Map<String, Object>> mapMapAnytype = null;

  @JsonProperty("anytype_1")
  private Object anytype1;

  @JsonProperty("anytype_2")
  private Object anytype2;

  @JsonProperty("anytype_3")
  private Object anytype3;

  /**
   **/
  public AdditionalPropertiesClass mapString(Map<String, String> mapString) {
    this.mapString = mapString;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("map_string")
  public Map<String, String> getMapString() {
    return mapString;
  }
  public void setMapString(Map<String, String> mapString) {
    this.mapString = mapString;
  }

  /**
   **/
  public AdditionalPropertiesClass mapNumber(Map<String, BigDecimal> mapNumber) {
    this.mapNumber = mapNumber;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("map_number")
  public Map<String, BigDecimal> getMapNumber() {
    return mapNumber;
  }
  public void setMapNumber(Map<String, BigDecimal> mapNumber) {
    this.mapNumber = mapNumber;
  }

  /**
   **/
  public AdditionalPropertiesClass mapInteger(Map<String, Integer> mapInteger) {
    this.mapInteger = mapInteger;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("map_integer")
  public Map<String, Integer> getMapInteger() {
    return mapInteger;
  }
  public void setMapInteger(Map<String, Integer> mapInteger) {
    this.mapInteger = mapInteger;
  }

  /**
   **/
  public AdditionalPropertiesClass mapBoolean(Map<String, Boolean> mapBoolean) {
    this.mapBoolean = mapBoolean;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("map_boolean")
  public Map<String, Boolean> getMapBoolean() {
    return mapBoolean;
  }
  public void setMapBoolean(Map<String, Boolean> mapBoolean) {
    this.mapBoolean = mapBoolean;
  }

  /**
   **/
  public AdditionalPropertiesClass mapArrayInteger(Map<String, List<Integer>> mapArrayInteger) {
    this.mapArrayInteger = mapArrayInteger;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("map_array_integer")
  public Map<String, List<Integer>> getMapArrayInteger() {
    return mapArrayInteger;
  }
  public void setMapArrayInteger(Map<String, List<Integer>> mapArrayInteger) {
    this.mapArrayInteger = mapArrayInteger;
  }

  /**
   **/
  public AdditionalPropertiesClass mapArrayAnytype(Map<String, List<Object>> mapArrayAnytype) {
    this.mapArrayAnytype = mapArrayAnytype;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("map_array_anytype")
  public Map<String, List<Object>> getMapArrayAnytype() {
    return mapArrayAnytype;
  }
  public void setMapArrayAnytype(Map<String, List<Object>> mapArrayAnytype) {
    this.mapArrayAnytype = mapArrayAnytype;
  }

  /**
   **/
  public AdditionalPropertiesClass mapMapString(Map<String, Map<String, String>> mapMapString) {
    this.mapMapString = mapMapString;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("map_map_string")
  public Map<String, Map<String, String>> getMapMapString() {
    return mapMapString;
  }
  public void setMapMapString(Map<String, Map<String, String>> mapMapString) {
    this.mapMapString = mapMapString;
  }

  /**
   **/
  public AdditionalPropertiesClass mapMapAnytype(Map<String, Map<String, Object>> mapMapAnytype) {
    this.mapMapAnytype = mapMapAnytype;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("map_map_anytype")
  public Map<String, Map<String, Object>> getMapMapAnytype() {
    return mapMapAnytype;
  }
  public void setMapMapAnytype(Map<String, Map<String, Object>> mapMapAnytype) {
    this.mapMapAnytype = mapMapAnytype;
  }

  /**
   **/
  public AdditionalPropertiesClass anytype1(Object anytype1) {
    this.anytype1 = anytype1;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("anytype_1")
  public Object getAnytype1() {
    return anytype1;
  }
  public void setAnytype1(Object anytype1) {
    this.anytype1 = anytype1;
  }

  /**
   **/
  public AdditionalPropertiesClass anytype2(Object anytype2) {
    this.anytype2 = anytype2;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("anytype_2")
  public Object getAnytype2() {
    return anytype2;
  }
  public void setAnytype2(Object anytype2) {
    this.anytype2 = anytype2;
  }

  /**
   **/
  public AdditionalPropertiesClass anytype3(Object anytype3) {
    this.anytype3 = anytype3;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("anytype_3")
  public Object getAnytype3() {
    return anytype3;
  }
  public void setAnytype3(Object anytype3) {
    this.anytype3 = anytype3;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    AdditionalPropertiesClass additionalPropertiesClass = (AdditionalPropertiesClass) o;
    return Objects.equals(mapString, additionalPropertiesClass.mapString) &&
        Objects.equals(mapNumber, additionalPropertiesClass.mapNumber) &&
        Objects.equals(mapInteger, additionalPropertiesClass.mapInteger) &&
        Objects.equals(mapBoolean, additionalPropertiesClass.mapBoolean) &&
        Objects.equals(mapArrayInteger, additionalPropertiesClass.mapArrayInteger) &&
        Objects.equals(mapArrayAnytype, additionalPropertiesClass.mapArrayAnytype) &&
        Objects.equals(mapMapString, additionalPropertiesClass.mapMapString) &&
        Objects.equals(mapMapAnytype, additionalPropertiesClass.mapMapAnytype) &&
        Objects.equals(anytype1, additionalPropertiesClass.anytype1) &&
        Objects.equals(anytype2, additionalPropertiesClass.anytype2) &&
        Objects.equals(anytype3, additionalPropertiesClass.anytype3);
  }

  @Override
  public int hashCode() {
    return Objects.hash(mapString, mapNumber, mapInteger, mapBoolean, mapArrayInteger, mapArrayAnytype, mapMapString, mapMapAnytype, anytype1, anytype2, anytype3);
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
  private String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

