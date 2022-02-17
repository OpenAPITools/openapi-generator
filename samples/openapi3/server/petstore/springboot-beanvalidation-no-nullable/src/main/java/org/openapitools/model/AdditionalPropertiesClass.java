package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import java.math.BigDecimal;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import javax.annotation.Generated;

/**
 * AdditionalPropertiesClass
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class AdditionalPropertiesClass   {

  @JsonProperty("map_string")
  @Valid
  private Map<String, String> mapString = null;

  @JsonProperty("map_number")
  @Valid
  private Map<String, BigDecimal> mapNumber = null;

  @JsonProperty("map_integer")
  @Valid
  private Map<String, Integer> mapInteger = null;

  @JsonProperty("map_boolean")
  @Valid
  private Map<String, Boolean> mapBoolean = null;

  @JsonProperty("map_array_integer")
  @Valid
  private Map<String, List<Integer>> mapArrayInteger = null;

  @JsonProperty("map_array_anytype")
  @Valid
  private Map<String, List<Object>> mapArrayAnytype = null;

  @JsonProperty("map_map_string")
  @Valid
  private Map<String, Map<String, String>> mapMapString = null;

  @JsonProperty("map_map_anytype")
  @Valid
  private Map<String, Map<String, Object>> mapMapAnytype = null;

  @JsonProperty("anytype_1")
  private Object anytype1;

  @JsonProperty("anytype_2")
  private Object anytype2;

  @JsonProperty("anytype_3")
  private Object anytype3;

  public AdditionalPropertiesClass mapString(Map<String, String> mapString) {
    this.mapString = mapString;
    return this;
  }

  public AdditionalPropertiesClass putMapStringItem(String key, String mapStringItem) {
    if (this.mapString == null) {
      this.mapString = new HashMap<>();
    }
    this.mapString.put(key, mapStringItem);
    return this;
  }

  /**
   * Get mapString
   * @return mapString
  */
  
  @Schema(name = "map_string", required = false)
  public Map<String, String> getMapString() {
    return mapString;
  }

  public void setMapString(Map<String, String> mapString) {
    this.mapString = mapString;
  }

  public AdditionalPropertiesClass mapNumber(Map<String, BigDecimal> mapNumber) {
    this.mapNumber = mapNumber;
    return this;
  }

  public AdditionalPropertiesClass putMapNumberItem(String key, BigDecimal mapNumberItem) {
    if (this.mapNumber == null) {
      this.mapNumber = new HashMap<>();
    }
    this.mapNumber.put(key, mapNumberItem);
    return this;
  }

  /**
   * Get mapNumber
   * @return mapNumber
  */
  @Valid 
  @Schema(name = "map_number", required = false)
  public Map<String, BigDecimal> getMapNumber() {
    return mapNumber;
  }

  public void setMapNumber(Map<String, BigDecimal> mapNumber) {
    this.mapNumber = mapNumber;
  }

  public AdditionalPropertiesClass mapInteger(Map<String, Integer> mapInteger) {
    this.mapInteger = mapInteger;
    return this;
  }

  public AdditionalPropertiesClass putMapIntegerItem(String key, Integer mapIntegerItem) {
    if (this.mapInteger == null) {
      this.mapInteger = new HashMap<>();
    }
    this.mapInteger.put(key, mapIntegerItem);
    return this;
  }

  /**
   * Get mapInteger
   * @return mapInteger
  */
  
  @Schema(name = "map_integer", required = false)
  public Map<String, Integer> getMapInteger() {
    return mapInteger;
  }

  public void setMapInteger(Map<String, Integer> mapInteger) {
    this.mapInteger = mapInteger;
  }

  public AdditionalPropertiesClass mapBoolean(Map<String, Boolean> mapBoolean) {
    this.mapBoolean = mapBoolean;
    return this;
  }

  public AdditionalPropertiesClass putMapBooleanItem(String key, Boolean mapBooleanItem) {
    if (this.mapBoolean == null) {
      this.mapBoolean = new HashMap<>();
    }
    this.mapBoolean.put(key, mapBooleanItem);
    return this;
  }

  /**
   * Get mapBoolean
   * @return mapBoolean
  */
  
  @Schema(name = "map_boolean", required = false)
  public Map<String, Boolean> getMapBoolean() {
    return mapBoolean;
  }

  public void setMapBoolean(Map<String, Boolean> mapBoolean) {
    this.mapBoolean = mapBoolean;
  }

  public AdditionalPropertiesClass mapArrayInteger(Map<String, List<Integer>> mapArrayInteger) {
    this.mapArrayInteger = mapArrayInteger;
    return this;
  }

  public AdditionalPropertiesClass putMapArrayIntegerItem(String key, List<Integer> mapArrayIntegerItem) {
    if (this.mapArrayInteger == null) {
      this.mapArrayInteger = new HashMap<>();
    }
    this.mapArrayInteger.put(key, mapArrayIntegerItem);
    return this;
  }

  /**
   * Get mapArrayInteger
   * @return mapArrayInteger
  */
  @Valid 
  @Schema(name = "map_array_integer", required = false)
  public Map<String, List<Integer>> getMapArrayInteger() {
    return mapArrayInteger;
  }

  public void setMapArrayInteger(Map<String, List<Integer>> mapArrayInteger) {
    this.mapArrayInteger = mapArrayInteger;
  }

  public AdditionalPropertiesClass mapArrayAnytype(Map<String, List<Object>> mapArrayAnytype) {
    this.mapArrayAnytype = mapArrayAnytype;
    return this;
  }

  public AdditionalPropertiesClass putMapArrayAnytypeItem(String key, List<Object> mapArrayAnytypeItem) {
    if (this.mapArrayAnytype == null) {
      this.mapArrayAnytype = new HashMap<>();
    }
    this.mapArrayAnytype.put(key, mapArrayAnytypeItem);
    return this;
  }

  /**
   * Get mapArrayAnytype
   * @return mapArrayAnytype
  */
  @Valid 
  @Schema(name = "map_array_anytype", required = false)
  public Map<String, List<Object>> getMapArrayAnytype() {
    return mapArrayAnytype;
  }

  public void setMapArrayAnytype(Map<String, List<Object>> mapArrayAnytype) {
    this.mapArrayAnytype = mapArrayAnytype;
  }

  public AdditionalPropertiesClass mapMapString(Map<String, Map<String, String>> mapMapString) {
    this.mapMapString = mapMapString;
    return this;
  }

  public AdditionalPropertiesClass putMapMapStringItem(String key, Map<String, String> mapMapStringItem) {
    if (this.mapMapString == null) {
      this.mapMapString = new HashMap<>();
    }
    this.mapMapString.put(key, mapMapStringItem);
    return this;
  }

  /**
   * Get mapMapString
   * @return mapMapString
  */
  @Valid 
  @Schema(name = "map_map_string", required = false)
  public Map<String, Map<String, String>> getMapMapString() {
    return mapMapString;
  }

  public void setMapMapString(Map<String, Map<String, String>> mapMapString) {
    this.mapMapString = mapMapString;
  }

  public AdditionalPropertiesClass mapMapAnytype(Map<String, Map<String, Object>> mapMapAnytype) {
    this.mapMapAnytype = mapMapAnytype;
    return this;
  }

  public AdditionalPropertiesClass putMapMapAnytypeItem(String key, Map<String, Object> mapMapAnytypeItem) {
    if (this.mapMapAnytype == null) {
      this.mapMapAnytype = new HashMap<>();
    }
    this.mapMapAnytype.put(key, mapMapAnytypeItem);
    return this;
  }

  /**
   * Get mapMapAnytype
   * @return mapMapAnytype
  */
  @Valid 
  @Schema(name = "map_map_anytype", required = false)
  public Map<String, Map<String, Object>> getMapMapAnytype() {
    return mapMapAnytype;
  }

  public void setMapMapAnytype(Map<String, Map<String, Object>> mapMapAnytype) {
    this.mapMapAnytype = mapMapAnytype;
  }

  public AdditionalPropertiesClass anytype1(Object anytype1) {
    this.anytype1 = anytype1;
    return this;
  }

  /**
   * Get anytype1
   * @return anytype1
  */
  
  @Schema(name = "anytype_1", required = false)
  public Object getAnytype1() {
    return anytype1;
  }

  public void setAnytype1(Object anytype1) {
    this.anytype1 = anytype1;
  }

  public AdditionalPropertiesClass anytype2(Object anytype2) {
    this.anytype2 = anytype2;
    return this;
  }

  /**
   * Get anytype2
   * @return anytype2
  */
  
  @Schema(name = "anytype_2", required = false)
  public Object getAnytype2() {
    return anytype2;
  }

  public void setAnytype2(Object anytype2) {
    this.anytype2 = anytype2;
  }

  public AdditionalPropertiesClass anytype3(Object anytype3) {
    this.anytype3 = anytype3;
    return this;
  }

  /**
   * Get anytype3
   * @return anytype3
  */
  
  @Schema(name = "anytype_3", required = false)
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
    return Objects.equals(this.mapString, additionalPropertiesClass.mapString) &&
        Objects.equals(this.mapNumber, additionalPropertiesClass.mapNumber) &&
        Objects.equals(this.mapInteger, additionalPropertiesClass.mapInteger) &&
        Objects.equals(this.mapBoolean, additionalPropertiesClass.mapBoolean) &&
        Objects.equals(this.mapArrayInteger, additionalPropertiesClass.mapArrayInteger) &&
        Objects.equals(this.mapArrayAnytype, additionalPropertiesClass.mapArrayAnytype) &&
        Objects.equals(this.mapMapString, additionalPropertiesClass.mapMapString) &&
        Objects.equals(this.mapMapAnytype, additionalPropertiesClass.mapMapAnytype) &&
        Objects.equals(this.anytype1, additionalPropertiesClass.anytype1) &&
        Objects.equals(this.anytype2, additionalPropertiesClass.anytype2) &&
        Objects.equals(this.anytype3, additionalPropertiesClass.anytype3);
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

