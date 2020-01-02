package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.math.BigDecimal;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;



public class AdditionalPropertiesClass  implements Serializable {
  
  private @Valid Map<String, String> mapString = new HashMap<String, String>();
  private @Valid Map<String, BigDecimal> mapNumber = new HashMap<String, BigDecimal>();
  private @Valid Map<String, Integer> mapInteger = new HashMap<String, Integer>();
  private @Valid Map<String, Boolean> mapBoolean = new HashMap<String, Boolean>();
  private @Valid Map<String, List<Integer>> mapArrayInteger = new HashMap<String, List<Integer>>();
  private @Valid Map<String, List<Object>> mapArrayAnytype = new HashMap<String, List<Object>>();
  private @Valid Map<String, Map<String, String>> mapMapString = new HashMap<String, Map<String, String>>();
  private @Valid Map<String, Map<String, Object>> mapMapAnytype = new HashMap<String, Map<String, Object>>();
  private @Valid Object anytype1;
  private @Valid Object anytype2;
  private @Valid Object anytype3;

  public AdditionalPropertiesClass(Map<String, String> mapString, Map<String, BigDecimal> mapNumber, Map<String, Integer> mapInteger, Map<String, Boolean> mapBoolean, Map<String, List<Integer>> mapArrayInteger, Map<String, List<Object>> mapArrayAnytype, Map<String, Map<String, String>> mapMapString, Map<String, Map<String, Object>> mapMapAnytype, Object anytype1, Object anytype2, Object anytype3) {
    this.mapString = mapString;
    this.mapNumber = mapNumber;
    this.mapInteger = mapInteger;
    this.mapBoolean = mapBoolean;
    this.mapArrayInteger = mapArrayInteger;
    this.mapArrayAnytype = mapArrayAnytype;
    this.mapMapString = mapMapString;
    this.mapMapAnytype = mapMapAnytype;
    this.anytype1 = anytype1;
    this.anytype2 = anytype2;
    this.anytype3 = anytype3;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("map_string")
  public Map<String, String> getMapString() {
    return mapString;
  }

  public void setMapString(Map<String, String> mapString) {
    this.mapString = mapString;
  }
  @ApiModelProperty(value = "")
  @JsonProperty("map_number")
  public Map<String, BigDecimal> getMapNumber() {
    return mapNumber;
  }

  public void setMapNumber(Map<String, BigDecimal> mapNumber) {
    this.mapNumber = mapNumber;
  }
  @ApiModelProperty(value = "")
  @JsonProperty("map_integer")
  public Map<String, Integer> getMapInteger() {
    return mapInteger;
  }

  public void setMapInteger(Map<String, Integer> mapInteger) {
    this.mapInteger = mapInteger;
  }
  @ApiModelProperty(value = "")
  @JsonProperty("map_boolean")
  public Map<String, Boolean> getMapBoolean() {
    return mapBoolean;
  }

  public void setMapBoolean(Map<String, Boolean> mapBoolean) {
    this.mapBoolean = mapBoolean;
  }
  @ApiModelProperty(value = "")
  @JsonProperty("map_array_integer")
  public Map<String, List<Integer>> getMapArrayInteger() {
    return mapArrayInteger;
  }

  public void setMapArrayInteger(Map<String, List<Integer>> mapArrayInteger) {
    this.mapArrayInteger = mapArrayInteger;
  }
  @ApiModelProperty(value = "")
  @JsonProperty("map_array_anytype")
  public Map<String, List<Object>> getMapArrayAnytype() {
    return mapArrayAnytype;
  }

  public void setMapArrayAnytype(Map<String, List<Object>> mapArrayAnytype) {
    this.mapArrayAnytype = mapArrayAnytype;
  }
  @ApiModelProperty(value = "")
  @JsonProperty("map_map_string")
  public Map<String, Map<String, String>> getMapMapString() {
    return mapMapString;
  }

  public void setMapMapString(Map<String, Map<String, String>> mapMapString) {
    this.mapMapString = mapMapString;
  }
  @ApiModelProperty(value = "")
  @JsonProperty("map_map_anytype")
  public Map<String, Map<String, Object>> getMapMapAnytype() {
    return mapMapAnytype;
  }

  public void setMapMapAnytype(Map<String, Map<String, Object>> mapMapAnytype) {
    this.mapMapAnytype = mapMapAnytype;
  }
  @ApiModelProperty(value = "")
  @JsonProperty("anytype_1")
  public Object getAnytype1() {
    return anytype1;
  }

  public void setAnytype1(Object anytype1) {
    this.anytype1 = anytype1;
  }
  @ApiModelProperty(value = "")
  @JsonProperty("anytype_2")
  public Object getAnytype2() {
    return anytype2;
  }

  public void setAnytype2(Object anytype2) {
    this.anytype2 = anytype2;
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
  public boolean equals(java.lang.Object o) {
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
  private String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }

  public static Builder builder() {
    return new Builder();
  }

  public static class Builder {
    private Map<String, String> mapString = new HashMap<String, String>();
    private Map<String, BigDecimal> mapNumber = new HashMap<String, BigDecimal>();
    private Map<String, Integer> mapInteger = new HashMap<String, Integer>();
    private Map<String, Boolean> mapBoolean = new HashMap<String, Boolean>();
    private Map<String, List<Integer>> mapArrayInteger = new HashMap<String, List<Integer>>();
    private Map<String, List<Object>> mapArrayAnytype = new HashMap<String, List<Object>>();
    private Map<String, Map<String, String>> mapMapString = new HashMap<String, Map<String, String>>();
    private Map<String, Map<String, Object>> mapMapAnytype = new HashMap<String, Map<String, Object>>();
    private Object anytype1;
    private Object anytype2;
    private Object anytype3;

    /**
      **/
    public Builder mapString(Map<String, String> mapString) {
      this.mapString = mapString;
      return this;
    }
    /**
      **/
    public Builder mapNumber(Map<String, BigDecimal> mapNumber) {
      this.mapNumber = mapNumber;
      return this;
    }
    /**
      **/
    public Builder mapInteger(Map<String, Integer> mapInteger) {
      this.mapInteger = mapInteger;
      return this;
    }
    /**
      **/
    public Builder mapBoolean(Map<String, Boolean> mapBoolean) {
      this.mapBoolean = mapBoolean;
      return this;
    }
    /**
      **/
    public Builder mapArrayInteger(Map<String, List<Integer>> mapArrayInteger) {
      this.mapArrayInteger = mapArrayInteger;
      return this;
    }
    /**
      **/
    public Builder mapArrayAnytype(Map<String, List<Object>> mapArrayAnytype) {
      this.mapArrayAnytype = mapArrayAnytype;
      return this;
    }
    /**
      **/
    public Builder mapMapString(Map<String, Map<String, String>> mapMapString) {
      this.mapMapString = mapMapString;
      return this;
    }
    /**
      **/
    public Builder mapMapAnytype(Map<String, Map<String, Object>> mapMapAnytype) {
      this.mapMapAnytype = mapMapAnytype;
      return this;
    }
    /**
      **/
    public Builder anytype1(Object anytype1) {
      this.anytype1 = anytype1;
      return this;
    }
    /**
      **/
    public Builder anytype2(Object anytype2) {
      this.anytype2 = anytype2;
      return this;
    }
    /**
      **/
    public Builder anytype3(Object anytype3) {
      this.anytype3 = anytype3;
      return this;
    }

    public AdditionalPropertiesClass build() {
      return new AdditionalPropertiesClass(mapString, mapNumber, mapInteger, mapBoolean, mapArrayInteger, mapArrayAnytype, mapMapString, mapMapAnytype, anytype1, anytype2, anytype3);
    }
  }
}

