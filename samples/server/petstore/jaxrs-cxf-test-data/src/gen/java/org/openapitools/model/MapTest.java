package org.openapitools.model;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.openapitools.model.StringBooleanMap;
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


public class MapTest  {
  
  @ApiModelProperty(value = "")
  @Valid
  private Map<String, Map<String, String>> mapMapOfString = null;

@XmlType(name="InnerEnum")
@XmlEnum(String.class)
public enum InnerEnum {

    @XmlEnumValue("UPPER") @JsonProperty("UPPER") UPPER(String.valueOf("UPPER")), 
    @XmlEnumValue("lower") @JsonProperty("lower") LOWER(String.valueOf("lower"));

    private String value;

    InnerEnum (String v) {
        value = v;
    }

    public String value() {
        return value;
    }

    @Override
    public String toString() {
        return String.valueOf(value);
    }

    public static InnerEnum fromValue(String value) {
        for (InnerEnum b : InnerEnum.values()) {
            if (b.value.equals(value)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected value '" + value + "'");
    }
}

  @ApiModelProperty(value = "")
  private Map<String, InnerEnum> mapOfEnumString = null;

  @ApiModelProperty(value = "")
  private Map<String, Boolean> directMap = null;

  @ApiModelProperty(value = "")
  @Valid
  private StringBooleanMap indirectMap = new StringBooleanMap();
 /**
  * Get mapMapOfString
  * @return mapMapOfString
  */
  @JsonProperty("map_map_of_string")
  public Map<String, Map<String, String>> getMapMapOfString() {
    return mapMapOfString;
  }

  /**
   * Sets the <code>mapMapOfString</code> property.
   */
  public void setMapMapOfString(Map<String, Map<String, String>> mapMapOfString) {
    this.mapMapOfString = mapMapOfString;
  }

  /**
   * Sets the <code>mapMapOfString</code> property.
   */
  public MapTest mapMapOfString(Map<String, Map<String, String>> mapMapOfString) {
    this.mapMapOfString = mapMapOfString;
    return this;
  }

  /**
   * Puts a new item into the <code>mapMapOfString</code> map.
   */
  public MapTest putMapMapOfStringItem(String key, Map<String, String> mapMapOfStringItem) {
    this.mapMapOfString.put(key, mapMapOfStringItem);
    return this;
  }

 /**
  * Get mapOfEnumString
  * @return mapOfEnumString
  */
  @JsonProperty("map_of_enum_string")
  public Map<String, InnerEnum> getMapOfEnumString() {
    return mapOfEnumString;
  }

  /**
   * Sets the <code>mapOfEnumString</code> property.
   */
  public void setMapOfEnumString(Map<String, InnerEnum> mapOfEnumString) {
    this.mapOfEnumString = mapOfEnumString;
  }

  /**
   * Sets the <code>mapOfEnumString</code> property.
   */
  public MapTest mapOfEnumString(Map<String, InnerEnum> mapOfEnumString) {
    this.mapOfEnumString = mapOfEnumString;
    return this;
  }

  /**
   * Puts a new item into the <code>mapOfEnumString</code> map.
   */
  public MapTest putMapOfEnumStringItem(String key, InnerEnum mapOfEnumStringItem) {
    this.mapOfEnumString.put(key, mapOfEnumStringItem);
    return this;
  }

 /**
  * Get directMap
  * @return directMap
  */
  @JsonProperty("direct_map")
  public Map<String, Boolean> getDirectMap() {
    return directMap;
  }

  /**
   * Sets the <code>directMap</code> property.
   */
  public void setDirectMap(Map<String, Boolean> directMap) {
    this.directMap = directMap;
  }

  /**
   * Sets the <code>directMap</code> property.
   */
  public MapTest directMap(Map<String, Boolean> directMap) {
    this.directMap = directMap;
    return this;
  }

  /**
   * Puts a new item into the <code>directMap</code> map.
   */
  public MapTest putDirectMapItem(String key, Boolean directMapItem) {
    this.directMap.put(key, directMapItem);
    return this;
  }

 /**
  * Get indirectMap
  * @return indirectMap
  */
  @JsonProperty("indirect_map")
  public StringBooleanMap getIndirectMap() {
    return indirectMap;
  }

  /**
   * Sets the <code>indirectMap</code> property.
   */
  public void setIndirectMap(StringBooleanMap indirectMap) {
    this.indirectMap = indirectMap;
  }

  /**
   * Sets the <code>indirectMap</code> property.
   */
  public MapTest indirectMap(StringBooleanMap indirectMap) {
    this.indirectMap = indirectMap;
    return this;
  }


  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class MapTest {\n");
    
    sb.append("    mapMapOfString: ").append(toIndentedString(mapMapOfString)).append("\n");
    sb.append("    mapOfEnumString: ").append(toIndentedString(mapOfEnumString)).append("\n");
    sb.append("    directMap: ").append(toIndentedString(directMap)).append("\n");
    sb.append("    indirectMap: ").append(toIndentedString(indirectMap)).append("\n");
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

