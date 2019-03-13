package org.openapitools.model;

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
  private Map<String, String> mapProperty = null;

  @ApiModelProperty(value = "")
  @Valid
  private Map<String, Map<String, String>> mapOfMapProperty = null;
 /**
  * Get mapProperty
  * @return mapProperty
  */
  @JsonProperty("map_property")
  public Map<String, String> getMapProperty() {
    return mapProperty;
  }

  /**
   * Sets the <code>mapProperty</code> property.
   */
  public void setMapProperty(Map<String, String> mapProperty) {
    this.mapProperty = mapProperty;
  }

  /**
   * Sets the <code>mapProperty</code> property.
   */
  public AdditionalPropertiesClass mapProperty(Map<String, String> mapProperty) {
    this.mapProperty = mapProperty;
    return this;
  }

  /**
   * Puts a new item into the <code>mapProperty</code> map.
   */
  public AdditionalPropertiesClass putMapPropertyItem(String key, String mapPropertyItem) {
    this.mapProperty.put(key, mapPropertyItem);
    return this;
  }

 /**
  * Get mapOfMapProperty
  * @return mapOfMapProperty
  */
  @JsonProperty("map_of_map_property")
  public Map<String, Map<String, String>> getMapOfMapProperty() {
    return mapOfMapProperty;
  }

  /**
   * Sets the <code>mapOfMapProperty</code> property.
   */
  public void setMapOfMapProperty(Map<String, Map<String, String>> mapOfMapProperty) {
    this.mapOfMapProperty = mapOfMapProperty;
  }

  /**
   * Sets the <code>mapOfMapProperty</code> property.
   */
  public AdditionalPropertiesClass mapOfMapProperty(Map<String, Map<String, String>> mapOfMapProperty) {
    this.mapOfMapProperty = mapOfMapProperty;
    return this;
  }

  /**
   * Puts a new item into the <code>mapOfMapProperty</code> map.
   */
  public AdditionalPropertiesClass putMapOfMapPropertyItem(String key, Map<String, String> mapOfMapPropertyItem) {
    this.mapOfMapProperty.put(key, mapOfMapPropertyItem);
    return this;
  }


  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class AdditionalPropertiesClass {\n");
    
    sb.append("    mapProperty: ").append(toIndentedString(mapProperty)).append("\n");
    sb.append("    mapOfMapProperty: ").append(toIndentedString(mapOfMapProperty)).append("\n");
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

