package io.swagger.model;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.validation.constraints.*;

import io.swagger.annotations.ApiModelProperty;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;

public class AdditionalPropertiesClass  {
  
  @ApiModelProperty(value = "")
  private Map<String, String> mapProperty = new HashMap<String, String>();
  @ApiModelProperty(value = "")
  private Map<String, Map<String, String>> mapOfMapProperty = new HashMap<String, Map<String, String>>();

 /**
   * Get mapProperty
   * @return mapProperty
  **/
  public Map<String, String> getMapProperty() {
    return mapProperty;
  }

  public void setMapProperty(Map<String, String> mapProperty) {
    this.mapProperty = mapProperty;
  }

  public AdditionalPropertiesClass mapProperty(Map<String, String> mapProperty) {
    this.mapProperty = mapProperty;
    return this;
  }

  public AdditionalPropertiesClass putMapPropertyItem(String key, String mapPropertyItem) {
    this.mapProperty.put(key, mapPropertyItem);
    return this;
  }

 /**
   * Get mapOfMapProperty
   * @return mapOfMapProperty
  **/
  public Map<String, Map<String, String>> getMapOfMapProperty() {
    return mapOfMapProperty;
  }

  public void setMapOfMapProperty(Map<String, Map<String, String>> mapOfMapProperty) {
    this.mapOfMapProperty = mapOfMapProperty;
  }

  public AdditionalPropertiesClass mapOfMapProperty(Map<String, Map<String, String>> mapOfMapProperty) {
    this.mapOfMapProperty = mapOfMapProperty;
    return this;
  }

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

