package io.swagger.model;

import io.swagger.model.Animal;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import javax.validation.constraints.*;

import io.swagger.annotations.ApiModelProperty;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;

public class MixedPropertiesAndAdditionalPropertiesClass  {
  
  @ApiModelProperty(example = "null", value = "")
  private UUID uuid = null;
  @ApiModelProperty(example = "null", value = "")
  private javax.xml.datatype.XMLGregorianCalendar dateTime = null;
  @ApiModelProperty(example = "null", value = "")
  private Map<String, Animal> map = new HashMap<String, Animal>();

 /**
   * Get uuid
   * @return uuid
  **/
  public UUID getUuid() {
    return uuid;
  }

  public void setUuid(UUID uuid) {
    this.uuid = uuid;
  }

  public MixedPropertiesAndAdditionalPropertiesClass uuid(UUID uuid) {
    this.uuid = uuid;
    return this;
  }

 /**
   * Get dateTime
   * @return dateTime
  **/
  public javax.xml.datatype.XMLGregorianCalendar getDateTime() {
    return dateTime;
  }

  public void setDateTime(javax.xml.datatype.XMLGregorianCalendar dateTime) {
    this.dateTime = dateTime;
  }

  public MixedPropertiesAndAdditionalPropertiesClass dateTime(javax.xml.datatype.XMLGregorianCalendar dateTime) {
    this.dateTime = dateTime;
    return this;
  }

 /**
   * Get map
   * @return map
  **/
  public Map<String, Animal> getMap() {
    return map;
  }

  public void setMap(Map<String, Animal> map) {
    this.map = map;
  }

  public MixedPropertiesAndAdditionalPropertiesClass map(Map<String, Animal> map) {
    this.map = map;
    return this;
  }

  public MixedPropertiesAndAdditionalPropertiesClass putMapItem(String key, Animal mapItem) {
    this.map.put(key, mapItem);
    return this;
  }


  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class MixedPropertiesAndAdditionalPropertiesClass {\n");
    
    sb.append("    uuid: ").append(toIndentedString(uuid)).append("\n");
    sb.append("    dateTime: ").append(toIndentedString(dateTime)).append("\n");
    sb.append("    map: ").append(toIndentedString(map)).append("\n");
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

