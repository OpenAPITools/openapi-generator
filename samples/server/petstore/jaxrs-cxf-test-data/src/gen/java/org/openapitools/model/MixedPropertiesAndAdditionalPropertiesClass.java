package org.openapitools.model;

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import org.openapitools.model.Animal;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.ApiModelProperty;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;


public class MixedPropertiesAndAdditionalPropertiesClass  {
  
  @ApiModelProperty(value = "")
  private UUID uuid;

  @ApiModelProperty(value = "")
  @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'hh:mm:ss.SSSX")
  private Date dateTime;

  @ApiModelProperty(value = "")
  @Valid
  private Map<String, Animal> map = null;
 /**
  * Get uuid
  * @return uuid
  */
  @JsonProperty("uuid")
  public UUID getUuid() {
    return uuid;
  }

  /**
   * Sets the <code>uuid</code> property.
   */
 public void setUuid(UUID uuid) {
    this.uuid = uuid;
  }

  /**
   * Sets the <code>uuid</code> property.
   */
  public MixedPropertiesAndAdditionalPropertiesClass uuid(UUID uuid) {
    this.uuid = uuid;
    return this;
  }

 /**
  * Get dateTime
  * @return dateTime
  */
  @JsonProperty("dateTime")
  public Date getDateTime() {
    return dateTime;
  }

  /**
   * Sets the <code>dateTime</code> property.
   */
 public void setDateTime(Date dateTime) {
    this.dateTime = dateTime;
  }

  /**
   * Sets the <code>dateTime</code> property.
   */
  public MixedPropertiesAndAdditionalPropertiesClass dateTime(Date dateTime) {
    this.dateTime = dateTime;
    return this;
  }

 /**
  * Get map
  * @return map
  */
  @JsonProperty("map")
  public Map<String, Animal> getMap() {
    return map;
  }

  /**
   * Sets the <code>map</code> property.
   */
 public void setMap(Map<String, Animal> map) {
    this.map = map;
  }

  /**
   * Sets the <code>map</code> property.
   */
  public MixedPropertiesAndAdditionalPropertiesClass map(Map<String, Animal> map) {
    this.map = map;
    return this;
  }

  /**
   * Puts a new item into the <code>map</code> map.
   */
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
  private static String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

