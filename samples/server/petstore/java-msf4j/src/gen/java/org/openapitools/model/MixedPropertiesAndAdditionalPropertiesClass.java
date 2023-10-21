package org.openapitools.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import org.openapitools.model.Animal;

/**
 * MixedPropertiesAndAdditionalPropertiesClass
 */
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaMSF4JServerCodegen")
public class MixedPropertiesAndAdditionalPropertiesClass   {
  @JsonProperty("uuid")
  private UUID _uuid;

  @JsonProperty("dateTime")
  private Date _dateTime;

  @JsonProperty("map")
  private Map<String, Animal> _map = null;

  public MixedPropertiesAndAdditionalPropertiesClass _uuid(UUID _uuid) {
    this._uuid = _uuid;
    return this;
  }

   /**
   * Get _uuid
   * @return _uuid
  **/
  @ApiModelProperty(value = "")
  public UUID getUuid() {
    return _uuid;
  }

  public void setUuid(UUID _uuid) {
    this._uuid = _uuid;
  }

  public MixedPropertiesAndAdditionalPropertiesClass _dateTime(Date _dateTime) {
    this._dateTime = _dateTime;
    return this;
  }

   /**
   * Get _dateTime
   * @return _dateTime
  **/
  @ApiModelProperty(value = "")
  public Date getDateTime() {
    return _dateTime;
  }

  public void setDateTime(Date _dateTime) {
    this._dateTime = _dateTime;
  }

  public MixedPropertiesAndAdditionalPropertiesClass _map(Map<String, Animal> _map) {
    this._map = _map;
    return this;
  }

  public MixedPropertiesAndAdditionalPropertiesClass putMapItem(String key, Animal _mapItem) {
    if (this._map == null) {
      this._map = new HashMap<>();
    }
    this._map.put(key, _mapItem);
    return this;
  }

   /**
   * Get _map
   * @return _map
  **/
  @ApiModelProperty(value = "")
  public Map<String, Animal> getMap() {
    return _map;
  }

  public void setMap(Map<String, Animal> _map) {
    this._map = _map;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    MixedPropertiesAndAdditionalPropertiesClass mixedPropertiesAndAdditionalPropertiesClass = (MixedPropertiesAndAdditionalPropertiesClass) o;
    return Objects.equals(this._uuid, mixedPropertiesAndAdditionalPropertiesClass._uuid) &&
        Objects.equals(this._dateTime, mixedPropertiesAndAdditionalPropertiesClass._dateTime) &&
        Objects.equals(this._map, mixedPropertiesAndAdditionalPropertiesClass._map);
  }

  @Override
  public int hashCode() {
    return Objects.hash(_uuid, _dateTime, _map);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class MixedPropertiesAndAdditionalPropertiesClass {\n");
    
    sb.append("    _uuid: ").append(toIndentedString(_uuid)).append("\n");
    sb.append("    _dateTime: ").append(toIndentedString(_dateTime)).append("\n");
    sb.append("    _map: ").append(toIndentedString(_map)).append("\n");
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

