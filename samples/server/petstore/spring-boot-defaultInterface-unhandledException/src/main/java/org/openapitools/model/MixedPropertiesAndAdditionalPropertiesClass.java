package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import java.time.OffsetDateTime;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import org.openapitools.model.Animal;
import org.springframework.format.annotation.DateTimeFormat;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import javax.annotation.Generated;

/**
 * MixedPropertiesAndAdditionalPropertiesClass
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class MixedPropertiesAndAdditionalPropertiesClass {

  private UUID _uuid;

  @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME)
  private OffsetDateTime _dateTime;

  @Valid
  private Map<String, Animal> _map = new HashMap<>();

  public MixedPropertiesAndAdditionalPropertiesClass _uuid(UUID _uuid) {
    this._uuid = _uuid;
    return this;
  }

  /**
   * Get _uuid
   * @return _uuid
  */
  @Valid 
  @Schema(name = "uuid", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("uuid")
  public UUID getUuid() {
    return _uuid;
  }

  public void setUuid(UUID _uuid) {
    this._uuid = _uuid;
  }

  public MixedPropertiesAndAdditionalPropertiesClass _dateTime(OffsetDateTime _dateTime) {
    this._dateTime = _dateTime;
    return this;
  }

  /**
   * Get _dateTime
   * @return _dateTime
  */
  @Valid 
  @Schema(name = "dateTime", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("dateTime")
  public OffsetDateTime getDateTime() {
    return _dateTime;
  }

  public void setDateTime(OffsetDateTime _dateTime) {
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
  */
  @Valid 
  @Schema(name = "map", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("map")
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

