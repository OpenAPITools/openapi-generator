package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import java.time.OffsetDateTime;
import java.util.HashMap;
import java.util.List;
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
public class MixedPropertiesAndAdditionalPropertiesClass   {

  @JsonProperty("uuid")
  private Optional<UUID> uuid;

  @JsonProperty("dateTime")
  @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME)
  private Optional<OffsetDateTime> dateTime;

  @JsonProperty("map")
  @Valid
  private Map<String, Animal> map = null;

  public MixedPropertiesAndAdditionalPropertiesClass uuid(UUID uuid) {
    this.uuid = Optional.of(uuid);
    return this;
  }

  /**
   * Get uuid
   * @return uuid
  */
  @Schema(name = "uuid", required = false)
  public Optional<UUID> getUuid() {
    return uuid;
  }

  public void setUuid(Optional<UUID> uuid) {
    this.uuid = uuid;
  }

  public MixedPropertiesAndAdditionalPropertiesClass dateTime(OffsetDateTime dateTime) {
    this.dateTime = Optional.of(dateTime);
    return this;
  }

  /**
   * Get dateTime
   * @return dateTime
  */
  @Schema(name = "dateTime", required = false)
  public Optional<OffsetDateTime> getDateTime() {
    return dateTime;
  }

  public void setDateTime(Optional<OffsetDateTime> dateTime) {
    this.dateTime = dateTime;
  }

  public MixedPropertiesAndAdditionalPropertiesClass map(Map<String, Animal> map) {
    this.map = Optional.of(map);
    return this;
  }

  public MixedPropertiesAndAdditionalPropertiesClass putMapItem(String key, Animal mapItem) {
    if (this.map == null) {
      this.map = new HashMap<>();
    }
    this.map.put(key, mapItem);
    return this;
  }

  /**
   * Get map
   * @return map
  */
  @Schema(name = "map", required = false)
  public Optional<Map<String, Animal>> getMap() {
    return map;
  }

  public void setMap(Optional<Map<String, Animal>> map) {
    this.map = map;
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
    return Objects.equals(this.uuid, mixedPropertiesAndAdditionalPropertiesClass.uuid) &&
        Objects.equals(this.dateTime, mixedPropertiesAndAdditionalPropertiesClass.dateTime) &&
        Objects.equals(this.map, mixedPropertiesAndAdditionalPropertiesClass.map);
  }

  @Override
  public int hashCode() {
    return Objects.hash(uuid, dateTime, map);
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
  private String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

