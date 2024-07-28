package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
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


import java.util.*;
import javax.annotation.Generated;

/**
 * MixedPropertiesAndAdditionalPropertiesClass
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 8.0.0-SNAPSHOT")
public class MixedPropertiesAndAdditionalPropertiesClass {

  private Optional<UUID> _uuid = Optional.empty();

  @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME)
  private Optional<OffsetDateTime> _dateTime = Optional.empty();

  @Valid
  private Map<String, Animal> _map = new HashMap<>();

  public MixedPropertiesAndAdditionalPropertiesClass _uuid(UUID _uuid) {
    this._uuid = Optional.of(_uuid);
    return this;
  }

  /**
   * Get _uuid
   * @return _uuid
   */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("uuid")
  public Optional<UUID> getUuid() {
    return _uuid;
  }

  public void setUuid(Optional<UUID> _uuid) {
    this._uuid = _uuid;
  }

  public MixedPropertiesAndAdditionalPropertiesClass _dateTime(OffsetDateTime _dateTime) {
    this._dateTime = Optional.of(_dateTime);
    return this;
  }

  /**
   * Get _dateTime
   * @return _dateTime
   */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("dateTime")
  public Optional<OffsetDateTime> getDateTime() {
    return _dateTime;
  }

  public void setDateTime(Optional<OffsetDateTime> _dateTime) {
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
  @ApiModelProperty(value = "")
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
  
  public static class Builder {

    private MixedPropertiesAndAdditionalPropertiesClass instance;

    public Builder() {
      this(new MixedPropertiesAndAdditionalPropertiesClass());
    }

    protected Builder(MixedPropertiesAndAdditionalPropertiesClass instance) {
      this.instance = instance;
    }

    protected Builder copyOf(MixedPropertiesAndAdditionalPropertiesClass value) { 
      this.instance.setUuid(value._uuid);
      this.instance.setDateTime(value._dateTime);
      this.instance.setMap(value._map);
      return this;
    }

    public MixedPropertiesAndAdditionalPropertiesClass.Builder _uuid(UUID _uuid) {
      this.instance._uuid(_uuid);
      return this;
    }
    
    public MixedPropertiesAndAdditionalPropertiesClass.Builder _dateTime(OffsetDateTime _dateTime) {
      this.instance._dateTime(_dateTime);
      return this;
    }
    
    public MixedPropertiesAndAdditionalPropertiesClass.Builder _map(Map<String, Animal> _map) {
      this.instance._map(_map);
      return this;
    }
    
    /**
    * returns a built MixedPropertiesAndAdditionalPropertiesClass instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public MixedPropertiesAndAdditionalPropertiesClass build() {
      try {
        return this.instance;
      } finally {
        // ensure that this.instance is not reused
        this.instance = null;
      }
    }

    @Override
    public String toString() {
      return getClass() + "=(" + instance + ")";
    }
  }

  /**
  * Create a builder with no initialized field (except for the default values).
  */
  public static MixedPropertiesAndAdditionalPropertiesClass.Builder builder() {
    return new MixedPropertiesAndAdditionalPropertiesClass.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public MixedPropertiesAndAdditionalPropertiesClass.Builder toBuilder() {
    MixedPropertiesAndAdditionalPropertiesClass.Builder builder = new MixedPropertiesAndAdditionalPropertiesClass.Builder();
    return builder.copyOf(this);
  }

}

