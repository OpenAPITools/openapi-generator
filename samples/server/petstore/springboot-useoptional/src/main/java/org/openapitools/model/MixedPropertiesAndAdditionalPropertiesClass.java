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
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * MixedPropertiesAndAdditionalPropertiesClass
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
public class MixedPropertiesAndAdditionalPropertiesClass {

  private Optional<UUID> uuid = Optional.empty();

  @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME)
  private Optional<OffsetDateTime> dateTime = Optional.empty();

  @Valid
  private Map<String, Animal> map = new HashMap<>();

  public MixedPropertiesAndAdditionalPropertiesClass uuid(UUID uuid) {
    this.uuid = Optional.ofNullable(uuid);
    return this;
  }

  /**
   * Get uuid
   * @return uuid
   */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("uuid")
  public Optional<UUID> getUuid() {
    return uuid;
  }

  public void setUuid(Optional<UUID> uuid) {
    this.uuid = uuid;
  }

  public MixedPropertiesAndAdditionalPropertiesClass dateTime(OffsetDateTime dateTime) {
    this.dateTime = Optional.ofNullable(dateTime);
    return this;
  }

  /**
   * Get dateTime
   * @return dateTime
   */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("dateTime")
  public Optional<OffsetDateTime> getDateTime() {
    return dateTime;
  }

  public void setDateTime(Optional<OffsetDateTime> dateTime) {
    this.dateTime = dateTime;
  }

  public MixedPropertiesAndAdditionalPropertiesClass map(Map<String, Animal> map) {
    this.map = map;
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
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("map")
  public Map<String, Animal> getMap() {
    return map;
  }

  public void setMap(Map<String, Animal> map) {
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
  
  public static class Builder {

    private MixedPropertiesAndAdditionalPropertiesClass instance;

    public Builder() {
      this(new MixedPropertiesAndAdditionalPropertiesClass());
    }

    protected Builder(MixedPropertiesAndAdditionalPropertiesClass instance) {
      this.instance = instance;
    }

    protected Builder copyOf(MixedPropertiesAndAdditionalPropertiesClass value) { 
      this.instance.setUuid(value.uuid);
      this.instance.setDateTime(value.dateTime);
      this.instance.setMap(value.map);
      return this;
    }

    public MixedPropertiesAndAdditionalPropertiesClass.Builder uuid(UUID uuid) {
      this.instance.uuid(uuid);
      return this;
    }
    
    public MixedPropertiesAndAdditionalPropertiesClass.Builder dateTime(OffsetDateTime dateTime) {
      this.instance.dateTime(dateTime);
      return this;
    }
    
    public MixedPropertiesAndAdditionalPropertiesClass.Builder map(Map<String, Animal> map) {
      this.instance.map(map);
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

