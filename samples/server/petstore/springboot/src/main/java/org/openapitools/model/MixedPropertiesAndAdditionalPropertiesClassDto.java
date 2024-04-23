package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.time.OffsetDateTime;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import org.openapitools.model.AnimalDto;
import org.springframework.format.annotation.DateTimeFormat;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * MixedPropertiesAndAdditionalPropertiesClassDto
 */

@JsonTypeName("MixedPropertiesAndAdditionalPropertiesClass")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.6.0-SNAPSHOT")
public class MixedPropertiesAndAdditionalPropertiesClassDto {

  private UUID uuid;

  @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME)
  private OffsetDateTime dateTime;

  @Valid
  private Map<String, AnimalDto> map = new HashMap<>();

  public MixedPropertiesAndAdditionalPropertiesClassDto uuid(UUID uuid) {
    this.uuid = uuid;
    return this;
  }

  /**
   * Get uuid
   * @return uuid
  */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("uuid")
  public UUID getUuid() {
    return uuid;
  }

  public void setUuid(UUID uuid) {
    this.uuid = uuid;
  }

  public MixedPropertiesAndAdditionalPropertiesClassDto dateTime(OffsetDateTime dateTime) {
    this.dateTime = dateTime;
    return this;
  }

  /**
   * Get dateTime
   * @return dateTime
  */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("dateTime")
  public OffsetDateTime getDateTime() {
    return dateTime;
  }

  public void setDateTime(OffsetDateTime dateTime) {
    this.dateTime = dateTime;
  }

  public MixedPropertiesAndAdditionalPropertiesClassDto map(Map<String, AnimalDto> map) {
    this.map = map;
    return this;
  }

  public MixedPropertiesAndAdditionalPropertiesClassDto putMapItem(String key, AnimalDto mapItem) {
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
  public Map<String, AnimalDto> getMap() {
    return map;
  }

  public void setMap(Map<String, AnimalDto> map) {
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
    MixedPropertiesAndAdditionalPropertiesClassDto mixedPropertiesAndAdditionalPropertiesClass = (MixedPropertiesAndAdditionalPropertiesClassDto) o;
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
    sb.append("class MixedPropertiesAndAdditionalPropertiesClassDto {\n");
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

    private MixedPropertiesAndAdditionalPropertiesClassDto instance;

    public Builder() {
      this(new MixedPropertiesAndAdditionalPropertiesClassDto());
    }

    protected Builder(MixedPropertiesAndAdditionalPropertiesClassDto instance) {
      this.instance = instance;
    }

    protected Builder copyOf(MixedPropertiesAndAdditionalPropertiesClassDto value) { 
      this.instance.setUuid(value.uuid);
      this.instance.setDateTime(value.dateTime);
      this.instance.setMap(value.map);
      return this;
    }

    public MixedPropertiesAndAdditionalPropertiesClassDto.Builder uuid(UUID uuid) {
      this.instance.uuid(uuid);
      return this;
    }
    
    public MixedPropertiesAndAdditionalPropertiesClassDto.Builder dateTime(OffsetDateTime dateTime) {
      this.instance.dateTime(dateTime);
      return this;
    }
    
    public MixedPropertiesAndAdditionalPropertiesClassDto.Builder map(Map<String, AnimalDto> map) {
      this.instance.map(map);
      return this;
    }
    
    /**
    * returns a built MixedPropertiesAndAdditionalPropertiesClassDto instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public MixedPropertiesAndAdditionalPropertiesClassDto build() {
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
  public static MixedPropertiesAndAdditionalPropertiesClassDto.Builder builder() {
    return new MixedPropertiesAndAdditionalPropertiesClassDto.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public MixedPropertiesAndAdditionalPropertiesClassDto.Builder toBuilder() {
    MixedPropertiesAndAdditionalPropertiesClassDto.Builder builder = new MixedPropertiesAndAdditionalPropertiesClassDto.Builder();
    return builder.copyOf(this);
  }

}

