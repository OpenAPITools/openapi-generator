package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import java.time.OffsetDateTime;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import org.openapitools.model.Animal;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import org.hibernate.validator.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * MixedPropertiesAndAdditionalPropertiesClass
 */
@lombok.Getter
@lombok.Setter
@lombok.ToString
@lombok.EqualsAndHashCode

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public class MixedPropertiesAndAdditionalPropertiesClass {

  @Valid 
  @Schema(name = "uuid", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("uuid")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable UUID uuid;

  @Valid 
  @Schema(name = "dateTime", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("dateTime")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME)
  private @Nullable OffsetDateTime dateTime;

  
  @Schema(name = "map", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("map")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private Map<String, @Valid Animal> map = new HashMap<>();

  public MixedPropertiesAndAdditionalPropertiesClass uuid(@Nullable UUID uuid) {
    this.uuid = uuid;
    return this;
  }


  public MixedPropertiesAndAdditionalPropertiesClass dateTime(@Nullable OffsetDateTime dateTime) {
    this.dateTime = dateTime;
    return this;
  }


  public MixedPropertiesAndAdditionalPropertiesClass map(Map<String, @Valid Animal> map) {
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



}

