package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
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
 * SpecialModelName
 */
@lombok.Getter
@lombok.Setter
@lombok.ToString
@lombok.EqualsAndHashCode

@JsonTypeName("_special_model.name_")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public class SpecialModelName {

  
  @Schema(name = "$special[property.name]", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("$special[property.name]")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable Long $specialPropertyName;

  public SpecialModelName $specialPropertyName(@Nullable Long $specialPropertyName) {
    this.$specialPropertyName = $specialPropertyName;
    return this;
  }



}

