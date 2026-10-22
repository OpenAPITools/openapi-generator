package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
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
 * ReadOnlyFirst
 */
@lombok.Getter
@lombok.Setter
@lombok.ToString
@lombok.EqualsAndHashCode

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public class ReadOnlyFirst {

  
  @Schema(name = "bar", accessMode = Schema.AccessMode.READ_ONLY, requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("bar")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable String bar;

  
  @Schema(name = "baz", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("baz")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable String baz;

  public ReadOnlyFirst bar(@Nullable String bar) {
    this.bar = bar;
    return this;
  }


  public ReadOnlyFirst baz(@Nullable String baz) {
    this.baz = baz;
    return this;
  }



}

