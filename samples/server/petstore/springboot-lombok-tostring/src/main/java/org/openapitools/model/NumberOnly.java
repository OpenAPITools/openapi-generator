package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import java.math.BigDecimal;
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
 * NumberOnly
 */
@lombok.Getter
@lombok.Setter
@lombok.ToString
@lombok.EqualsAndHashCode

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public class NumberOnly {

  @Valid 
  @Schema(name = "JustNumber", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("JustNumber")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable BigDecimal justNumber;

  public NumberOnly justNumber(@Nullable BigDecimal justNumber) {
    this.justNumber = justNumber;
    return this;
  }



}

