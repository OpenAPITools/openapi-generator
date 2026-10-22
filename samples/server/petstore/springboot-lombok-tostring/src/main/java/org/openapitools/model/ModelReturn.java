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
 * Model for testing reserved words
 */
@lombok.Getter
@lombok.Setter
@lombok.ToString
@lombok.EqualsAndHashCode

@Schema(name = "Return", description = "Model for testing reserved words")
@JsonTypeName("Return")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public class ModelReturn {

  
  @Schema(name = "return", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("return")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable Integer _return;

  public ModelReturn _return(@Nullable Integer _return) {
    this._return = _return;
    return this;
  }



}

