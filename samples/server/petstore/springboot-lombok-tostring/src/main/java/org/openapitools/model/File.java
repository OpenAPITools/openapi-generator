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
 * Must be named &#x60;File&#x60; for test.
 */
@lombok.Getter
@lombok.Setter
@lombok.ToString
@lombok.EqualsAndHashCode

@Schema(name = "File", description = "Must be named `File` for test.")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public class File {

  
  @Schema(name = "sourceURI", description = "Test capitalization", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("sourceURI")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable String sourceURI;

  public File sourceURI(@Nullable String sourceURI) {
    this.sourceURI = sourceURI;
    return this;
  }



}

