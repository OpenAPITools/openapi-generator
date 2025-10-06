package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
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
 * Describes the result of uploading an image resource
 */
@lombok.Getter
@lombok.Setter
@lombok.ToString
@lombok.EqualsAndHashCode

@Schema(name = "ApiResponse", description = "Describes the result of uploading an image resource")
@JsonTypeName("ApiResponse")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
public class ModelApiResponse {

  private @Nullable Integer code;

  private @Nullable String type;

  private @Nullable String message;

  public ModelApiResponse code(@Nullable Integer code) {
    this.code = code;
    return this;
  }


  public ModelApiResponse type(@Nullable String type) {
    this.type = type;
    return this;
  }


  public ModelApiResponse message(@Nullable String message) {
    this.message = message;
    return this;
  }



}

