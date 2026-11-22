package org.openapitools.model;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.annotation.Generated;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import java.net.URI;
import java.time.OffsetDateTime;
import java.util.*;
import java.util.Objects;
import org.hibernate.validator.constraints.*;
import org.springframework.lang.Nullable;

/**
 * Describes the result of uploading an image resource
 */
@lombok.Getter
@lombok.Setter
@lombok.ToString
@lombok.EqualsAndHashCode

@Schema(name = "ApiResponse", description = "Describes the result of uploading an image resource")
@JsonTypeName("ApiResponse")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.28.0-SNAPSHOT")
public class ModelApiResponse {

  
  @Schema(name = "code", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("code")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable Integer code;

  
  @Schema(name = "type", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("type")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable String type;

  
  @Schema(name = "message", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("message")
  @JsonInclude(JsonInclude.Include.NON_NULL)
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

