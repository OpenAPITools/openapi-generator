package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import com.fasterxml.jackson.annotation.JsonTypeName;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * Describes the result of uploading an image resource
 */
@lombok.Data
@lombok.Builder
@lombok.NoArgsConstructor
@lombok.AllArgsConstructor

@Schema(name = "ApiResponse", description = "Describes the result of uploading an image resource")
@JsonPropertyOrder({
    ModelApiResponse.JSON_PROPERTY_CODE,
    ModelApiResponse.JSON_PROPERTY_TYPE,
    ModelApiResponse.JSON_PROPERTY_MESSAGE
})
@JsonTypeName("ApiResponse")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.23.0-SNAPSHOT")
public class ModelApiResponse {

    public static final String JSON_PROPERTY_CODE = "code";
  
  @Schema(name = "code", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("code")

  private @Nullable Integer code;

    public static final String JSON_PROPERTY_TYPE = "type";
  
  @Schema(name = "type", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("type")

  private @Nullable String type;

    public static final String JSON_PROPERTY_MESSAGE = "message";
  
  @Schema(name = "message", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("message")

  private @Nullable String message;

}

