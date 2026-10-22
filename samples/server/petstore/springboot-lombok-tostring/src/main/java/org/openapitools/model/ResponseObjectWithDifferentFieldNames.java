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
 * ResponseObjectWithDifferentFieldNames
 */
@lombok.Getter
@lombok.Setter
@lombok.ToString
@lombok.EqualsAndHashCode

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public class ResponseObjectWithDifferentFieldNames {

  
  @Schema(name = "normalPropertyName", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("normalPropertyName")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable String normalPropertyName;

  
  @Schema(name = "UPPER_CASE_PROPERTY_SNAKE", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("UPPER_CASE_PROPERTY_SNAKE")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable String UPPER_CASE_PROPERTY_SNAKE;

  
  @Schema(name = "lower-case-property-dashes", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("lower-case-property-dashes")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable String lowerCasePropertyDashes;

  
  @Schema(name = "property name with spaces", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("property name with spaces")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable String propertyNameWithSpaces;

  public ResponseObjectWithDifferentFieldNames normalPropertyName(@Nullable String normalPropertyName) {
    this.normalPropertyName = normalPropertyName;
    return this;
  }


  public ResponseObjectWithDifferentFieldNames UPPER_CASE_PROPERTY_SNAKE(@Nullable String UPPER_CASE_PROPERTY_SNAKE) {
    this.UPPER_CASE_PROPERTY_SNAKE = UPPER_CASE_PROPERTY_SNAKE;
    return this;
  }


  public ResponseObjectWithDifferentFieldNames lowerCasePropertyDashes(@Nullable String lowerCasePropertyDashes) {
    this.lowerCasePropertyDashes = lowerCasePropertyDashes;
    return this;
  }


  public ResponseObjectWithDifferentFieldNames propertyNameWithSpaces(@Nullable String propertyNameWithSpaces) {
    this.propertyNameWithSpaces = propertyNameWithSpaces;
    return this;
  }



}

