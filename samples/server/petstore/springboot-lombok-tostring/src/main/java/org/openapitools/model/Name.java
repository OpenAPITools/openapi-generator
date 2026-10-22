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
 * Model for testing model name same as property name
 */
@lombok.Getter
@lombok.Setter
@lombok.ToString
@lombok.EqualsAndHashCode

@Schema(name = "Name", description = "Model for testing model name same as property name")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public class Name {

  @NotNull 
  @Schema(name = "name", requiredMode = Schema.RequiredMode.REQUIRED)
  @JsonProperty("name")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private Integer name;

  
  @Schema(name = "snake_case", accessMode = Schema.AccessMode.READ_ONLY, requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("snake_case")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable Integer snakeCase;

  
  @Schema(name = "property", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("property")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable String property;

  
  @Schema(name = "123Number", accessMode = Schema.AccessMode.READ_ONLY, requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("123Number")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable Integer _123number;

  public Name() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public Name(Integer name) {
    this.name = name;
  }

  public Name name(Integer name) {
    this.name = name;
    return this;
  }


  public Name snakeCase(@Nullable Integer snakeCase) {
    this.snakeCase = snakeCase;
    return this;
  }


  public Name property(@Nullable String property) {
    this.property = property;
    return this;
  }


  public Name _123number(@Nullable Integer _123number) {
    this._123number = _123number;
    return this;
  }



}

