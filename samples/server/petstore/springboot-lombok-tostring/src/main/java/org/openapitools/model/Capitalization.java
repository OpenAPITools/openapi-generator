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
 * Capitalization
 */
@lombok.Getter
@lombok.Setter
@lombok.ToString
@lombok.EqualsAndHashCode

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public class Capitalization {

  
  @Schema(name = "smallCamel", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("smallCamel")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable String smallCamel;

  
  @Schema(name = "CapitalCamel", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("CapitalCamel")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable String capitalCamel;

  
  @Schema(name = "small_Snake", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("small_Snake")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable String smallSnake;

  
  @Schema(name = "Capital_Snake", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("Capital_Snake")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable String capitalSnake;

  
  @Schema(name = "SCA_ETH_Flow_Points", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("SCA_ETH_Flow_Points")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable String scAETHFlowPoints;

  
  @Schema(name = "ATT_NAME", description = "Name of the pet ", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("ATT_NAME")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable String ATT_NAME;

  public Capitalization smallCamel(@Nullable String smallCamel) {
    this.smallCamel = smallCamel;
    return this;
  }


  public Capitalization capitalCamel(@Nullable String capitalCamel) {
    this.capitalCamel = capitalCamel;
    return this;
  }


  public Capitalization smallSnake(@Nullable String smallSnake) {
    this.smallSnake = smallSnake;
    return this;
  }


  public Capitalization capitalSnake(@Nullable String capitalSnake) {
    this.capitalSnake = capitalSnake;
    return this;
  }


  public Capitalization scAETHFlowPoints(@Nullable String scAETHFlowPoints) {
    this.scAETHFlowPoints = scAETHFlowPoints;
    return this;
  }


  public Capitalization ATT_NAME(@Nullable String ATT_NAME) {
    this.ATT_NAME = ATT_NAME;
    return this;
  }



}

