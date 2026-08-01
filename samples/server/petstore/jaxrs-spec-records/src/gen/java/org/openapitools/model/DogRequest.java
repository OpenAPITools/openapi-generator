package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.openapitools.model.PetType;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.*;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.annotation.JsonTypeName;



@JsonTypeName("DOG")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.25.0-SNAPSHOT")
public record DogRequest(
  @ApiModelProperty(required = true, value = "")
  @JsonProperty(required = true, value = "petType") @NotNull PetType petType,
  @ApiModelProperty(required = true, value = "")
  @JsonProperty(required = true, value = "name") @NotNull String name,
  @ApiModelProperty(required = true, value = "")
  @JsonProperty(required = true, value = "trained") @NotNull Boolean trained
) implements PetRequest {
}

