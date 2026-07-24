package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.openapitools.model.PetType;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.*;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.annotation.JsonTypeName;



@JsonTypeName("CAT")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.24.0-SNAPSHOT")
public record CatRequest(
  @JsonProperty(required = true, value = "petType") @NotNull PetType petType,
  @JsonProperty(required = true, value = "name") @NotNull String name,
  @JsonProperty(required = true, value = "indoor") @NotNull Boolean indoor
) implements Serializable, PetRequest {
}

