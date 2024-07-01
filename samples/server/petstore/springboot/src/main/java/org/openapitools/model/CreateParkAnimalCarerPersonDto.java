package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.openapitools.model.AbstractCreateParkPersonDto;
import org.openapitools.model.CreateParkAnimalCarerPersonResponsibleForDtoOneOfDtoDto;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * CreateParkAnimalCarerPersonDto
 */


@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.7.0-SNAPSHOT")
public class CreateParkAnimalCarerPersonDto extends AbstractCreateParkPersonDto implements CreateParkPersonRequestDto {

  private JsonNullable<CreateParkAnimalCarerPersonResponsibleForDtoOneOfDtoDto> responsibleFor = JsonNullable.<CreateParkAnimalCarerPersonResponsibleForDtoOneOfDtoDto>undefined();

  public CreateParkAnimalCarerPersonDto() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public CreateParkAnimalCarerPersonDto(CreateParkAnimalCarerPersonResponsibleForDtoOneOfDtoDto responsibleFor, String type) {
    super(type);
    this.responsibleFor = JsonNullable.of(responsibleFor);
  }

  public CreateParkAnimalCarerPersonDto responsibleFor(CreateParkAnimalCarerPersonResponsibleForDtoOneOfDtoDto responsibleFor) {
    this.responsibleFor = JsonNullable.of(responsibleFor);
    return this;
  }

  /**
   * Get responsibleFor
   * @return responsibleFor
   */
  @NotNull @Valid 
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("responsibleFor")
  public JsonNullable<CreateParkAnimalCarerPersonResponsibleForDtoOneOfDtoDto> getResponsibleFor() {
    return responsibleFor;
  }

  public void setResponsibleFor(JsonNullable<CreateParkAnimalCarerPersonResponsibleForDtoOneOfDtoDto> responsibleFor) {
    this.responsibleFor = responsibleFor;
  }


  public CreateParkAnimalCarerPersonDto type(String type) {
    super.type(type);
    return this;
  }
  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    CreateParkAnimalCarerPersonDto createParkAnimalCarerPerson = (CreateParkAnimalCarerPersonDto) o;
    return Objects.equals(this.responsibleFor, createParkAnimalCarerPerson.responsibleFor) &&
        super.equals(o);
  }

  @Override
  public int hashCode() {
    return Objects.hash(responsibleFor, super.hashCode());
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class CreateParkAnimalCarerPersonDto {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    responsibleFor: ").append(toIndentedString(responsibleFor)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

