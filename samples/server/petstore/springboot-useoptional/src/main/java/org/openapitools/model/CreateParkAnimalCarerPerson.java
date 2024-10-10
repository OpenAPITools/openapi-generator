package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.openapitools.model.AbstractCreateParkPerson;
import org.openapitools.model.CreateParkAnimalCarerPersonResponsibleForOneOf;
import com.fasterxml.jackson.annotation.JsonTypeName;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * CreateParkAnimalCarerPerson
 */


@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.7.0-SNAPSHOT")
public class CreateParkAnimalCarerPerson extends AbstractCreateParkPerson implements CreateParkPersonRequest {

  private JsonNullable<CreateParkAnimalCarerPersonResponsibleForOneOf> responsibleFor = JsonNullable.<CreateParkAnimalCarerPersonResponsibleForOneOf>undefined();

  public CreateParkAnimalCarerPerson() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public CreateParkAnimalCarerPerson(CreateParkAnimalCarerPersonResponsibleForOneOf responsibleFor, String type) {
    super(type);
    this.responsibleFor = JsonNullable.of(responsibleFor);
  }

  public CreateParkAnimalCarerPerson responsibleFor(CreateParkAnimalCarerPersonResponsibleForOneOf responsibleFor) {
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
  public JsonNullable<CreateParkAnimalCarerPersonResponsibleForOneOf> getResponsibleFor() {
    return responsibleFor;
  }

  public void setResponsibleFor(JsonNullable<CreateParkAnimalCarerPersonResponsibleForOneOf> responsibleFor) {
    this.responsibleFor = responsibleFor;
  }


  public CreateParkAnimalCarerPerson type(String type) {
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
    CreateParkAnimalCarerPerson createParkAnimalCarerPerson = (CreateParkAnimalCarerPerson) o;
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
    sb.append("class CreateParkAnimalCarerPerson {\n");
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
  
  public static class Builder extends AbstractCreateParkPerson.Builder {

    private CreateParkAnimalCarerPerson instance;

    public Builder() {
      this(new CreateParkAnimalCarerPerson());
    }

    protected Builder(CreateParkAnimalCarerPerson instance) {
      super(instance); // the parent builder shares the same instance
      this.instance = instance;
    }

    protected Builder copyOf(CreateParkAnimalCarerPerson value) { 
      super.copyOf(instance);
      this.instance.setResponsibleFor(value.responsibleFor);
      return this;
    }

    public CreateParkAnimalCarerPerson.Builder responsibleFor(CreateParkAnimalCarerPersonResponsibleForOneOf responsibleFor) {
      this.instance.responsibleFor(responsibleFor);
      return this;
    }
    
    public CreateParkAnimalCarerPerson.Builder responsibleFor(JsonNullable<CreateParkAnimalCarerPersonResponsibleForOneOf> responsibleFor) {
      this.instance.responsibleFor = responsibleFor;
      return this;
    }
    
    @Override
    public CreateParkAnimalCarerPerson.Builder type(String type) {
      this.instance.type(type);
      return this;
    }
    
    /**
    * returns a built CreateParkAnimalCarerPerson instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public CreateParkAnimalCarerPerson build() {
      try {
        return this.instance;
      } finally {
        // ensure that this.instance is not reused
        super.build();
        this.instance = null;
      }
    }

    @Override
    public String toString() {
      return getClass() + "=(" + instance + ")";
    }
  }

  /**
  * Create a builder with no initialized field (except for the default values).
  */
  public static CreateParkAnimalCarerPerson.Builder builder() {
    return new CreateParkAnimalCarerPerson.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public CreateParkAnimalCarerPerson.Builder toBuilder() {
    CreateParkAnimalCarerPerson.Builder builder = new CreateParkAnimalCarerPerson.Builder();
    return builder.copyOf(this);
  }

}

