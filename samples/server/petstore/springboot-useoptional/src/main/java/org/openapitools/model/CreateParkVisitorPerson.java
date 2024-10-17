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
import com.fasterxml.jackson.annotation.JsonTypeName;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * CreateParkVisitorPerson
 */


@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.7.0-SNAPSHOT")
public class CreateParkVisitorPerson extends AbstractCreateParkPerson implements CreateParkPersonRequest {

  public CreateParkVisitorPerson() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public CreateParkVisitorPerson(String type) {
    super(type);
  }


  public CreateParkVisitorPerson type(String type) {
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
    return super.equals(o);
  }

  @Override
  public int hashCode() {
    return Objects.hash(super.hashCode());
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class CreateParkVisitorPerson {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
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

    private CreateParkVisitorPerson instance;

    public Builder() {
      this(new CreateParkVisitorPerson());
    }

    protected Builder(CreateParkVisitorPerson instance) {
      super(instance); // the parent builder shares the same instance
      this.instance = instance;
    }

    protected Builder copyOf(CreateParkVisitorPerson value) { 
      super.copyOf(instance);
      return this;
    }

    @Override
    public CreateParkVisitorPerson.Builder type(String type) {
      this.instance.type(type);
      return this;
    }
    
    /**
    * returns a built CreateParkVisitorPerson instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public CreateParkVisitorPerson build() {
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
  public static CreateParkVisitorPerson.Builder builder() {
    return new CreateParkVisitorPerson.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public CreateParkVisitorPerson.Builder toBuilder() {
    CreateParkVisitorPerson.Builder builder = new CreateParkVisitorPerson.Builder();
    return builder.copyOf(this);
  }

}

