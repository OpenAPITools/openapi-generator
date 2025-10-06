package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * Model for testing model name starting with number
 */

@ApiModel(description = "Model for testing model name starting with number")
@JsonTypeName("200_response")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
public class Model200Response {

  private Optional<Integer> name = Optional.empty();

  private Optional<String> propertyClass = Optional.empty();

  public Model200Response name(Integer name) {
    this.name = Optional.ofNullable(name);
    return this;
  }

  /**
   * Get name
   * @return name
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("name")
  public Optional<Integer> getName() {
    return name;
  }

  public void setName(Optional<Integer> name) {
    this.name = name;
  }

  public Model200Response propertyClass(String propertyClass) {
    this.propertyClass = Optional.ofNullable(propertyClass);
    return this;
  }

  /**
   * Get propertyClass
   * @return propertyClass
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("class")
  public Optional<String> getPropertyClass() {
    return propertyClass;
  }

  public void setPropertyClass(Optional<String> propertyClass) {
    this.propertyClass = propertyClass;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Model200Response _200response = (Model200Response) o;
    return Objects.equals(this.name, _200response.name) &&
        Objects.equals(this.propertyClass, _200response.propertyClass);
  }

  @Override
  public int hashCode() {
    return Objects.hash(name, propertyClass);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Model200Response {\n");
    sb.append("    name: ").append(toIndentedString(name)).append("\n");
    sb.append("    propertyClass: ").append(toIndentedString(propertyClass)).append("\n");
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
  
  public static class Builder {

    private Model200Response instance;

    public Builder() {
      this(new Model200Response());
    }

    protected Builder(Model200Response instance) {
      this.instance = instance;
    }

    protected Builder copyOf(Model200Response value) { 
      this.instance.setName(value.name);
      this.instance.setPropertyClass(value.propertyClass);
      return this;
    }

    public Model200Response.Builder name(Integer name) {
      this.instance.name(name);
      return this;
    }
    
    public Model200Response.Builder propertyClass(String propertyClass) {
      this.instance.propertyClass(propertyClass);
      return this;
    }
    
    /**
    * returns a built Model200Response instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public Model200Response build() {
      try {
        return this.instance;
      } finally {
        // ensure that this.instance is not reused
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
  public static Model200Response.Builder builder() {
    return new Model200Response.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public Model200Response.Builder toBuilder() {
    Model200Response.Builder builder = new Model200Response.Builder();
    return builder.copyOf(this);
  }

}

