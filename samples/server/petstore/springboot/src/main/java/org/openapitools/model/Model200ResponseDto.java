package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
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
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.6.0-SNAPSHOT")
public class Model200ResponseDto {

  private Integer name;

  private String propertyClass;

  public Model200ResponseDto name(Integer name) {
    this.name = name;
    return this;
  }

  /**
   * Get name
   * @return name
  */
  
  @ApiModelProperty(value = "")
  @JsonProperty("name")
  public Integer getName() {
    return name;
  }

  public void setName(Integer name) {
    this.name = name;
  }

  public Model200ResponseDto propertyClass(String propertyClass) {
    this.propertyClass = propertyClass;
    return this;
  }

  /**
   * Get propertyClass
   * @return propertyClass
  */
  
  @ApiModelProperty(value = "")
  @JsonProperty("class")
  public String getPropertyClass() {
    return propertyClass;
  }

  public void setPropertyClass(String propertyClass) {
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
    Model200ResponseDto _200Response = (Model200ResponseDto) o;
    return Objects.equals(this.name, _200Response.name) &&
        Objects.equals(this.propertyClass, _200Response.propertyClass);
  }

  @Override
  public int hashCode() {
    return Objects.hash(name, propertyClass);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Model200ResponseDto {\n");
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

    private Model200ResponseDto instance;

    public Builder() {
      this(new Model200ResponseDto());
    }

    protected Builder(Model200ResponseDto instance) {
      this.instance = instance;
    }

    protected Builder copyOf(Model200ResponseDto value) { 
      this.instance.setName(value.name);
      this.instance.setPropertyClass(value.propertyClass);
      return this;
    }

    public Model200ResponseDto.Builder name(Integer name) {
      this.instance.name(name);
      return this;
    }
    
    public Model200ResponseDto.Builder propertyClass(String propertyClass) {
      this.instance.propertyClass(propertyClass);
      return this;
    }
    
    /**
    * returns a built Model200ResponseDto instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public Model200ResponseDto build() {
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
  public static Model200ResponseDto.Builder builder() {
    return new Model200ResponseDto.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public Model200ResponseDto.Builder toBuilder() {
    Model200ResponseDto.Builder builder = new Model200ResponseDto.Builder();
    return builder.copyOf(this);
  }

}

