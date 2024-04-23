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
 * Model for testing model with \&quot;_class\&quot; property
 */

@ApiModel(description = "Model for testing model with \"_class\" property")
@JsonTypeName("ClassModel")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.6.0-SNAPSHOT")
public class ClassModelDto {

  private String propertyClass;

  public ClassModelDto propertyClass(String propertyClass) {
    this.propertyClass = propertyClass;
    return this;
  }

  /**
   * Get propertyClass
   * @return propertyClass
  */
  
  @ApiModelProperty(value = "")
  @JsonProperty("_class")
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
    ClassModelDto classModel = (ClassModelDto) o;
    return Objects.equals(this.propertyClass, classModel.propertyClass);
  }

  @Override
  public int hashCode() {
    return Objects.hash(propertyClass);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ClassModelDto {\n");
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

    private ClassModelDto instance;

    public Builder() {
      this(new ClassModelDto());
    }

    protected Builder(ClassModelDto instance) {
      this.instance = instance;
    }

    protected Builder copyOf(ClassModelDto value) { 
      this.instance.setPropertyClass(value.propertyClass);
      return this;
    }

    public ClassModelDto.Builder propertyClass(String propertyClass) {
      this.instance.propertyClass(propertyClass);
      return this;
    }
    
    /**
    * returns a built ClassModelDto instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public ClassModelDto build() {
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
  public static ClassModelDto.Builder builder() {
    return new ClassModelDto.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public ClassModelDto.Builder toBuilder() {
    ClassModelDto.Builder builder = new ClassModelDto.Builder();
    return builder.copyOf(this);
  }

}

