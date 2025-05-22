package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
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
 * Model for testing model with \&quot;_class\&quot; property
 */

@ApiModel(description = "Model for testing model with \"_class\" property")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.14.0-SNAPSHOT")
public class ClassModel {

  private Optional<String> propertyClass = Optional.empty();

  public ClassModel propertyClass(String propertyClass) {
    this.propertyClass = Optional.ofNullable(propertyClass);
    return this;
  }

  /**
   * Get propertyClass
   * @return propertyClass
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("_class")
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
    ClassModel classModel = (ClassModel) o;
    return Objects.equals(this.propertyClass, classModel.propertyClass);
  }

  @Override
  public int hashCode() {
    return Objects.hash(propertyClass);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ClassModel {\n");
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

    private ClassModel instance;

    public Builder() {
      this(new ClassModel());
    }

    protected Builder(ClassModel instance) {
      this.instance = instance;
    }

    protected Builder copyOf(ClassModel value) { 
      this.instance.setPropertyClass(value.propertyClass);
      return this;
    }

    public ClassModel.Builder propertyClass(String propertyClass) {
      this.instance.propertyClass(propertyClass);
      return this;
    }
    
    /**
    * returns a built ClassModel instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public ClassModel build() {
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
  public static ClassModel.Builder builder() {
    return new ClassModel.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public ClassModel.Builder toBuilder() {
    ClassModel.Builder builder = new ClassModel.Builder();
    return builder.copyOf(this);
  }

}

