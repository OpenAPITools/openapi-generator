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
 * Model for testing model name same as property name
 */

@ApiModel(description = "Model for testing model name same as property name")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
public class Name {

  private Integer name;

  private Optional<Integer> snakeCase = Optional.empty();

  private Optional<String> property = Optional.empty();

  private Optional<Integer> _123number = Optional.empty();

  public Name() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public Name(Integer name) {
    this.name = name;
  }

  public Name name(Integer name) {
    this.name = name;
    return this;
  }

  /**
   * Get name
   * @return name
   */
  @NotNull 
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("name")
  public Integer getName() {
    return name;
  }

  public void setName(Integer name) {
    this.name = name;
  }

  public Name snakeCase(Integer snakeCase) {
    this.snakeCase = Optional.ofNullable(snakeCase);
    return this;
  }

  /**
   * Get snakeCase
   * @return snakeCase
   */
  
  @ApiModelProperty(readOnly = true, value = "")
  @JsonProperty("snake_case")
  public Optional<Integer> getSnakeCase() {
    return snakeCase;
  }

  public void setSnakeCase(Optional<Integer> snakeCase) {
    this.snakeCase = snakeCase;
  }

  public Name property(String property) {
    this.property = Optional.ofNullable(property);
    return this;
  }

  /**
   * Get property
   * @return property
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("property")
  public Optional<String> getProperty() {
    return property;
  }

  public void setProperty(Optional<String> property) {
    this.property = property;
  }

  public Name _123number(Integer _123number) {
    this._123number = Optional.ofNullable(_123number);
    return this;
  }

  /**
   * Get _123number
   * @return _123number
   */
  
  @ApiModelProperty(readOnly = true, value = "")
  @JsonProperty("123Number")
  public Optional<Integer> get123number() {
    return _123number;
  }

  public void set123number(Optional<Integer> _123number) {
    this._123number = _123number;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Name name = (Name) o;
    return Objects.equals(this.name, name.name) &&
        Objects.equals(this.snakeCase, name.snakeCase) &&
        Objects.equals(this.property, name.property) &&
        Objects.equals(this._123number, name._123number);
  }

  @Override
  public int hashCode() {
    return Objects.hash(name, snakeCase, property, _123number);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Name {\n");
    sb.append("    name: ").append(toIndentedString(name)).append("\n");
    sb.append("    snakeCase: ").append(toIndentedString(snakeCase)).append("\n");
    sb.append("    property: ").append(toIndentedString(property)).append("\n");
    sb.append("    _123number: ").append(toIndentedString(_123number)).append("\n");
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

    private Name instance;

    public Builder() {
      this(new Name());
    }

    protected Builder(Name instance) {
      this.instance = instance;
    }

    protected Builder copyOf(Name value) { 
      this.instance.setName(value.name);
      this.instance.setSnakeCase(value.snakeCase);
      this.instance.setProperty(value.property);
      this.instance.set123number(value._123number);
      return this;
    }

    public Name.Builder name(Integer name) {
      this.instance.name(name);
      return this;
    }
    
    public Name.Builder snakeCase(Integer snakeCase) {
      this.instance.snakeCase(snakeCase);
      return this;
    }
    
    public Name.Builder property(String property) {
      this.instance.property(property);
      return this;
    }
    
    public Name.Builder _123number(Integer _123number) {
      this.instance._123number(_123number);
      return this;
    }
    
    /**
    * returns a built Name instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public Name build() {
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
  public static Name.Builder builder() {
    return new Name.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public Name.Builder toBuilder() {
    Name.Builder builder = new Name.Builder();
    return builder.copyOf(this);
  }

}

