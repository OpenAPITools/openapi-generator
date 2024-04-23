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
 * Model for testing model name same as property name
 */

@ApiModel(description = "Model for testing model name same as property name")
@JsonTypeName("Name")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.6.0-SNAPSHOT")
public class NameDto {

  private Integer name;

  private Integer snakeCase;

  private String property;

  private Integer _123Number;

  public NameDto() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public NameDto(Integer name) {
    this.name = name;
  }

  public NameDto name(Integer name) {
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

  public NameDto snakeCase(Integer snakeCase) {
    this.snakeCase = snakeCase;
    return this;
  }

  /**
   * Get snakeCase
   * @return snakeCase
  */
  
  @ApiModelProperty(readOnly = true, value = "")
  @JsonProperty("snake_case")
  public Integer getSnakeCase() {
    return snakeCase;
  }

  public void setSnakeCase(Integer snakeCase) {
    this.snakeCase = snakeCase;
  }

  public NameDto property(String property) {
    this.property = property;
    return this;
  }

  /**
   * Get property
   * @return property
  */
  
  @ApiModelProperty(value = "")
  @JsonProperty("property")
  public String getProperty() {
    return property;
  }

  public void setProperty(String property) {
    this.property = property;
  }

  public NameDto _123Number(Integer _123Number) {
    this._123Number = _123Number;
    return this;
  }

  /**
   * Get _123Number
   * @return _123Number
  */
  
  @ApiModelProperty(readOnly = true, value = "")
  @JsonProperty("123Number")
  public Integer get123Number() {
    return _123Number;
  }

  public void set123Number(Integer _123Number) {
    this._123Number = _123Number;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    NameDto name = (NameDto) o;
    return Objects.equals(this.name, name.name) &&
        Objects.equals(this.snakeCase, name.snakeCase) &&
        Objects.equals(this.property, name.property) &&
        Objects.equals(this._123Number, name._123Number);
  }

  @Override
  public int hashCode() {
    return Objects.hash(name, snakeCase, property, _123Number);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class NameDto {\n");
    sb.append("    name: ").append(toIndentedString(name)).append("\n");
    sb.append("    snakeCase: ").append(toIndentedString(snakeCase)).append("\n");
    sb.append("    property: ").append(toIndentedString(property)).append("\n");
    sb.append("    _123Number: ").append(toIndentedString(_123Number)).append("\n");
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

    private NameDto instance;

    public Builder() {
      this(new NameDto());
    }

    protected Builder(NameDto instance) {
      this.instance = instance;
    }

    protected Builder copyOf(NameDto value) { 
      this.instance.setName(value.name);
      this.instance.setSnakeCase(value.snakeCase);
      this.instance.setProperty(value.property);
      this.instance.set123Number(value._123Number);
      return this;
    }

    public NameDto.Builder name(Integer name) {
      this.instance.name(name);
      return this;
    }
    
    public NameDto.Builder snakeCase(Integer snakeCase) {
      this.instance.snakeCase(snakeCase);
      return this;
    }
    
    public NameDto.Builder property(String property) {
      this.instance.property(property);
      return this;
    }
    
    public NameDto.Builder _123Number(Integer _123Number) {
      this.instance._123Number(_123Number);
      return this;
    }
    
    /**
    * returns a built NameDto instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public NameDto build() {
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
  public static NameDto.Builder builder() {
    return new NameDto.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public NameDto.Builder toBuilder() {
    NameDto.Builder builder = new NameDto.Builder();
    return builder.copyOf(this);
  }

}

