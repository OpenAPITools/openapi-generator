package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.Optional;
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
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class Name   {

  @JsonProperty("name")
  private Integer name;

  @JsonProperty("snake_case")
  private Optional<Integer> snakeCase = Optional.empty();

  @JsonProperty("property")
  private Optional<String> property = Optional.empty();

  @JsonProperty("123Number")
  private Optional<Integer> _123number = Optional.empty();

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
  public Optional<Integer> getSnakeCase() {
    return snakeCase;
  }

  @JsonIgnore
  public void setSnakeCase(Integer snakeCase) {
    this.snakeCase = Optional.ofNullable(snakeCase);
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
  public Optional<String> getProperty() {
    return property;
  }

  @JsonIgnore
  public void setProperty(String property) {
    this.property = Optional.ofNullable(property);
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
  public Optional<Integer> get123number() {
    return _123number;
  }

  @JsonIgnore
  public void set123number(Integer _123number) {
    this._123number = Optional.ofNullable(_123number);
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
}

