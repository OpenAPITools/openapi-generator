package org.openapitools.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

/**
 * Model for testing model name same as property name
 */
@ApiModel(description = "Model for testing model name same as property name")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaMSF4JServerCodegen")
public class Name   {
  @JsonProperty("name")
  private Integer name;

  @JsonProperty("snake_case")
  private Integer snakeCase;

  @JsonProperty("property")
  private String property;

  @JsonProperty("123Number")
  private Integer _123number;

  public Name name(Integer name) {
    this.name = name;
    return this;
  }

   /**
   * Get name
   * @return name
  **/
  @ApiModelProperty(required = true, value = "")
  public Integer getName() {
    return name;
  }

  public void setName(Integer name) {
    this.name = name;
  }

   /**
   * Get snakeCase
   * @return snakeCase
  **/
  @ApiModelProperty(value = "")
  public Integer getSnakeCase() {
    return snakeCase;
  }

  public Name property(String property) {
    this.property = property;
    return this;
  }

   /**
   * Get property
   * @return property
  **/
  @ApiModelProperty(value = "")
  public String getProperty() {
    return property;
  }

  public void setProperty(String property) {
    this.property = property;
  }

   /**
   * Get _123number
   * @return _123number
  **/
  @ApiModelProperty(value = "")
  public Integer get123number() {
    return _123number;
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

