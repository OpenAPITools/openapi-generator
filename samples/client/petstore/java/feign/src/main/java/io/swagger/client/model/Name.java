package io.swagger.client.model;

import com.fasterxml.jackson.annotation.JsonValue;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;


/**
 * Model for testing model name same as property name
 */
@ApiModel(description = "Model for testing model name same as property name")
@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaClientCodegen", date = "2016-05-13T16:29:08.210Z")
public class Name   {
  
  private Integer name = null;
  private Integer snakeCase = null;
  private String property = null;
  private Integer _123Number = null;

  
  /**
   **/
  public Name name(Integer name) {
    this.name = name;
    return this;
  }
  
  @ApiModelProperty(example = "null", required = true, value = "")
  @JsonProperty("name")
  public Integer getName() {
    return name;
  }
  public void setName(Integer name) {
    this.name = name;
  }


  @ApiModelProperty(example = "null", value = "")
  @JsonProperty("snake_case")
  public Integer getSnakeCase() {
    return snakeCase;
  }


  /**
   **/
  public Name property(String property) {
    this.property = property;
    return this;
  }
  
  @ApiModelProperty(example = "null", value = "")
  @JsonProperty("property")
  public String getProperty() {
    return property;
  }
  public void setProperty(String property) {
    this.property = property;
  }


  @ApiModelProperty(example = "null", value = "")
  @JsonProperty("123Number")
  public Integer get123Number() {
    return _123Number;
  }


  @Override
  public boolean equals(java.lang.Object o) {
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
        Objects.equals(this._123Number, name._123Number);
  }

  @Override
  public int hashCode() {
    return Objects.hash(name, snakeCase, property, _123Number);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Name {\n");
    
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
  private String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

