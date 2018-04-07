package io.swagger.model;

import io.swagger.annotations.ApiModel;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.ApiModelProperty;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
  * Model for testing model name same as property name
 **/
@ApiModel(description="Model for testing model name same as property name")
public class Name  {
  
  @ApiModelProperty(required = true, value = "")
  private Integer name = null;

  @ApiModelProperty(value = "")
  private Integer snakeCase = null;

  @ApiModelProperty(value = "")
  private String property = null;

  @ApiModelProperty(value = "")
  private Integer _123Number = null;
 /**
   * Get name
   * @return name
  **/
  @JsonProperty("name")
  @NotNull
  public Integer getName() {
    return name;
  }

  public void setName(Integer name) {
    this.name = name;
  }

  public Name name(Integer name) {
    this.name = name;
    return this;
  }

 /**
   * Get snakeCase
   * @return snakeCase
  **/
  @JsonProperty("snake_case")
  public Integer getSnakeCase() {
    return snakeCase;
  }


 /**
   * Get property
   * @return property
  **/
  @JsonProperty("property")
  public String getProperty() {
    return property;
  }

  public void setProperty(String property) {
    this.property = property;
  }

  public Name property(String property) {
    this.property = property;
    return this;
  }

 /**
   * Get _123Number
   * @return _123Number
  **/
  @JsonProperty("123Number")
  public Integer get123Number() {
    return _123Number;
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
  private static String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

