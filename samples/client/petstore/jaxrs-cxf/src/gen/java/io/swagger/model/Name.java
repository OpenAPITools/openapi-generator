package io.swagger.model;

import io.swagger.annotations.ApiModel;

import io.swagger.annotations.ApiModelProperty;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;

@ApiModel(description="Model for testing model name same as property name")
public class Name  {
  
  @ApiModelProperty(example = "null", required = true, value = "")
  private Integer name = null;
  @ApiModelProperty(example = "null", value = "")
  private Integer snakeCase = null;
  @ApiModelProperty(example = "null", value = "")
  private String property = null;
  @ApiModelProperty(example = "null", value = "")
  private Integer _123Number = null;

 /**
   * Get name
   * @return name
  **/
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
  public Integer getSnakeCase() {
    return snakeCase;
  }


 /**
   * Get property
   * @return property
  **/
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
  private static String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

