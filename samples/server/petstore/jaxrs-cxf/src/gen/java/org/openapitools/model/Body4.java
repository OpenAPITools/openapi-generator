package org.openapitools.model;

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

public class Body4  {
  
  @ApiModelProperty(required = true, value = "field1")
 /**
   * field1
  **/
  private String param = null;

  @ApiModelProperty(required = true, value = "field2")
 /**
   * field2
  **/
  private String param2 = null;
 /**
   * field1
   * @return param
  **/
  @JsonProperty("param")
  @NotNull
  public String getParam() {
    return param;
  }

  public void setParam(String param) {
    this.param = param;
  }

  public Body4 param(String param) {
    this.param = param;
    return this;
  }

 /**
   * field2
   * @return param2
  **/
  @JsonProperty("param2")
  @NotNull
  public String getParam2() {
    return param2;
  }

  public void setParam2(String param2) {
    this.param2 = param2;
  }

  public Body4 param2(String param2) {
    this.param2 = param2;
    return this;
  }


  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Body4 {\n");
    
    sb.append("    param: ").append(toIndentedString(param)).append("\n");
    sb.append("    param2: ").append(toIndentedString(param2)).append("\n");
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

