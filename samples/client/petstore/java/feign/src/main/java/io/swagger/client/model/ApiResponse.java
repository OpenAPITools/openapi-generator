package io.swagger.client.model;

import io.swagger.client.StringUtil;



import java.util.Objects;

import io.swagger.annotations.*;
import com.fasterxml.jackson.annotation.JsonProperty;



@ApiModel(description = "")
@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaClientCodegen", date = "2015-12-09T22:59:22.180-05:00")
public class ApiResponse   {
  
  private Integer code = null;
  private String type = null;
  private String message = null;

  
  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("code")
  public Integer getCode() {
    return code;
  }
  public void setCode(Integer code) {
    this.code = code;
  }

  
  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("type")
  public String getType() {
    return type;
  }
  public void setType(String type) {
    this.type = type;
  }

  
  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("message")
  public String getMessage() {
    return message;
  }
  public void setMessage(String message) {
    this.message = message;
  }

  

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ApiResponse apiResponse = (ApiResponse) o;
    return Objects.equals(code, apiResponse.code) &&
        Objects.equals(type, apiResponse.type) &&
        Objects.equals(message, apiResponse.message);
  }

  @Override
  public int hashCode() {
    return Objects.hash(code, type, message);
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class ApiResponse {\n");
    
    sb.append("    code: ").append(StringUtil.toIndentedString(code)).append("\n");
    sb.append("    type: ").append(StringUtil.toIndentedString(type)).append("\n");
    sb.append("    message: ").append(StringUtil.toIndentedString(message)).append("\n");
    sb.append("}");
    return sb.toString();
  }
}


