package io.swagger.client.model;


import io.swagger.annotations.*;
import com.google.gson.annotations.SerializedName;


@ApiModel(description = "")
public class Animal  {
  
  @SerializedName("className")
  private String className = null;

  
  /**
   **/
  @ApiModelProperty(required = true, value = "")
  public String getClassName() {
    return className;
  }
  public void setClassName(String className) {
    this.className = className;
  }

  

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class Animal {\n");
    
    sb.append("  className: ").append(className).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
