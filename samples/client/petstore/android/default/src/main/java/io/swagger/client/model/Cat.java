package io.swagger.client.model;

import io.swagger.client.model.Animal;

import io.swagger.annotations.*;
import com.google.gson.annotations.SerializedName;


@ApiModel(description = "")
public class Cat extends Animal {
  
  @SerializedName("className")
  private String className = null;
  @SerializedName("declawed")
  private Boolean declawed = null;

  
  /**
   **/
  @ApiModelProperty(required = true, value = "")
  public String getClassName() {
    return className;
  }
  public void setClassName(String className) {
    this.className = className;
  }

  
  /**
   **/
  @ApiModelProperty(value = "")
  public Boolean getDeclawed() {
    return declawed;
  }
  public void setDeclawed(Boolean declawed) {
    this.declawed = declawed;
  }

  

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class Cat {\n");
    sb.append("  " + super.toString()).append("\n");
    sb.append("  className: ").append(className).append("\n");
    sb.append("  declawed: ").append(declawed).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
