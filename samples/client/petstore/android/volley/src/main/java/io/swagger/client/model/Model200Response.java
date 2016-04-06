package io.swagger.client.model;


import io.swagger.annotations.*;
import com.google.gson.annotations.SerializedName;


@ApiModel(description = "")
public class Model200Response  {
  
  @SerializedName("name")
  private Integer name = null;

  
  /**
   **/
  @ApiModelProperty(value = "")
  public Integer getName() {
    return name;
  }
  public void setName(Integer name) {
    this.name = name;
  }

  

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class Model200Response {\n");
    
    sb.append("  name: ").append(name).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
