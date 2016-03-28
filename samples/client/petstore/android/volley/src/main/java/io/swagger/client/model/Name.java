package io.swagger.client.model;


import io.swagger.annotations.*;
import com.google.gson.annotations.SerializedName;


@ApiModel(description = "")
public class Name  {
  
  @SerializedName("name")
  private Integer name = null;
  @SerializedName("snake_case")
  private Integer snakeCase = null;

  
  /**
   **/
  @ApiModelProperty(value = "")
  public Integer getName() {
    return name;
  }
  public void setName(Integer name) {
    this.name = name;
  }

  
  /**
   **/
  @ApiModelProperty(value = "")
  public Integer getSnakeCase() {
    return snakeCase;
  }
  public void setSnakeCase(Integer snakeCase) {
    this.snakeCase = snakeCase;
  }

  

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class Name {\n");
    
    sb.append("  name: ").append(name).append("\n");
    sb.append("  snakeCase: ").append(snakeCase).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
