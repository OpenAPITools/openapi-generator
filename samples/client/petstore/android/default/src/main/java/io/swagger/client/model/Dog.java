package io.swagger.client.model;

import io.swagger.client.model.Animal;

import io.swagger.annotations.*;
import com.google.gson.annotations.SerializedName;


@ApiModel(description = "")
public class Dog extends Animal {
  
  @SerializedName("className")
  private String className = null;
  @SerializedName("breed")
  private String breed = null;

  
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
  public String getBreed() {
    return breed;
  }
  public void setBreed(String breed) {
    this.breed = breed;
  }

  

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class Dog {\n");
    sb.append("  " + super.toString()).append("\n");
    sb.append("  className: ").append(className).append("\n");
    sb.append("  breed: ").append(breed).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
