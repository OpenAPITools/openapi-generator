package io.swagger.client.model;


import io.swagger.annotations.*;
import com.google.gson.annotations.SerializedName;


@ApiModel(description = "")
public class SpecialModelName  {
  
  @SerializedName("$special[property.name]")
  private Long specialPropertyName = null;

  
  /**
   **/
  @ApiModelProperty(value = "")
  public Long getSpecialPropertyName() {
    return specialPropertyName;
  }
  public void setSpecialPropertyName(Long specialPropertyName) {
    this.specialPropertyName = specialPropertyName;
  }

  

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class SpecialModelName {\n");
    
    sb.append("  specialPropertyName: ").append(specialPropertyName).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
