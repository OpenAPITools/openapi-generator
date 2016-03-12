package io.swagger.client.model;


import io.swagger.annotations.*;
import com.google.gson.annotations.SerializedName;


@ApiModel(description = "")
public class ModelReturn  {
  
  @SerializedName("return")
  private Integer _return = null;

  
  /**
   **/
  @ApiModelProperty(value = "")
  public Integer getReturn() {
    return _return;
  }
  public void setReturn(Integer _return) {
    this._return = _return;
  }

  

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class ModelReturn {\n");
    
    sb.append("  _return: ").append(_return).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
