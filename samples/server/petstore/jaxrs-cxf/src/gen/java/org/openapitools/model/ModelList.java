package org.openapitools.model;

import com.fasterxml.jackson.annotation.JsonTypeName;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.ApiModelProperty;
import com.fasterxml.jackson.annotation.JsonProperty;

public class ModelList  {
  
  @ApiModelProperty(value = "")
  private String _123list;
 /**
   * Get _123list
   * @return _123list
  **/
  @JsonProperty("123-list")
  public String get123list() {
    return _123list;
  }

  public void set123list(String _123list) {
    this._123list = _123list;
  }

  public ModelList _123list(String _123list) {
    this._123list = _123list;
    return this;
  }


  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ModelList {\n");
    
    sb.append("    _123list: ").append(toIndentedString(_123list)).append("\n");
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

