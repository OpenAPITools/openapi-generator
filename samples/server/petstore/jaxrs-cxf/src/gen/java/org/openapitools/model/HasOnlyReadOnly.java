package org.openapitools.model;

import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.ApiModelProperty;
import com.fasterxml.jackson.annotation.JsonProperty;

public class HasOnlyReadOnly  {
  
  @ApiModelProperty(value = "")
  private String bar;

  @ApiModelProperty(value = "")
  private String foo;
 /**
   * Get bar
   * @return bar
  **/
  @JsonProperty("bar")
  public String getBar() {
    return bar;
  }


 /**
   * Get foo
   * @return foo
  **/
  @JsonProperty("foo")
  public String getFoo() {
    return foo;
  }



  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class HasOnlyReadOnly {\n");
    
    sb.append("    bar: ").append(toIndentedString(bar)).append("\n");
    sb.append("    foo: ").append(toIndentedString(foo)).append("\n");
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

