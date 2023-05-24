package org.openapitools.model;

import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.ApiModelProperty;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;


public class SpecialModelName  {
  
  @ApiModelProperty(value = "")
  private Long $specialPropertyName;
 /**
  * Get $specialPropertyName
  * @return $specialPropertyName
  */
  @JsonProperty("$special[property.name]")
  public Long get$SpecialPropertyName() {
    return $specialPropertyName;
  }

  /**
   * Sets the <code>$specialPropertyName</code> property.
   */
 public void set$SpecialPropertyName(Long $specialPropertyName) {
    this.$specialPropertyName = $specialPropertyName;
  }

  /**
   * Sets the <code>$specialPropertyName</code> property.
   */
  public SpecialModelName $specialPropertyName(Long $specialPropertyName) {
    this.$specialPropertyName = $specialPropertyName;
    return this;
  }


  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class SpecialModelName {\n");
    
    sb.append("    $specialPropertyName: ").append(toIndentedString($specialPropertyName)).append("\n");
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

