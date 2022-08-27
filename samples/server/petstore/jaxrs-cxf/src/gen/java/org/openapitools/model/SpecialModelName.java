package org.openapitools.model;

import com.fasterxml.jackson.annotation.JsonTypeName;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.ApiModelProperty;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;


public class SpecialModelName  {
  
  @ApiModelProperty(value = "")
  private Long $SpecialPropertyName;
 /**
   * Get $SpecialPropertyName
   * @return $SpecialPropertyName
  **/
  @JsonProperty("$special[property.name]")
  public Long get$SpecialPropertyName() {
    return $SpecialPropertyName;
  }

  public void set$SpecialPropertyName(Long $SpecialPropertyName) {
    this.$SpecialPropertyName = $SpecialPropertyName;
  }

  public SpecialModelName $SpecialPropertyName(Long $SpecialPropertyName) {
    this.$SpecialPropertyName = $SpecialPropertyName;
    return this;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    SpecialModelName $SpecialModelName = (SpecialModelName) o;
    return Objects.equals($SpecialPropertyName, $SpecialModelName.$SpecialPropertyName);
  }

  @Override
  public int hashCode() {
    return Objects.hash($SpecialPropertyName);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class SpecialModelName {\n");
    
    sb.append("    $SpecialPropertyName: ").append(toIndentedString($SpecialPropertyName)).append("\n");
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

