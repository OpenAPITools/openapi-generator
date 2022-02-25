package org.openapitools.model;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.ApiModelProperty;
import com.fasterxml.jackson.annotation.JsonProperty;

public class ArrayOfNumberOnly  {
  
  @ApiModelProperty(value = "")
  @Valid
  private List<BigDecimal> arrayNumber = null;
 /**
   * Get arrayNumber
   * @return arrayNumber
  **/
  @JsonProperty("ArrayNumber")
  public List<BigDecimal> getArrayNumber() {
    return arrayNumber;
  }

  public void setArrayNumber(List<BigDecimal> arrayNumber) {
    this.arrayNumber = arrayNumber;
  }

  public ArrayOfNumberOnly arrayNumber(List<BigDecimal> arrayNumber) {
    this.arrayNumber = arrayNumber;
    return this;
  }

  public ArrayOfNumberOnly addArrayNumberItem(BigDecimal arrayNumberItem) {
    this.arrayNumber.add(arrayNumberItem);
    return this;
  }


  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ArrayOfNumberOnly {\n");
    
    sb.append("    arrayNumber: ").append(toIndentedString(arrayNumber)).append("\n");
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

