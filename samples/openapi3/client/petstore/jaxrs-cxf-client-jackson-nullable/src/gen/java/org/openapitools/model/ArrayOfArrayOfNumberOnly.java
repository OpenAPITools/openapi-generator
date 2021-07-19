package org.openapitools.model;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import io.swagger.annotations.ApiModelProperty;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
import com.fasterxml.jackson.annotation.JsonProperty;

public class ArrayOfArrayOfNumberOnly  {
  
  @ApiModelProperty(value = "")
  private List<List<BigDecimal>> arrayArrayNumber = null;
 /**
   * Get arrayArrayNumber
   * @return arrayArrayNumber
  **/
  @JsonProperty("ArrayArrayNumber")
  public List<List<BigDecimal>> getArrayArrayNumber() {
    return arrayArrayNumber;
  }

  public void setArrayArrayNumber(List<List<BigDecimal>> arrayArrayNumber) {
    this.arrayArrayNumber = arrayArrayNumber;
  }

  public ArrayOfArrayOfNumberOnly arrayArrayNumber(List<List<BigDecimal>> arrayArrayNumber) {
    this.arrayArrayNumber = arrayArrayNumber;
    return this;
  }

  public ArrayOfArrayOfNumberOnly addArrayArrayNumberItem(List<BigDecimal> arrayArrayNumberItem) {
    this.arrayArrayNumber.add(arrayArrayNumberItem);
    return this;
  }


  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ArrayOfArrayOfNumberOnly {\n");
    
    sb.append("    arrayArrayNumber: ").append(toIndentedString(arrayArrayNumber)).append("\n");
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

