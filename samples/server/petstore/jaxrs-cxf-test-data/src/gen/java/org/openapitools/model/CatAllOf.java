package org.openapitools.model;

import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.ApiModelProperty;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;


public class CatAllOf  {
  
  @ApiModelProperty(value = "")
  private Boolean declawed;
 /**
  * Get declawed
  * @return declawed
  */
  @JsonProperty("declawed")
  public Boolean getDeclawed() {
    return declawed;
  }

  /**
   * Sets the <code>declawed</code> property.
   */
  public void setDeclawed(Boolean declawed) {
    this.declawed = declawed;
  }

  /**
   * Sets the <code>declawed</code> property.
   */
  public CatAllOf declawed(Boolean declawed) {
    this.declawed = declawed;
    return this;
  }


  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class CatAllOf {\n");
    
    sb.append("    declawed: ").append(toIndentedString(declawed)).append("\n");
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

