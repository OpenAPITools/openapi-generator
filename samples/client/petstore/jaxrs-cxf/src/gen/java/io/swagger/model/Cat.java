package io.swagger.model;

import io.swagger.model.Animal;

import io.swagger.annotations.ApiModelProperty;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;

public class Cat extends Animal {
  
  @ApiModelProperty(example = "null", value = "")
  private Boolean declawed = null;

 /**
   * Get declawed
   * @return declawed
  **/
  public Boolean getDeclawed() {
    return declawed;
  }

  public void setDeclawed(Boolean declawed) {
    this.declawed = declawed;
  }

  public Cat declawed(Boolean declawed) {
    this.declawed = declawed;
    return this;
  }


  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Cat {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
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

