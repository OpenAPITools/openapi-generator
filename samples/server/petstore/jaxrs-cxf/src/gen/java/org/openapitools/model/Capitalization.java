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
import com.fasterxml.jackson.annotation.JsonProperty;

public class Capitalization  {
  
  @ApiModelProperty(value = "")
  private String smallCamel;

  @ApiModelProperty(value = "")
  private String capitalCamel;

  @ApiModelProperty(value = "")
  private String smallSnake;

  @ApiModelProperty(value = "")
  private String capitalSnake;

  @ApiModelProperty(value = "")
  private String scAETHFlowPoints;

  @ApiModelProperty(value = "Name of the pet ")
 /**
   * Name of the pet 
  **/
  private String ATT_NAME;
 /**
   * Get smallCamel
   * @return smallCamel
  **/
  @JsonProperty("smallCamel")
  public String getSmallCamel() {
    return smallCamel;
  }

  public void setSmallCamel(String smallCamel) {
    this.smallCamel = smallCamel;
  }

  public Capitalization smallCamel(String smallCamel) {
    this.smallCamel = smallCamel;
    return this;
  }

 /**
   * Get capitalCamel
   * @return capitalCamel
  **/
  @JsonProperty("CapitalCamel")
  public String getCapitalCamel() {
    return capitalCamel;
  }

  public void setCapitalCamel(String capitalCamel) {
    this.capitalCamel = capitalCamel;
  }

  public Capitalization capitalCamel(String capitalCamel) {
    this.capitalCamel = capitalCamel;
    return this;
  }

 /**
   * Get smallSnake
   * @return smallSnake
  **/
  @JsonProperty("small_Snake")
  public String getSmallSnake() {
    return smallSnake;
  }

  public void setSmallSnake(String smallSnake) {
    this.smallSnake = smallSnake;
  }

  public Capitalization smallSnake(String smallSnake) {
    this.smallSnake = smallSnake;
    return this;
  }

 /**
   * Get capitalSnake
   * @return capitalSnake
  **/
  @JsonProperty("Capital_Snake")
  public String getCapitalSnake() {
    return capitalSnake;
  }

  public void setCapitalSnake(String capitalSnake) {
    this.capitalSnake = capitalSnake;
  }

  public Capitalization capitalSnake(String capitalSnake) {
    this.capitalSnake = capitalSnake;
    return this;
  }

 /**
   * Get scAETHFlowPoints
   * @return scAETHFlowPoints
  **/
  @JsonProperty("SCA_ETH_Flow_Points")
  public String getScAETHFlowPoints() {
    return scAETHFlowPoints;
  }

  public void setScAETHFlowPoints(String scAETHFlowPoints) {
    this.scAETHFlowPoints = scAETHFlowPoints;
  }

  public Capitalization scAETHFlowPoints(String scAETHFlowPoints) {
    this.scAETHFlowPoints = scAETHFlowPoints;
    return this;
  }

 /**
   * Name of the pet 
   * @return ATT_NAME
  **/
  @JsonProperty("ATT_NAME")
  public String getATTNAME() {
    return ATT_NAME;
  }

  public void setATTNAME(String ATT_NAME) {
    this.ATT_NAME = ATT_NAME;
  }

  public Capitalization ATT_NAME(String ATT_NAME) {
    this.ATT_NAME = ATT_NAME;
    return this;
  }


  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Capitalization {\n");
    
    sb.append("    smallCamel: ").append(toIndentedString(smallCamel)).append("\n");
    sb.append("    capitalCamel: ").append(toIndentedString(capitalCamel)).append("\n");
    sb.append("    smallSnake: ").append(toIndentedString(smallSnake)).append("\n");
    sb.append("    capitalSnake: ").append(toIndentedString(capitalSnake)).append("\n");
    sb.append("    scAETHFlowPoints: ").append(toIndentedString(scAETHFlowPoints)).append("\n");
    sb.append("    ATT_NAME: ").append(toIndentedString(ATT_NAME)).append("\n");
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

