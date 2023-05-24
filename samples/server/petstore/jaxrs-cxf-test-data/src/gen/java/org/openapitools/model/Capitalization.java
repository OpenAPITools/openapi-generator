package org.openapitools.model;

import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.ApiModelProperty;
import com.fasterxml.jackson.annotation.JsonFormat;
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

 /**
  * Name of the pet 
  */
  @ApiModelProperty(value = "Name of the pet ")
  private String ATT_NAME;
 /**
  * Get smallCamel
  * @return smallCamel
  */
  @JsonProperty("smallCamel")
  public String getSmallCamel() {
    return smallCamel;
  }

  /**
   * Sets the <code>smallCamel</code> property.
   */
 public void setSmallCamel(String smallCamel) {
    this.smallCamel = smallCamel;
  }

  /**
   * Sets the <code>smallCamel</code> property.
   */
  public Capitalization smallCamel(String smallCamel) {
    this.smallCamel = smallCamel;
    return this;
  }

 /**
  * Get capitalCamel
  * @return capitalCamel
  */
  @JsonProperty("CapitalCamel")
  public String getCapitalCamel() {
    return capitalCamel;
  }

  /**
   * Sets the <code>capitalCamel</code> property.
   */
 public void setCapitalCamel(String capitalCamel) {
    this.capitalCamel = capitalCamel;
  }

  /**
   * Sets the <code>capitalCamel</code> property.
   */
  public Capitalization capitalCamel(String capitalCamel) {
    this.capitalCamel = capitalCamel;
    return this;
  }

 /**
  * Get smallSnake
  * @return smallSnake
  */
  @JsonProperty("small_Snake")
  public String getSmallSnake() {
    return smallSnake;
  }

  /**
   * Sets the <code>smallSnake</code> property.
   */
 public void setSmallSnake(String smallSnake) {
    this.smallSnake = smallSnake;
  }

  /**
   * Sets the <code>smallSnake</code> property.
   */
  public Capitalization smallSnake(String smallSnake) {
    this.smallSnake = smallSnake;
    return this;
  }

 /**
  * Get capitalSnake
  * @return capitalSnake
  */
  @JsonProperty("Capital_Snake")
  public String getCapitalSnake() {
    return capitalSnake;
  }

  /**
   * Sets the <code>capitalSnake</code> property.
   */
 public void setCapitalSnake(String capitalSnake) {
    this.capitalSnake = capitalSnake;
  }

  /**
   * Sets the <code>capitalSnake</code> property.
   */
  public Capitalization capitalSnake(String capitalSnake) {
    this.capitalSnake = capitalSnake;
    return this;
  }

 /**
  * Get scAETHFlowPoints
  * @return scAETHFlowPoints
  */
  @JsonProperty("SCA_ETH_Flow_Points")
  public String getScAETHFlowPoints() {
    return scAETHFlowPoints;
  }

  /**
   * Sets the <code>scAETHFlowPoints</code> property.
   */
 public void setScAETHFlowPoints(String scAETHFlowPoints) {
    this.scAETHFlowPoints = scAETHFlowPoints;
  }

  /**
   * Sets the <code>scAETHFlowPoints</code> property.
   */
  public Capitalization scAETHFlowPoints(String scAETHFlowPoints) {
    this.scAETHFlowPoints = scAETHFlowPoints;
    return this;
  }

 /**
  * Name of the pet 
  * @return ATT_NAME
  */
  @JsonProperty("ATT_NAME")
  public String getATTNAME() {
    return ATT_NAME;
  }

  /**
   * Sets the <code>ATT_NAME</code> property.
   */
 public void setATTNAME(String ATT_NAME) {
    this.ATT_NAME = ATT_NAME;
  }

  /**
   * Sets the <code>ATT_NAME</code> property.
   */
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

