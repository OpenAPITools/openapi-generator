package io.swagger.model;

import javax.validation.constraints.*;
import javax.validation.Valid;


import io.swagger.annotations.*;
import java.util.Objects;


public class Capitalization   {
  
  private @Valid String smallCamel = null;
  private @Valid String capitalCamel = null;
  private @Valid String smallSnake = null;
  private @Valid String capitalSnake = null;
  private @Valid String scAETHFlowPoints = null;
  private @Valid String ATT_NAME = null;

  /**
   **/
  public Capitalization smallCamel(String smallCamel) {
    this.smallCamel = smallCamel;
    return this;
  }

  
  @ApiModelProperty(value = "")
  public String getSmallCamel() {
    return smallCamel;
  }
  public void setSmallCamel(String smallCamel) {
    this.smallCamel = smallCamel;
  }

  /**
   **/
  public Capitalization capitalCamel(String capitalCamel) {
    this.capitalCamel = capitalCamel;
    return this;
  }

  
  @ApiModelProperty(value = "")
  public String getCapitalCamel() {
    return capitalCamel;
  }
  public void setCapitalCamel(String capitalCamel) {
    this.capitalCamel = capitalCamel;
  }

  /**
   **/
  public Capitalization smallSnake(String smallSnake) {
    this.smallSnake = smallSnake;
    return this;
  }

  
  @ApiModelProperty(value = "")
  public String getSmallSnake() {
    return smallSnake;
  }
  public void setSmallSnake(String smallSnake) {
    this.smallSnake = smallSnake;
  }

  /**
   **/
  public Capitalization capitalSnake(String capitalSnake) {
    this.capitalSnake = capitalSnake;
    return this;
  }

  
  @ApiModelProperty(value = "")
  public String getCapitalSnake() {
    return capitalSnake;
  }
  public void setCapitalSnake(String capitalSnake) {
    this.capitalSnake = capitalSnake;
  }

  /**
   **/
  public Capitalization scAETHFlowPoints(String scAETHFlowPoints) {
    this.scAETHFlowPoints = scAETHFlowPoints;
    return this;
  }

  
  @ApiModelProperty(value = "")
  public String getScAETHFlowPoints() {
    return scAETHFlowPoints;
  }
  public void setScAETHFlowPoints(String scAETHFlowPoints) {
    this.scAETHFlowPoints = scAETHFlowPoints;
  }

  /**
   * Name of the pet 
   **/
  public Capitalization ATT_NAME(String ATT_NAME) {
    this.ATT_NAME = ATT_NAME;
    return this;
  }

  
  @ApiModelProperty(value = "Name of the pet ")
  public String getATTNAME() {
    return ATT_NAME;
  }
  public void setATTNAME(String ATT_NAME) {
    this.ATT_NAME = ATT_NAME;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Capitalization capitalization = (Capitalization) o;
    return Objects.equals(smallCamel, capitalization.smallCamel) &&
        Objects.equals(capitalCamel, capitalization.capitalCamel) &&
        Objects.equals(smallSnake, capitalization.smallSnake) &&
        Objects.equals(capitalSnake, capitalization.capitalSnake) &&
        Objects.equals(scAETHFlowPoints, capitalization.scAETHFlowPoints) &&
        Objects.equals(ATT_NAME, capitalization.ATT_NAME);
  }

  @Override
  public int hashCode() {
    return Objects.hash(smallCamel, capitalCamel, smallSnake, capitalSnake, scAETHFlowPoints, ATT_NAME);
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
  private String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

