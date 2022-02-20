package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.Optional;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * Capitalization
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class Capitalization   {

  @JsonProperty("smallCamel")
  private Optional<String> smallCamel = Optional.empty();

  @JsonProperty("CapitalCamel")
  private Optional<String> capitalCamel = Optional.empty();

  @JsonProperty("small_Snake")
  private Optional<String> smallSnake = Optional.empty();

  @JsonProperty("Capital_Snake")
  private Optional<String> capitalSnake = Optional.empty();

  @JsonProperty("SCA_ETH_Flow_Points")
  private Optional<String> scAETHFlowPoints = Optional.empty();

  @JsonProperty("ATT_NAME")
  private Optional<String> ATT_NAME = Optional.empty();

  public Capitalization smallCamel(String smallCamel) {
    this.smallCamel = Optional.ofNullable(smallCamel);
    return this;
  }

  /**
   * Get smallCamel
   * @return smallCamel
  */
  @ApiModelProperty(value = "")
  public Optional<String> getSmallCamel() {
    return smallCamel;
  }

  @JsonIgnore
  public void setSmallCamel(String smallCamel) {
    this.smallCamel = Optional.ofNullable(smallCamel);
  }

  public Capitalization capitalCamel(String capitalCamel) {
    this.capitalCamel = Optional.ofNullable(capitalCamel);
    return this;
  }

  /**
   * Get capitalCamel
   * @return capitalCamel
  */
  @ApiModelProperty(value = "")
  public Optional<String> getCapitalCamel() {
    return capitalCamel;
  }

  @JsonIgnore
  public void setCapitalCamel(String capitalCamel) {
    this.capitalCamel = Optional.ofNullable(capitalCamel);
  }

  public Capitalization smallSnake(String smallSnake) {
    this.smallSnake = Optional.ofNullable(smallSnake);
    return this;
  }

  /**
   * Get smallSnake
   * @return smallSnake
  */
  @ApiModelProperty(value = "")
  public Optional<String> getSmallSnake() {
    return smallSnake;
  }

  @JsonIgnore
  public void setSmallSnake(String smallSnake) {
    this.smallSnake = Optional.ofNullable(smallSnake);
  }

  public Capitalization capitalSnake(String capitalSnake) {
    this.capitalSnake = Optional.ofNullable(capitalSnake);
    return this;
  }

  /**
   * Get capitalSnake
   * @return capitalSnake
  */
  @ApiModelProperty(value = "")
  public Optional<String> getCapitalSnake() {
    return capitalSnake;
  }

  @JsonIgnore
  public void setCapitalSnake(String capitalSnake) {
    this.capitalSnake = Optional.ofNullable(capitalSnake);
  }

  public Capitalization scAETHFlowPoints(String scAETHFlowPoints) {
    this.scAETHFlowPoints = Optional.ofNullable(scAETHFlowPoints);
    return this;
  }

  /**
   * Get scAETHFlowPoints
   * @return scAETHFlowPoints
  */
  @ApiModelProperty(value = "")
  public Optional<String> getScAETHFlowPoints() {
    return scAETHFlowPoints;
  }

  @JsonIgnore
  public void setScAETHFlowPoints(String scAETHFlowPoints) {
    this.scAETHFlowPoints = Optional.ofNullable(scAETHFlowPoints);
  }

  public Capitalization ATT_NAME(String ATT_NAME) {
    this.ATT_NAME = Optional.ofNullable(ATT_NAME);
    return this;
  }

  /**
   * Name of the pet 
   * @return ATT_NAME
  */
  @ApiModelProperty(value = "Name of the pet ")
  public Optional<String> getATTNAME() {
    return ATT_NAME;
  }

  @JsonIgnore
  public void setATTNAME(String ATT_NAME) {
    this.ATT_NAME = Optional.ofNullable(ATT_NAME);
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Capitalization capitalization = (Capitalization) o;
    return Objects.equals(this.smallCamel, capitalization.smallCamel) &&
        Objects.equals(this.capitalCamel, capitalization.capitalCamel) &&
        Objects.equals(this.smallSnake, capitalization.smallSnake) &&
        Objects.equals(this.capitalSnake, capitalization.capitalSnake) &&
        Objects.equals(this.scAETHFlowPoints, capitalization.scAETHFlowPoints) &&
        Objects.equals(this.ATT_NAME, capitalization.ATT_NAME);
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
  private String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

