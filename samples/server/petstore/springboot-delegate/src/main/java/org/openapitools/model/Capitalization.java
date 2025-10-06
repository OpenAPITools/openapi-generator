package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * Capitalization
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
public class Capitalization {

  private @Nullable String smallCamel;

  private @Nullable String capitalCamel;

  private @Nullable String smallSnake;

  private @Nullable String capitalSnake;

  private @Nullable String scAETHFlowPoints;

  private @Nullable String ATT_NAME;

  public Capitalization() {
    super();
  }

  /**
   * Constructor with all args parameters
   */
  public Capitalization(@Nullable String smallCamel, @Nullable String capitalCamel, @Nullable String smallSnake, @Nullable String capitalSnake, @Nullable String scAETHFlowPoints, @Nullable String ATT_NAME) {
      this.smallCamel = smallCamel;
      this.capitalCamel = capitalCamel;
      this.smallSnake = smallSnake;
      this.capitalSnake = capitalSnake;
      this.scAETHFlowPoints = scAETHFlowPoints;
      this.ATT_NAME = ATT_NAME;
  }

  public Capitalization smallCamel(@Nullable String smallCamel) {
    this.smallCamel = smallCamel;
    return this;
  }

  /**
   * Get smallCamel
   * @return smallCamel
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("smallCamel")
  public @Nullable String getSmallCamel() {
    return smallCamel;
  }

  public void setSmallCamel(@Nullable String smallCamel) {
    this.smallCamel = smallCamel;
  }

  public Capitalization capitalCamel(@Nullable String capitalCamel) {
    this.capitalCamel = capitalCamel;
    return this;
  }

  /**
   * Get capitalCamel
   * @return capitalCamel
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("CapitalCamel")
  public @Nullable String getCapitalCamel() {
    return capitalCamel;
  }

  public void setCapitalCamel(@Nullable String capitalCamel) {
    this.capitalCamel = capitalCamel;
  }

  public Capitalization smallSnake(@Nullable String smallSnake) {
    this.smallSnake = smallSnake;
    return this;
  }

  /**
   * Get smallSnake
   * @return smallSnake
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("small_Snake")
  public @Nullable String getSmallSnake() {
    return smallSnake;
  }

  public void setSmallSnake(@Nullable String smallSnake) {
    this.smallSnake = smallSnake;
  }

  public Capitalization capitalSnake(@Nullable String capitalSnake) {
    this.capitalSnake = capitalSnake;
    return this;
  }

  /**
   * Get capitalSnake
   * @return capitalSnake
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("Capital_Snake")
  public @Nullable String getCapitalSnake() {
    return capitalSnake;
  }

  public void setCapitalSnake(@Nullable String capitalSnake) {
    this.capitalSnake = capitalSnake;
  }

  public Capitalization scAETHFlowPoints(@Nullable String scAETHFlowPoints) {
    this.scAETHFlowPoints = scAETHFlowPoints;
    return this;
  }

  /**
   * Get scAETHFlowPoints
   * @return scAETHFlowPoints
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("SCA_ETH_Flow_Points")
  public @Nullable String getScAETHFlowPoints() {
    return scAETHFlowPoints;
  }

  public void setScAETHFlowPoints(@Nullable String scAETHFlowPoints) {
    this.scAETHFlowPoints = scAETHFlowPoints;
  }

  public Capitalization ATT_NAME(@Nullable String ATT_NAME) {
    this.ATT_NAME = ATT_NAME;
    return this;
  }

  /**
   * Name of the pet 
   * @return ATT_NAME
   */
  
  @ApiModelProperty(value = "Name of the pet ")
  @JsonProperty("ATT_NAME")
  public @Nullable String getATTNAME() {
    return ATT_NAME;
  }

  public void setATTNAME(@Nullable String ATT_NAME) {
    this.ATT_NAME = ATT_NAME;
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

