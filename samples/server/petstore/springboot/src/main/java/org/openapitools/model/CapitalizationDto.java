package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * CapitalizationDto
 */

@JsonTypeName("Capitalization")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.6.0-SNAPSHOT")
public class CapitalizationDto {

  private String smallCamel;

  private String capitalCamel;

  private String smallSnake;

  private String capitalSnake;

  private String scAETHFlowPoints;

  private String ATT_NAME;

  public CapitalizationDto smallCamel(String smallCamel) {
    this.smallCamel = smallCamel;
    return this;
  }

  /**
   * Get smallCamel
   * @return smallCamel
  */
  
  @ApiModelProperty(value = "")
  @JsonProperty("smallCamel")
  public String getSmallCamel() {
    return smallCamel;
  }

  public void setSmallCamel(String smallCamel) {
    this.smallCamel = smallCamel;
  }

  public CapitalizationDto capitalCamel(String capitalCamel) {
    this.capitalCamel = capitalCamel;
    return this;
  }

  /**
   * Get capitalCamel
   * @return capitalCamel
  */
  
  @ApiModelProperty(value = "")
  @JsonProperty("CapitalCamel")
  public String getCapitalCamel() {
    return capitalCamel;
  }

  public void setCapitalCamel(String capitalCamel) {
    this.capitalCamel = capitalCamel;
  }

  public CapitalizationDto smallSnake(String smallSnake) {
    this.smallSnake = smallSnake;
    return this;
  }

  /**
   * Get smallSnake
   * @return smallSnake
  */
  
  @ApiModelProperty(value = "")
  @JsonProperty("small_Snake")
  public String getSmallSnake() {
    return smallSnake;
  }

  public void setSmallSnake(String smallSnake) {
    this.smallSnake = smallSnake;
  }

  public CapitalizationDto capitalSnake(String capitalSnake) {
    this.capitalSnake = capitalSnake;
    return this;
  }

  /**
   * Get capitalSnake
   * @return capitalSnake
  */
  
  @ApiModelProperty(value = "")
  @JsonProperty("Capital_Snake")
  public String getCapitalSnake() {
    return capitalSnake;
  }

  public void setCapitalSnake(String capitalSnake) {
    this.capitalSnake = capitalSnake;
  }

  public CapitalizationDto scAETHFlowPoints(String scAETHFlowPoints) {
    this.scAETHFlowPoints = scAETHFlowPoints;
    return this;
  }

  /**
   * Get scAETHFlowPoints
   * @return scAETHFlowPoints
  */
  
  @ApiModelProperty(value = "")
  @JsonProperty("SCA_ETH_Flow_Points")
  public String getScAETHFlowPoints() {
    return scAETHFlowPoints;
  }

  public void setScAETHFlowPoints(String scAETHFlowPoints) {
    this.scAETHFlowPoints = scAETHFlowPoints;
  }

  public CapitalizationDto ATT_NAME(String ATT_NAME) {
    this.ATT_NAME = ATT_NAME;
    return this;
  }

  /**
   * Name of the pet 
   * @return ATT_NAME
  */
  
  @ApiModelProperty(value = "Name of the pet ")
  @JsonProperty("ATT_NAME")
  public String getATTNAME() {
    return ATT_NAME;
  }

  public void setATTNAME(String ATT_NAME) {
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
    CapitalizationDto capitalization = (CapitalizationDto) o;
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
    sb.append("class CapitalizationDto {\n");
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
  
  public static class Builder {

    private CapitalizationDto instance;

    public Builder() {
      this(new CapitalizationDto());
    }

    protected Builder(CapitalizationDto instance) {
      this.instance = instance;
    }

    protected Builder copyOf(CapitalizationDto value) { 
      this.instance.setSmallCamel(value.smallCamel);
      this.instance.setCapitalCamel(value.capitalCamel);
      this.instance.setSmallSnake(value.smallSnake);
      this.instance.setCapitalSnake(value.capitalSnake);
      this.instance.setScAETHFlowPoints(value.scAETHFlowPoints);
      this.instance.setATTNAME(value.ATT_NAME);
      return this;
    }

    public CapitalizationDto.Builder smallCamel(String smallCamel) {
      this.instance.smallCamel(smallCamel);
      return this;
    }
    
    public CapitalizationDto.Builder capitalCamel(String capitalCamel) {
      this.instance.capitalCamel(capitalCamel);
      return this;
    }
    
    public CapitalizationDto.Builder smallSnake(String smallSnake) {
      this.instance.smallSnake(smallSnake);
      return this;
    }
    
    public CapitalizationDto.Builder capitalSnake(String capitalSnake) {
      this.instance.capitalSnake(capitalSnake);
      return this;
    }
    
    public CapitalizationDto.Builder scAETHFlowPoints(String scAETHFlowPoints) {
      this.instance.scAETHFlowPoints(scAETHFlowPoints);
      return this;
    }
    
    public CapitalizationDto.Builder ATT_NAME(String ATT_NAME) {
      this.instance.ATT_NAME(ATT_NAME);
      return this;
    }
    
    /**
    * returns a built CapitalizationDto instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public CapitalizationDto build() {
      try {
        return this.instance;
      } finally {
        // ensure that this.instance is not reused
        this.instance = null;
      }
    }

    @Override
    public String toString() {
      return getClass() + "=(" + instance + ")";
    }
  }

  /**
  * Create a builder with no initialized field (except for the default values).
  */
  public static CapitalizationDto.Builder builder() {
    return new CapitalizationDto.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public CapitalizationDto.Builder toBuilder() {
    CapitalizationDto.Builder builder = new CapitalizationDto.Builder();
    return builder.copyOf(this);
  }

}

