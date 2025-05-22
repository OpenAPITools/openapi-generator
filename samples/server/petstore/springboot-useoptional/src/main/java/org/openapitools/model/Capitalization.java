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

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.14.0-SNAPSHOT")
public class Capitalization {

  private Optional<String> smallCamel = Optional.empty();

  private Optional<String> capitalCamel = Optional.empty();

  private Optional<String> smallSnake = Optional.empty();

  private Optional<String> capitalSnake = Optional.empty();

  private Optional<String> scAETHFlowPoints = Optional.empty();

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
  @JsonProperty("smallCamel")
  public Optional<String> getSmallCamel() {
    return smallCamel;
  }

  public void setSmallCamel(Optional<String> smallCamel) {
    this.smallCamel = smallCamel;
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
  @JsonProperty("CapitalCamel")
  public Optional<String> getCapitalCamel() {
    return capitalCamel;
  }

  public void setCapitalCamel(Optional<String> capitalCamel) {
    this.capitalCamel = capitalCamel;
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
  @JsonProperty("small_Snake")
  public Optional<String> getSmallSnake() {
    return smallSnake;
  }

  public void setSmallSnake(Optional<String> smallSnake) {
    this.smallSnake = smallSnake;
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
  @JsonProperty("Capital_Snake")
  public Optional<String> getCapitalSnake() {
    return capitalSnake;
  }

  public void setCapitalSnake(Optional<String> capitalSnake) {
    this.capitalSnake = capitalSnake;
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
  @JsonProperty("SCA_ETH_Flow_Points")
  public Optional<String> getScAETHFlowPoints() {
    return scAETHFlowPoints;
  }

  public void setScAETHFlowPoints(Optional<String> scAETHFlowPoints) {
    this.scAETHFlowPoints = scAETHFlowPoints;
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
  @JsonProperty("ATT_NAME")
  public Optional<String> getATTNAME() {
    return ATT_NAME;
  }

  public void setATTNAME(Optional<String> ATT_NAME) {
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
  
  public static class Builder {

    private Capitalization instance;

    public Builder() {
      this(new Capitalization());
    }

    protected Builder(Capitalization instance) {
      this.instance = instance;
    }

    protected Builder copyOf(Capitalization value) { 
      this.instance.setSmallCamel(value.smallCamel);
      this.instance.setCapitalCamel(value.capitalCamel);
      this.instance.setSmallSnake(value.smallSnake);
      this.instance.setCapitalSnake(value.capitalSnake);
      this.instance.setScAETHFlowPoints(value.scAETHFlowPoints);
      this.instance.setATTNAME(value.ATT_NAME);
      return this;
    }

    public Capitalization.Builder smallCamel(String smallCamel) {
      this.instance.smallCamel(smallCamel);
      return this;
    }
    
    public Capitalization.Builder capitalCamel(String capitalCamel) {
      this.instance.capitalCamel(capitalCamel);
      return this;
    }
    
    public Capitalization.Builder smallSnake(String smallSnake) {
      this.instance.smallSnake(smallSnake);
      return this;
    }
    
    public Capitalization.Builder capitalSnake(String capitalSnake) {
      this.instance.capitalSnake(capitalSnake);
      return this;
    }
    
    public Capitalization.Builder scAETHFlowPoints(String scAETHFlowPoints) {
      this.instance.scAETHFlowPoints(scAETHFlowPoints);
      return this;
    }
    
    public Capitalization.Builder ATT_NAME(String ATT_NAME) {
      this.instance.ATT_NAME(ATT_NAME);
      return this;
    }
    
    /**
    * returns a built Capitalization instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public Capitalization build() {
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
  public static Capitalization.Builder builder() {
    return new Capitalization.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public Capitalization.Builder toBuilder() {
    Capitalization.Builder builder = new Capitalization.Builder();
    return builder.copyOf(this);
  }

}

