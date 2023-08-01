package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.annotation.JsonTypeName;



@org.eclipse.microprofile.openapi.annotations.media.Schema(description="")
@JsonTypeName("Capitalization")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")
public class Capitalization  implements Serializable {
  private @Valid String smallCamel;
  private @Valid String capitalCamel;
  private @Valid String smallSnake;
  private @Valid String capitalSnake;
  private @Valid String scAETHFlowPoints;
  private @Valid String ATT_NAME;

  protected Capitalization(CapitalizationBuilder<?, ?> b) {
    this.smallCamel = b.smallCamel;
    this.capitalCamel = b.capitalCamel;
    this.smallSnake = b.smallSnake;
    this.capitalSnake = b.capitalSnake;
    this.scAETHFlowPoints = b.scAETHFlowPoints;
    this.ATT_NAME = b.ATT_NAME;
  }

  public Capitalization() {
  }

  /**
   **/
  public Capitalization smallCamel(String smallCamel) {
    this.smallCamel = smallCamel;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @org.eclipse.microprofile.openapi.annotations.media.Schema(description = "")
  @JsonProperty("smallCamel")
  public String getSmallCamel() {
    return smallCamel;
  }

  @JsonProperty("smallCamel")
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
  @org.eclipse.microprofile.openapi.annotations.media.Schema(description = "")
  @JsonProperty("CapitalCamel")
  public String getCapitalCamel() {
    return capitalCamel;
  }

  @JsonProperty("CapitalCamel")
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
  @org.eclipse.microprofile.openapi.annotations.media.Schema(description = "")
  @JsonProperty("small_Snake")
  public String getSmallSnake() {
    return smallSnake;
  }

  @JsonProperty("small_Snake")
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
  @org.eclipse.microprofile.openapi.annotations.media.Schema(description = "")
  @JsonProperty("Capital_Snake")
  public String getCapitalSnake() {
    return capitalSnake;
  }

  @JsonProperty("Capital_Snake")
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
  @org.eclipse.microprofile.openapi.annotations.media.Schema(description = "")
  @JsonProperty("SCA_ETH_Flow_Points")
  public String getScAETHFlowPoints() {
    return scAETHFlowPoints;
  }

  @JsonProperty("SCA_ETH_Flow_Points")
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
  @org.eclipse.microprofile.openapi.annotations.media.Schema(description = "Name of the pet ")
  @JsonProperty("ATT_NAME")
  public String getATTNAME() {
    return ATT_NAME;
  }

  @JsonProperty("ATT_NAME")
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


  public static CapitalizationBuilder<?, ?> builder() {
    return new CapitalizationBuilderImpl();
  }

  private static final class CapitalizationBuilderImpl extends CapitalizationBuilder<Capitalization, CapitalizationBuilderImpl> {

    @Override
    protected CapitalizationBuilderImpl self() {
      return this;
    }

    @Override
    public Capitalization build() {
      return new Capitalization(this);
    }
  }

  public static abstract class CapitalizationBuilder<C extends Capitalization, B extends CapitalizationBuilder<C, B>>  {
    private String smallCamel;
    private String capitalCamel;
    private String smallSnake;
    private String capitalSnake;
    private String scAETHFlowPoints;
    private String ATT_NAME;
    protected abstract B self();

    public abstract C build();

    public B smallCamel(String smallCamel) {
      this.smallCamel = smallCamel;
      return self();
    }
    public B capitalCamel(String capitalCamel) {
      this.capitalCamel = capitalCamel;
      return self();
    }
    public B smallSnake(String smallSnake) {
      this.smallSnake = smallSnake;
      return self();
    }
    public B capitalSnake(String capitalSnake) {
      this.capitalSnake = capitalSnake;
      return self();
    }
    public B scAETHFlowPoints(String scAETHFlowPoints) {
      this.scAETHFlowPoints = scAETHFlowPoints;
      return self();
    }
    public B ATT_NAME(String ATT_NAME) {
      this.ATT_NAME = ATT_NAME;
      return self();
    }
  }
}

