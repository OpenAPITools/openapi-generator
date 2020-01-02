package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.math.BigDecimal;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;



public class OuterComposite  implements Serializable {
  
  private @Valid BigDecimal myNumber;
  private @Valid String myString;
  private @Valid Boolean myBoolean;

  public OuterComposite(BigDecimal myNumber, String myString, Boolean myBoolean) {
    this.myNumber = myNumber;
    this.myString = myString;
    this.myBoolean = myBoolean;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("my_number")
  public BigDecimal getMyNumber() {
    return myNumber;
  }

  public void setMyNumber(BigDecimal myNumber) {
    this.myNumber = myNumber;
  }
  @ApiModelProperty(value = "")
  @JsonProperty("my_string")
  public String getMyString() {
    return myString;
  }

  public void setMyString(String myString) {
    this.myString = myString;
  }
  @ApiModelProperty(value = "")
  @JsonProperty("my_boolean")
  public Boolean getMyBoolean() {
    return myBoolean;
  }

  public void setMyBoolean(Boolean myBoolean) {
    this.myBoolean = myBoolean;
  }

  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    OuterComposite outerComposite = (OuterComposite) o;
    return Objects.equals(this.myNumber, outerComposite.myNumber) &&
        Objects.equals(this.myString, outerComposite.myString) &&
        Objects.equals(this.myBoolean, outerComposite.myBoolean);
  }

  @Override
  public int hashCode() {
    return Objects.hash(myNumber, myString, myBoolean);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class OuterComposite {\n");
    
    sb.append("    myNumber: ").append(toIndentedString(myNumber)).append("\n");
    sb.append("    myString: ").append(toIndentedString(myString)).append("\n");
    sb.append("    myBoolean: ").append(toIndentedString(myBoolean)).append("\n");
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

  public static Builder builder() {
    return new Builder();
  }

  public static class Builder {
    private BigDecimal myNumber;
    private String myString;
    private Boolean myBoolean;

    /**
      **/
    public Builder myNumber(BigDecimal myNumber) {
      this.myNumber = myNumber;
      return this;
    }
    /**
      **/
    public Builder myString(String myString) {
      this.myString = myString;
      return this;
    }
    /**
      **/
    public Builder myBoolean(Boolean myBoolean) {
      this.myBoolean = myBoolean;
      return this;
    }

    public OuterComposite build() {
      return new OuterComposite(myNumber, myString, myBoolean);
    }
  }
}

