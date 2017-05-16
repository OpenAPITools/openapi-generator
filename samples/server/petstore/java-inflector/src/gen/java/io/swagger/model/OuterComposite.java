package io.swagger.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.math.BigDecimal;






public class OuterComposite   {
  @JsonProperty("my_number")
  private BigDecimal myNumber = null;

  @JsonProperty("my_string")
  private String myString = null;

  @JsonProperty("my_boolean")
  private Boolean myBoolean = null;

  /**
   **/
  public OuterComposite myNumber(BigDecimal myNumber) {
    this.myNumber = myNumber;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("my_number")
  public BigDecimal getMyNumber() {
    return myNumber;
  }
  public void setMyNumber(BigDecimal myNumber) {
    this.myNumber = myNumber;
  }

  /**
   **/
  public OuterComposite myString(String myString) {
    this.myString = myString;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("my_string")
  public String getMyString() {
    return myString;
  }
  public void setMyString(String myString) {
    this.myString = myString;
  }

  /**
   **/
  public OuterComposite myBoolean(Boolean myBoolean) {
    this.myBoolean = myBoolean;
    return this;
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
    return Objects.equals(myNumber, outerComposite.myNumber) &&
        Objects.equals(myString, outerComposite.myString) &&
        Objects.equals(myBoolean, outerComposite.myBoolean);
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
}

