package org.openapitools.model;

import java.math.BigDecimal;
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
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;


public class OuterComposite  {
  
  @ApiModelProperty(value = "")
  @Valid
  private BigDecimal myNumber;

  @ApiModelProperty(value = "")
  private String myString;

  @ApiModelProperty(value = "")
  private Boolean myBoolean;
 /**
  * Get myNumber
  * @return myNumber
  */
  @JsonProperty("my_number")
  public BigDecimal getMyNumber() {
    return myNumber;
  }

  /**
   * Sets the <code>myNumber</code> property.
   */
  public void setMyNumber(BigDecimal myNumber) {
    this.myNumber = myNumber;
  }

  /**
   * Sets the <code>myNumber</code> property.
   */
  public OuterComposite myNumber(BigDecimal myNumber) {
    this.myNumber = myNumber;
    return this;
  }

 /**
  * Get myString
  * @return myString
  */
  @JsonProperty("my_string")
  public String getMyString() {
    return myString;
  }

  /**
   * Sets the <code>myString</code> property.
   */
  public void setMyString(String myString) {
    this.myString = myString;
  }

  /**
   * Sets the <code>myString</code> property.
   */
  public OuterComposite myString(String myString) {
    this.myString = myString;
    return this;
  }

 /**
  * Get myBoolean
  * @return myBoolean
  */
  @JsonProperty("my_boolean")
  public Boolean getMyBoolean() {
    return myBoolean;
  }

  /**
   * Sets the <code>myBoolean</code> property.
   */
  public void setMyBoolean(Boolean myBoolean) {
    this.myBoolean = myBoolean;
  }

  /**
   * Sets the <code>myBoolean</code> property.
   */
  public OuterComposite myBoolean(Boolean myBoolean) {
    this.myBoolean = myBoolean;
    return this;
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
  private static String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

