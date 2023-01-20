package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.math.BigDecimal;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * OuterCompositeDto
 */

@JsonTypeName("OuterComposite")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class OuterCompositeDto {

  @JsonProperty("my_number")
  private BigDecimal myNumber;

  @JsonProperty("my_string")
  private String myString;

  @JsonProperty("my_boolean")
  private Boolean myBoolean;

  public OuterCompositeDto myNumber(BigDecimal myNumber) {
    this.myNumber = myNumber;
    return this;
  }

  /**
   * Get myNumber
   * @return myNumber
  */
  @Valid 
  @ApiModelProperty(value = "")
  public BigDecimal getMyNumber() {
    return myNumber;
  }

  public void setMyNumber(BigDecimal myNumber) {
    this.myNumber = myNumber;
  }

  public OuterCompositeDto myString(String myString) {
    this.myString = myString;
    return this;
  }

  /**
   * Get myString
   * @return myString
  */
  
  @ApiModelProperty(value = "")
  public String getMyString() {
    return myString;
  }

  public void setMyString(String myString) {
    this.myString = myString;
  }

  public OuterCompositeDto myBoolean(Boolean myBoolean) {
    this.myBoolean = myBoolean;
    return this;
  }

  /**
   * Get myBoolean
   * @return myBoolean
  */
  
  @ApiModelProperty(value = "")
  public Boolean getMyBoolean() {
    return myBoolean;
  }

  public void setMyBoolean(Boolean myBoolean) {
    this.myBoolean = myBoolean;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    OuterCompositeDto outerComposite = (OuterCompositeDto) o;
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
    sb.append("class OuterCompositeDto {\n");
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
  private String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

