package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
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
 * OuterComposite
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.6.0-SNAPSHOT")
public class OuterComposite {

  private Optional<BigDecimal> myNumber = Optional.empty();

  private Optional<String> myString = Optional.empty();

  private Optional<Boolean> myBoolean = Optional.empty();

  public OuterComposite myNumber(BigDecimal myNumber) {
    this.myNumber = Optional.of(myNumber);
    return this;
  }

  /**
   * Get myNumber
   * @return myNumber
  */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("my_number")
  public Optional<BigDecimal> getMyNumber() {
    return myNumber;
  }

  public void setMyNumber(Optional<BigDecimal> myNumber) {
    this.myNumber = myNumber;
  }

  public OuterComposite myString(String myString) {
    this.myString = Optional.of(myString);
    return this;
  }

  /**
   * Get myString
   * @return myString
  */
  
  @ApiModelProperty(value = "")
  @JsonProperty("my_string")
  public Optional<String> getMyString() {
    return myString;
  }

  public void setMyString(Optional<String> myString) {
    this.myString = myString;
  }

  public OuterComposite myBoolean(Boolean myBoolean) {
    this.myBoolean = Optional.of(myBoolean);
    return this;
  }

  /**
   * Get myBoolean
   * @return myBoolean
  */
  
  @ApiModelProperty(value = "")
  @JsonProperty("my_boolean")
  public Optional<Boolean> getMyBoolean() {
    return myBoolean;
  }

  public void setMyBoolean(Optional<Boolean> myBoolean) {
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
  private String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

