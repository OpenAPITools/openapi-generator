package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.math.BigDecimal;
import java.util.Optional;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * OuterComposite
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class OuterComposite   {

  @JsonProperty("my_number")
  private Optional<BigDecimal> myNumber = Optional.empty();

  @JsonProperty("my_string")
  private Optional<String> myString = Optional.empty();

  @JsonProperty("my_boolean")
  private Optional<Boolean> myBoolean = Optional.empty();

  public OuterComposite myNumber(BigDecimal myNumber) {
    this.myNumber = Optional.ofNullable(myNumber);
    return this;
  }

  /**
   * Get myNumber
   * @return myNumber
  */
  @ApiModelProperty(value = "")
  public Optional<BigDecimal> getMyNumber() {
    return myNumber;
  }

  @JsonIgnore
  public void setMyNumber(BigDecimal myNumber) {
    this.myNumber = Optional.ofNullable(myNumber);
  }

  public OuterComposite myString(String myString) {
    this.myString = Optional.ofNullable(myString);
    return this;
  }

  /**
   * Get myString
   * @return myString
  */
  @ApiModelProperty(value = "")
  public Optional<String> getMyString() {
    return myString;
  }

  @JsonIgnore
  public void setMyString(String myString) {
    this.myString = Optional.ofNullable(myString);
  }

  public OuterComposite myBoolean(Boolean myBoolean) {
    this.myBoolean = Optional.ofNullable(myBoolean);
    return this;
  }

  /**
   * Get myBoolean
   * @return myBoolean
  */
  @ApiModelProperty(value = "")
  public Optional<Boolean> getMyBoolean() {
    return myBoolean;
  }

  @JsonIgnore
  public void setMyBoolean(Boolean myBoolean) {
    this.myBoolean = Optional.ofNullable(myBoolean);
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

