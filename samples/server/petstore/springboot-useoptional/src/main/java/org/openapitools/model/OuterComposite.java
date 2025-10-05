package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.math.BigDecimal;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * OuterComposite
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
public class OuterComposite {

  private Optional<BigDecimal> myNumber = Optional.empty();

  private Optional<String> myString = Optional.empty();

  private Optional<Boolean> myBoolean = Optional.empty();

  public OuterComposite myNumber(BigDecimal myNumber) {
    this.myNumber = Optional.ofNullable(myNumber);
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
    this.myString = Optional.ofNullable(myString);
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
    this.myBoolean = Optional.ofNullable(myBoolean);
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
  
  public static class Builder {

    private OuterComposite instance;

    public Builder() {
      this(new OuterComposite());
    }

    protected Builder(OuterComposite instance) {
      this.instance = instance;
    }

    protected Builder copyOf(OuterComposite value) { 
      this.instance.setMyNumber(value.myNumber);
      this.instance.setMyString(value.myString);
      this.instance.setMyBoolean(value.myBoolean);
      return this;
    }

    public OuterComposite.Builder myNumber(BigDecimal myNumber) {
      this.instance.myNumber(myNumber);
      return this;
    }
    
    public OuterComposite.Builder myString(String myString) {
      this.instance.myString(myString);
      return this;
    }
    
    public OuterComposite.Builder myBoolean(Boolean myBoolean) {
      this.instance.myBoolean(myBoolean);
      return this;
    }
    
    /**
    * returns a built OuterComposite instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public OuterComposite build() {
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
  public static OuterComposite.Builder builder() {
    return new OuterComposite.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public OuterComposite.Builder toBuilder() {
    OuterComposite.Builder builder = new OuterComposite.Builder();
    return builder.copyOf(this);
  }

}

