package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import java.math.BigDecimal;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import javax.annotation.Generated;

/**
 * OuterComposite
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class OuterComposite {

  private BigDecimal myNumber;

  private String myString;

  private Boolean myBoolean;

  public OuterComposite myNumber(BigDecimal myNumber) {
    this.myNumber = myNumber;
    return this;
  }

  /**
   * Get myNumber
   * @return myNumber
  */
  @Valid 
  @Schema(name = "my_number", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("my_number")
  public BigDecimal getMyNumber() {
    return myNumber;
  }

  public void setMyNumber(BigDecimal myNumber) {
    this.myNumber = myNumber;
  }

  public OuterComposite myString(String myString) {
    this.myString = myString;
    return this;
  }

  /**
   * Get myString
   * @return myString
  */
  
  @Schema(name = "my_string", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("my_string")
  public String getMyString() {
    return myString;
  }

  public void setMyString(String myString) {
    this.myString = myString;
  }

  public OuterComposite myBoolean(Boolean myBoolean) {
    this.myBoolean = myBoolean;
    return this;
  }

  /**
   * Get myBoolean
   * @return myBoolean
  */
  
  @Schema(name = "my_boolean", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("my_boolean")
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
  * Create a builder with no initialized field.
  */
  public static OuterComposite.Builder builder() {
    return new OuterComposite.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public OuterComposite.Builder toBuilder() {
    OuterComposite.Builder builder = new OuterComposite.Builder();
    builder.instance.setMyNumber(myNumber);
    builder.instance.setMyString(myString);
    builder.instance.setMyBoolean(myBoolean);
    return builder;
  }

}

