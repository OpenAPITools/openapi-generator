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
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.6.0-SNAPSHOT")
public class OuterCompositeDto {

  private BigDecimal myNumber;

  private String myString;

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
  @JsonProperty("my_number")
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
  @JsonProperty("my_string")
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
  
  public static class Builder {

    private OuterCompositeDto instance;

    public Builder() {
      this(new OuterCompositeDto());
    }

    protected Builder(OuterCompositeDto instance) {
      this.instance = instance;
    }

    protected Builder copyOf(OuterCompositeDto value) { 
      this.instance.setMyNumber(value.myNumber);
      this.instance.setMyString(value.myString);
      this.instance.setMyBoolean(value.myBoolean);
      return this;
    }

    public OuterCompositeDto.Builder myNumber(BigDecimal myNumber) {
      this.instance.myNumber(myNumber);
      return this;
    }
    
    public OuterCompositeDto.Builder myString(String myString) {
      this.instance.myString(myString);
      return this;
    }
    
    public OuterCompositeDto.Builder myBoolean(Boolean myBoolean) {
      this.instance.myBoolean(myBoolean);
      return this;
    }
    
    /**
    * returns a built OuterCompositeDto instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public OuterCompositeDto build() {
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
  public static OuterCompositeDto.Builder builder() {
    return new OuterCompositeDto.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public OuterCompositeDto.Builder toBuilder() {
    OuterCompositeDto.Builder builder = new OuterCompositeDto.Builder();
    return builder.copyOf(this);
  }

}

