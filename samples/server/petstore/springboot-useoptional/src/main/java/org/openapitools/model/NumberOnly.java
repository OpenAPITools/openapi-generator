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
 * NumberOnly
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.14.0-SNAPSHOT")
public class NumberOnly {

  private Optional<BigDecimal> justNumber = Optional.empty();

  public NumberOnly justNumber(BigDecimal justNumber) {
    this.justNumber = Optional.ofNullable(justNumber);
    return this;
  }

  /**
   * Get justNumber
   * @return justNumber
   */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("JustNumber")
  public Optional<BigDecimal> getJustNumber() {
    return justNumber;
  }

  public void setJustNumber(Optional<BigDecimal> justNumber) {
    this.justNumber = justNumber;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    NumberOnly numberOnly = (NumberOnly) o;
    return Objects.equals(this.justNumber, numberOnly.justNumber);
  }

  @Override
  public int hashCode() {
    return Objects.hash(justNumber);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class NumberOnly {\n");
    sb.append("    justNumber: ").append(toIndentedString(justNumber)).append("\n");
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

    private NumberOnly instance;

    public Builder() {
      this(new NumberOnly());
    }

    protected Builder(NumberOnly instance) {
      this.instance = instance;
    }

    protected Builder copyOf(NumberOnly value) { 
      this.instance.setJustNumber(value.justNumber);
      return this;
    }

    public NumberOnly.Builder justNumber(BigDecimal justNumber) {
      this.instance.justNumber(justNumber);
      return this;
    }
    
    /**
    * returns a built NumberOnly instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public NumberOnly build() {
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
  public static NumberOnly.Builder builder() {
    return new NumberOnly.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public NumberOnly.Builder toBuilder() {
    NumberOnly.Builder builder = new NumberOnly.Builder();
    return builder.copyOf(this);
  }

}

