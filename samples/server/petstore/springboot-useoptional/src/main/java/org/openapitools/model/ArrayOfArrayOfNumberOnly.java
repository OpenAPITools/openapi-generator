package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * ArrayOfArrayOfNumberOnly
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
public class ArrayOfArrayOfNumberOnly {

  @Valid
  private List<List<BigDecimal>> arrayArrayNumber = new ArrayList<>();

  public ArrayOfArrayOfNumberOnly arrayArrayNumber(List<List<BigDecimal>> arrayArrayNumber) {
    this.arrayArrayNumber = arrayArrayNumber;
    return this;
  }

  public ArrayOfArrayOfNumberOnly addArrayArrayNumberItem(List<BigDecimal> arrayArrayNumberItem) {
    if (this.arrayArrayNumber == null) {
      this.arrayArrayNumber = new ArrayList<>();
    }
    this.arrayArrayNumber.add(arrayArrayNumberItem);
    return this;
  }

  /**
   * Get arrayArrayNumber
   * @return arrayArrayNumber
   */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("ArrayArrayNumber")
  public List<List<BigDecimal>> getArrayArrayNumber() {
    return arrayArrayNumber;
  }

  public void setArrayArrayNumber(List<List<BigDecimal>> arrayArrayNumber) {
    this.arrayArrayNumber = arrayArrayNumber;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ArrayOfArrayOfNumberOnly arrayOfArrayOfNumberOnly = (ArrayOfArrayOfNumberOnly) o;
    return Objects.equals(this.arrayArrayNumber, arrayOfArrayOfNumberOnly.arrayArrayNumber);
  }

  @Override
  public int hashCode() {
    return Objects.hash(arrayArrayNumber);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ArrayOfArrayOfNumberOnly {\n");
    sb.append("    arrayArrayNumber: ").append(toIndentedString(arrayArrayNumber)).append("\n");
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

    private ArrayOfArrayOfNumberOnly instance;

    public Builder() {
      this(new ArrayOfArrayOfNumberOnly());
    }

    protected Builder(ArrayOfArrayOfNumberOnly instance) {
      this.instance = instance;
    }

    protected Builder copyOf(ArrayOfArrayOfNumberOnly value) { 
      this.instance.setArrayArrayNumber(value.arrayArrayNumber);
      return this;
    }

    public ArrayOfArrayOfNumberOnly.Builder arrayArrayNumber(List<List<BigDecimal>> arrayArrayNumber) {
      this.instance.arrayArrayNumber(arrayArrayNumber);
      return this;
    }
    
    /**
    * returns a built ArrayOfArrayOfNumberOnly instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public ArrayOfArrayOfNumberOnly build() {
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
  public static ArrayOfArrayOfNumberOnly.Builder builder() {
    return new ArrayOfArrayOfNumberOnly.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public ArrayOfArrayOfNumberOnly.Builder toBuilder() {
    ArrayOfArrayOfNumberOnly.Builder builder = new ArrayOfArrayOfNumberOnly.Builder();
    return builder.copyOf(this);
  }

}

