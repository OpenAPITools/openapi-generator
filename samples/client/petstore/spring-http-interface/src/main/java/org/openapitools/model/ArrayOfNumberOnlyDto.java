package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.constraints.NotNull;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * ArrayOfNumberOnlyDto
 */

@JsonTypeName("ArrayOfNumberOnly")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class ArrayOfNumberOnlyDto {

  
  private List<BigDecimal> arrayNumber;

  public ArrayOfNumberOnlyDto arrayNumber(List<BigDecimal> arrayNumber) {
    this.arrayNumber = arrayNumber;
    return this;
  }

  public ArrayOfNumberOnlyDto addArrayNumberItem(BigDecimal arrayNumberItem) {
    if (this.arrayNumber == null) {
      this.arrayNumber = new ArrayList<>();
    }
    this.arrayNumber.add(arrayNumberItem);
    return this;
  }

  /**
   * Get arrayNumber
   * @return arrayNumber
  */
  
  @JsonProperty("ArrayNumber")
  public List<BigDecimal> getArrayNumber() {
    return arrayNumber;
  }

  public void setArrayNumber(List<BigDecimal> arrayNumber) {
    this.arrayNumber = arrayNumber;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ArrayOfNumberOnlyDto arrayOfNumberOnly = (ArrayOfNumberOnlyDto) o;
    return Objects.equals(this.arrayNumber, arrayOfNumberOnly.arrayNumber);
  }

  @Override
  public int hashCode() {
    return Objects.hash(arrayNumber);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ArrayOfNumberOnlyDto {\n");
    sb.append("    arrayNumber: ").append(toIndentedString(arrayNumber)).append("\n");
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

    private ArrayOfNumberOnlyDto instance;

    public Builder() {
      this(new ArrayOfNumberOnlyDto());
    }

    protected Builder(ArrayOfNumberOnlyDto instance) {
      this.instance = instance;
    }

    public ArrayOfNumberOnlyDto.Builder arrayNumber(List<BigDecimal> arrayNumber) {
      this.instance.arrayNumber(arrayNumber);
      return this;
    }
    /**
    * returns a built ArrayOfNumberOnlyDto instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public ArrayOfNumberOnlyDto build() {
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
  public static ArrayOfNumberOnlyDto.Builder builder() {
    return new ArrayOfNumberOnlyDto.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public ArrayOfNumberOnlyDto.Builder toBuilder() {
    ArrayOfNumberOnlyDto.Builder builder = new ArrayOfNumberOnlyDto.Builder();
    builder.instance.setArrayNumber(arrayNumber);
    return builder;
  }

}

