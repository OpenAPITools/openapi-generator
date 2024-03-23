package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * ArrayOfArrayOfNumberOnlyDto
 */

@JsonTypeName("ArrayOfArrayOfNumberOnly")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.5.0-SNAPSHOT")
public class ArrayOfArrayOfNumberOnlyDto {

  @Valid
  private List<List<BigDecimal>> arrayArrayNumber = new ArrayList<>();

  public ArrayOfArrayOfNumberOnlyDto arrayArrayNumber(List<List<BigDecimal>> arrayArrayNumber) {
    this.arrayArrayNumber = arrayArrayNumber;
    return this;
  }

  public ArrayOfArrayOfNumberOnlyDto addArrayArrayNumberItem(List<BigDecimal> arrayArrayNumberItem) {
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
    ArrayOfArrayOfNumberOnlyDto arrayOfArrayOfNumberOnly = (ArrayOfArrayOfNumberOnlyDto) o;
    return Objects.equals(this.arrayArrayNumber, arrayOfArrayOfNumberOnly.arrayArrayNumber);
  }

  @Override
  public int hashCode() {
    return Objects.hash(arrayArrayNumber);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ArrayOfArrayOfNumberOnlyDto {\n");
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

    private ArrayOfArrayOfNumberOnlyDto instance;

    public Builder() {
      this(new ArrayOfArrayOfNumberOnlyDto());
    }

    protected Builder(ArrayOfArrayOfNumberOnlyDto instance) {
      this.instance = instance;
    }

    public ArrayOfArrayOfNumberOnlyDto.Builder arrayArrayNumber(List<List<BigDecimal>> arrayArrayNumber) {
      this.instance.arrayArrayNumber(arrayArrayNumber);
      return this;
    }
    
    /**
    * returns a built ArrayOfArrayOfNumberOnlyDto instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public ArrayOfArrayOfNumberOnlyDto build() {
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
  public static ArrayOfArrayOfNumberOnlyDto.Builder builder() {
    return new ArrayOfArrayOfNumberOnlyDto.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public ArrayOfArrayOfNumberOnlyDto.Builder toBuilder() {
    ArrayOfArrayOfNumberOnlyDto.Builder builder = new ArrayOfArrayOfNumberOnlyDto.Builder();
    builder.instance.setArrayArrayNumber(arrayArrayNumber);
    return builder;
  }

}

