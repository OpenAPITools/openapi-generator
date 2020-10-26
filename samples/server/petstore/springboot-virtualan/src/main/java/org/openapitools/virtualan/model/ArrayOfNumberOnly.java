package org.openapitools.virtualan.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.ArrayList;
import java.util.List;
import org.openapitools.virtualan.model.Number;
import org.openapitools.jackson.nullable.JsonNullable;
import javax.validation.Valid;
import javax.validation.constraints.*;

/**
 * ArrayOfNumberOnly
 */
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class ArrayOfNumberOnly   {
  @JsonProperty("ArrayNumber")
  @Valid
  private List<Number> arrayNumber = null;

  public ArrayOfNumberOnly arrayNumber(List<Number> arrayNumber) {
    this.arrayNumber = arrayNumber;
    return this;
  }

  public ArrayOfNumberOnly addArrayNumberItem(Number arrayNumberItem) {
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
  @ApiModelProperty(value = "")

  @Valid

  public List<Number> getArrayNumber() {
    return arrayNumber;
  }

  public void setArrayNumber(List<Number> arrayNumber) {
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
    ArrayOfNumberOnly arrayOfNumberOnly = (ArrayOfNumberOnly) o;
    return Objects.equals(this.arrayNumber, arrayOfNumberOnly.arrayNumber);
  }

  @Override
  public int hashCode() {
    return Objects.hash(arrayNumber);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ArrayOfNumberOnly {\n");
    
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
}

