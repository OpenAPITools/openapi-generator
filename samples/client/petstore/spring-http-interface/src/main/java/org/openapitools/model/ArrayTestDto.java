package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.openapitools.model.ReadOnlyFirstDto;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.constraints.NotNull;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * ArrayTestDto
 */

@JsonTypeName("ArrayTest")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class ArrayTestDto {

  
  private List<String> arrayOfString;

  
  private List<List> arrayArrayOfInteger;

  
  private List<List> arrayArrayOfModel;

  public ArrayTestDto arrayOfString(List<String> arrayOfString) {
    this.arrayOfString = arrayOfString;
    return this;
  }

  public ArrayTestDto addArrayOfStringItem(String arrayOfStringItem) {
    if (this.arrayOfString == null) {
      this.arrayOfString = new ArrayList<>();
    }
    this.arrayOfString.add(arrayOfStringItem);
    return this;
  }

  /**
   * Get arrayOfString
   * @return arrayOfString
  */
  
  @JsonProperty("array_of_string")
  public List<String> getArrayOfString() {
    return arrayOfString;
  }

  public void setArrayOfString(List<String> arrayOfString) {
    this.arrayOfString = arrayOfString;
  }

  public ArrayTestDto arrayArrayOfInteger(List<List> arrayArrayOfInteger) {
    this.arrayArrayOfInteger = arrayArrayOfInteger;
    return this;
  }

  public ArrayTestDto addArrayArrayOfIntegerItem(List<Long> arrayArrayOfIntegerItem) {
    if (this.arrayArrayOfInteger == null) {
      this.arrayArrayOfInteger = new ArrayList<>();
    }
    this.arrayArrayOfInteger.add(arrayArrayOfIntegerItem);
    return this;
  }

  /**
   * Get arrayArrayOfInteger
   * @return arrayArrayOfInteger
  */
  
  @JsonProperty("array_array_of_integer")
  public List<List> getArrayArrayOfInteger() {
    return arrayArrayOfInteger;
  }

  public void setArrayArrayOfInteger(List<List> arrayArrayOfInteger) {
    this.arrayArrayOfInteger = arrayArrayOfInteger;
  }

  public ArrayTestDto arrayArrayOfModel(List<List> arrayArrayOfModel) {
    this.arrayArrayOfModel = arrayArrayOfModel;
    return this;
  }

  public ArrayTestDto addArrayArrayOfModelItem(List<ReadOnlyFirstDto> arrayArrayOfModelItem) {
    if (this.arrayArrayOfModel == null) {
      this.arrayArrayOfModel = new ArrayList<>();
    }
    this.arrayArrayOfModel.add(arrayArrayOfModelItem);
    return this;
  }

  /**
   * Get arrayArrayOfModel
   * @return arrayArrayOfModel
  */
  
  @JsonProperty("array_array_of_model")
  public List<List> getArrayArrayOfModel() {
    return arrayArrayOfModel;
  }

  public void setArrayArrayOfModel(List<List> arrayArrayOfModel) {
    this.arrayArrayOfModel = arrayArrayOfModel;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ArrayTestDto arrayTest = (ArrayTestDto) o;
    return Objects.equals(this.arrayOfString, arrayTest.arrayOfString) &&
        Objects.equals(this.arrayArrayOfInteger, arrayTest.arrayArrayOfInteger) &&
        Objects.equals(this.arrayArrayOfModel, arrayTest.arrayArrayOfModel);
  }

  @Override
  public int hashCode() {
    return Objects.hash(arrayOfString, arrayArrayOfInteger, arrayArrayOfModel);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ArrayTestDto {\n");
    sb.append("    arrayOfString: ").append(toIndentedString(arrayOfString)).append("\n");
    sb.append("    arrayArrayOfInteger: ").append(toIndentedString(arrayArrayOfInteger)).append("\n");
    sb.append("    arrayArrayOfModel: ").append(toIndentedString(arrayArrayOfModel)).append("\n");
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

    private ArrayTestDto instance;

    public Builder() {
      this(new ArrayTestDto());
    }

    protected Builder(ArrayTestDto instance) {
      this.instance = instance;
    }

    public ArrayTestDto.Builder arrayOfString(List<String> arrayOfString) {
      this.instance.arrayOfString(arrayOfString);
      return this;
    }
    public ArrayTestDto.Builder arrayArrayOfInteger(List<List> arrayArrayOfInteger) {
      this.instance.arrayArrayOfInteger(arrayArrayOfInteger);
      return this;
    }
    public ArrayTestDto.Builder arrayArrayOfModel(List<List> arrayArrayOfModel) {
      this.instance.arrayArrayOfModel(arrayArrayOfModel);
      return this;
    }
    /**
    * returns a built ArrayTestDto instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public ArrayTestDto build() {
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
  public static ArrayTestDto.Builder builder() {
    return new ArrayTestDto.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public ArrayTestDto.Builder toBuilder() {
    ArrayTestDto.Builder builder = new ArrayTestDto.Builder();
    builder.instance.setArrayOfString(arrayOfString);
    builder.instance.setArrayArrayOfInteger(arrayArrayOfInteger);
    builder.instance.setArrayArrayOfModel(arrayArrayOfModel);
    return builder;
  }

}

