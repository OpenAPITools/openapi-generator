package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.openapitools.model.ReadOnlyFirst;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * ArrayTest
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
public class ArrayTest {

  @Valid
  private List<String> arrayOfString = new ArrayList<>();

  @Valid
  private List<List<Long>> arrayArrayOfInteger = new ArrayList<>();

  @Valid
  private List<List<@Valid ReadOnlyFirst>> arrayArrayOfModel = new ArrayList<>();

  public ArrayTest arrayOfString(List<String> arrayOfString) {
    this.arrayOfString = arrayOfString;
    return this;
  }

  public ArrayTest addArrayOfStringItem(String arrayOfStringItem) {
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
  
  @ApiModelProperty(value = "")
  @JsonProperty("array_of_string")
  public List<String> getArrayOfString() {
    return arrayOfString;
  }

  public void setArrayOfString(List<String> arrayOfString) {
    this.arrayOfString = arrayOfString;
  }

  public ArrayTest arrayArrayOfInteger(List<List<Long>> arrayArrayOfInteger) {
    this.arrayArrayOfInteger = arrayArrayOfInteger;
    return this;
  }

  public ArrayTest addArrayArrayOfIntegerItem(List<Long> arrayArrayOfIntegerItem) {
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
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("array_array_of_integer")
  public List<List<Long>> getArrayArrayOfInteger() {
    return arrayArrayOfInteger;
  }

  public void setArrayArrayOfInteger(List<List<Long>> arrayArrayOfInteger) {
    this.arrayArrayOfInteger = arrayArrayOfInteger;
  }

  public ArrayTest arrayArrayOfModel(List<List<@Valid ReadOnlyFirst>> arrayArrayOfModel) {
    this.arrayArrayOfModel = arrayArrayOfModel;
    return this;
  }

  public ArrayTest addArrayArrayOfModelItem(List<@Valid ReadOnlyFirst> arrayArrayOfModelItem) {
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
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("array_array_of_model")
  public List<List<@Valid ReadOnlyFirst>> getArrayArrayOfModel() {
    return arrayArrayOfModel;
  }

  public void setArrayArrayOfModel(List<List<@Valid ReadOnlyFirst>> arrayArrayOfModel) {
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
    ArrayTest arrayTest = (ArrayTest) o;
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
    sb.append("class ArrayTest {\n");
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

    private ArrayTest instance;

    public Builder() {
      this(new ArrayTest());
    }

    protected Builder(ArrayTest instance) {
      this.instance = instance;
    }

    protected Builder copyOf(ArrayTest value) { 
      this.instance.setArrayOfString(value.arrayOfString);
      this.instance.setArrayArrayOfInteger(value.arrayArrayOfInteger);
      this.instance.setArrayArrayOfModel(value.arrayArrayOfModel);
      return this;
    }

    public ArrayTest.Builder arrayOfString(List<String> arrayOfString) {
      this.instance.arrayOfString(arrayOfString);
      return this;
    }
    
    public ArrayTest.Builder arrayArrayOfInteger(List<List<Long>> arrayArrayOfInteger) {
      this.instance.arrayArrayOfInteger(arrayArrayOfInteger);
      return this;
    }
    
    public ArrayTest.Builder arrayArrayOfModel(List<List<ReadOnlyFirst>> arrayArrayOfModel) {
      this.instance.arrayArrayOfModel(arrayArrayOfModel);
      return this;
    }
    
    /**
    * returns a built ArrayTest instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public ArrayTest build() {
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
  public static ArrayTest.Builder builder() {
    return new ArrayTest.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public ArrayTest.Builder toBuilder() {
    ArrayTest.Builder builder = new ArrayTest.Builder();
    return builder.copyOf(this);
  }

}

