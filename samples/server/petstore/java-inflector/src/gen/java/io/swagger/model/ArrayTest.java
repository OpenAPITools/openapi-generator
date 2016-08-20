package io.swagger.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import io.swagger.model.ReadOnlyFirst;
import java.util.ArrayList;
import java.util.List;





@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaInflectorServerCodegen", date = "2016-08-20T17:24:26.037+08:00")
public class ArrayTest   {
  @JsonProperty("array_of_string")
  private List<String> arrayOfString = new ArrayList<String>();

  @JsonProperty("array_array_of_integer")
  private List<List<Long>> arrayArrayOfInteger = new ArrayList<List<Long>>();

  @JsonProperty("array_array_of_model")
  private List<List<ReadOnlyFirst>> arrayArrayOfModel = new ArrayList<List<ReadOnlyFirst>>();

  /**
   **/
  public ArrayTest arrayOfString(List<String> arrayOfString) {
    this.arrayOfString = arrayOfString;
    return this;
  }

  
  @ApiModelProperty(example = "null", value = "")
  @JsonProperty("array_of_string")
  public List<String> getArrayOfString() {
    return arrayOfString;
  }
  public void setArrayOfString(List<String> arrayOfString) {
    this.arrayOfString = arrayOfString;
  }

  /**
   **/
  public ArrayTest arrayArrayOfInteger(List<List<Long>> arrayArrayOfInteger) {
    this.arrayArrayOfInteger = arrayArrayOfInteger;
    return this;
  }

  
  @ApiModelProperty(example = "null", value = "")
  @JsonProperty("array_array_of_integer")
  public List<List<Long>> getArrayArrayOfInteger() {
    return arrayArrayOfInteger;
  }
  public void setArrayArrayOfInteger(List<List<Long>> arrayArrayOfInteger) {
    this.arrayArrayOfInteger = arrayArrayOfInteger;
  }

  /**
   **/
  public ArrayTest arrayArrayOfModel(List<List<ReadOnlyFirst>> arrayArrayOfModel) {
    this.arrayArrayOfModel = arrayArrayOfModel;
    return this;
  }

  
  @ApiModelProperty(example = "null", value = "")
  @JsonProperty("array_array_of_model")
  public List<List<ReadOnlyFirst>> getArrayArrayOfModel() {
    return arrayArrayOfModel;
  }
  public void setArrayArrayOfModel(List<List<ReadOnlyFirst>> arrayArrayOfModel) {
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
    return Objects.equals(arrayOfString, arrayTest.arrayOfString) &&
        Objects.equals(arrayArrayOfInteger, arrayTest.arrayArrayOfInteger) &&
        Objects.equals(arrayArrayOfModel, arrayTest.arrayArrayOfModel);
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
}

