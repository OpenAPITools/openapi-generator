package apimodels;

import java.util.Objects;
import apimodels.ReadOnlyFirst;
import java.util.ArrayList;
import java.util.List;
import javax.validation.constraints.*;
import com.fasterxml.jackson.annotation.*;
/**
 * ArrayTest
 */

public class ArrayTest   {
  @JsonProperty("array_of_string")
  private List<String> arrayOfString = null;

  @JsonProperty("array_array_of_integer")
  private List<List<Long>> arrayArrayOfInteger = null;

  @JsonProperty("array_array_of_model")
  private List<List<ReadOnlyFirst>> arrayArrayOfModel = null;

  public ArrayTest arrayOfString(List<String> arrayOfString) {
    this.arrayOfString = arrayOfString;
    return this;
  }

  public ArrayTest addArrayOfStringItem(String arrayOfStringItem) {
    if (this.arrayOfString == null) {
      this.arrayOfString = new ArrayList<String>();
    }
    this.arrayOfString.add(arrayOfStringItem);
    return this;
  }

   /**
   * Get arrayOfString
   * @return arrayOfString
  **/
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
      this.arrayArrayOfInteger = new ArrayList<List<Long>>();
    }
    this.arrayArrayOfInteger.add(arrayArrayOfIntegerItem);
    return this;
  }

   /**
   * Get arrayArrayOfInteger
   * @return arrayArrayOfInteger
  **/
    public List<List<Long>> getArrayArrayOfInteger() {
    return arrayArrayOfInteger;
  }

  public void setArrayArrayOfInteger(List<List<Long>> arrayArrayOfInteger) {
    this.arrayArrayOfInteger = arrayArrayOfInteger;
  }

  public ArrayTest arrayArrayOfModel(List<List<ReadOnlyFirst>> arrayArrayOfModel) {
    this.arrayArrayOfModel = arrayArrayOfModel;
    return this;
  }

  public ArrayTest addArrayArrayOfModelItem(List<ReadOnlyFirst> arrayArrayOfModelItem) {
    if (this.arrayArrayOfModel == null) {
      this.arrayArrayOfModel = new ArrayList<List<ReadOnlyFirst>>();
    }
    this.arrayArrayOfModel.add(arrayArrayOfModelItem);
    return this;
  }

   /**
   * Get arrayArrayOfModel
   * @return arrayArrayOfModel
  **/
    public List<List<ReadOnlyFirst>> getArrayArrayOfModel() {
    return arrayArrayOfModel;
  }

  public void setArrayArrayOfModel(List<List<ReadOnlyFirst>> arrayArrayOfModel) {
    this.arrayArrayOfModel = arrayArrayOfModel;
  }


  @Override
  public boolean equals(java.lang.Object o) {
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
  private String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

