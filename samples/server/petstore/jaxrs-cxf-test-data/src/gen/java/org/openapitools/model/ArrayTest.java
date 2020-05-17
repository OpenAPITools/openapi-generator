package org.openapitools.model;

import java.util.ArrayList;
import java.util.List;
import org.openapitools.model.ReadOnlyFirst;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.ApiModelProperty;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;

public class ArrayTest  {
  
  @ApiModelProperty(value = "")
  private List<String> arrayOfString = null;

  @ApiModelProperty(value = "")
  @Valid
  private List<List<Long>> arrayArrayOfInteger = null;

  @ApiModelProperty(value = "")
  @Valid
  private List<List<ReadOnlyFirst>> arrayArrayOfModel = null;
 /**
  * Get arrayOfString
  * @return arrayOfString
  */
  @JsonProperty("array_of_string")
  public List<String> getArrayOfString() {
    return arrayOfString;
  }

  /**
   * Sets the <code>arrayOfString</code> property.
   */
  public void setArrayOfString(List<String> arrayOfString) {
    this.arrayOfString = arrayOfString;
  }

  /**
   * Sets the <code>arrayOfString</code> property.
   */
  public ArrayTest arrayOfString(List<String> arrayOfString) {
    this.arrayOfString = arrayOfString;
    return this;
  }

  /**
   * Adds a new item to the <code>arrayOfString</code> list.
   */
  public ArrayTest addArrayOfStringItem(String arrayOfStringItem) {
    this.arrayOfString.add(arrayOfStringItem);
    return this;
  }

 /**
  * Get arrayArrayOfInteger
  * @return arrayArrayOfInteger
  */
  @JsonProperty("array_array_of_integer")
  public List<List<Long>> getArrayArrayOfInteger() {
    return arrayArrayOfInteger;
  }

  /**
   * Sets the <code>arrayArrayOfInteger</code> property.
   */
  public void setArrayArrayOfInteger(List<List<Long>> arrayArrayOfInteger) {
    this.arrayArrayOfInteger = arrayArrayOfInteger;
  }

  /**
   * Sets the <code>arrayArrayOfInteger</code> property.
   */
  public ArrayTest arrayArrayOfInteger(List<List<Long>> arrayArrayOfInteger) {
    this.arrayArrayOfInteger = arrayArrayOfInteger;
    return this;
  }

  /**
   * Adds a new item to the <code>arrayArrayOfInteger</code> list.
   */
  public ArrayTest addArrayArrayOfIntegerItem(List<Long> arrayArrayOfIntegerItem) {
    this.arrayArrayOfInteger.add(arrayArrayOfIntegerItem);
    return this;
  }

 /**
  * Get arrayArrayOfModel
  * @return arrayArrayOfModel
  */
  @JsonProperty("array_array_of_model")
  public List<List<ReadOnlyFirst>> getArrayArrayOfModel() {
    return arrayArrayOfModel;
  }

  /**
   * Sets the <code>arrayArrayOfModel</code> property.
   */
  public void setArrayArrayOfModel(List<List<ReadOnlyFirst>> arrayArrayOfModel) {
    this.arrayArrayOfModel = arrayArrayOfModel;
  }

  /**
   * Sets the <code>arrayArrayOfModel</code> property.
   */
  public ArrayTest arrayArrayOfModel(List<List<ReadOnlyFirst>> arrayArrayOfModel) {
    this.arrayArrayOfModel = arrayArrayOfModel;
    return this;
  }

  /**
   * Adds a new item to the <code>arrayArrayOfModel</code> list.
   */
  public ArrayTest addArrayArrayOfModelItem(List<ReadOnlyFirst> arrayArrayOfModelItem) {
    this.arrayArrayOfModel.add(arrayArrayOfModelItem);
    return this;
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
  private static String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

