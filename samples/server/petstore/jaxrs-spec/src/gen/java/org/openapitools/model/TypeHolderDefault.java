package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.joda.time.LocalDate;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

/**
 * a model to test optional properties with server defaults
 **/
@ApiModel(description = "a model to test optional properties with server defaults")
public class TypeHolderDefault  implements Serializable {
  
  private @Valid String stringItem = "what";
  private @Valid Float numberItem = 1.234f;
  private @Valid Integer integerItem = -2;
  private @Valid Boolean boolItem = true;
  private @Valid LocalDate dateItem;
  private @Valid Date datetimeItem;
  private @Valid List<Integer> arrayItem = new ArrayList<Integer>();

  /**
   **/
  public TypeHolderDefault stringItem(String stringItem) {
    this.stringItem = stringItem;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("string_item")
  public String getStringItem() {
    return stringItem;
  }
  public void setStringItem(String stringItem) {
    this.stringItem = stringItem;
  }

  /**
   **/
  public TypeHolderDefault numberItem(Float numberItem) {
    this.numberItem = numberItem;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("number_item")
  public Float getNumberItem() {
    return numberItem;
  }
  public void setNumberItem(Float numberItem) {
    this.numberItem = numberItem;
  }

  /**
   **/
  public TypeHolderDefault integerItem(Integer integerItem) {
    this.integerItem = integerItem;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("integer_item")
  public Integer getIntegerItem() {
    return integerItem;
  }
  public void setIntegerItem(Integer integerItem) {
    this.integerItem = integerItem;
  }

  /**
   **/
  public TypeHolderDefault boolItem(Boolean boolItem) {
    this.boolItem = boolItem;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("bool_item")
  public Boolean getBoolItem() {
    return boolItem;
  }
  public void setBoolItem(Boolean boolItem) {
    this.boolItem = boolItem;
  }

  /**
   **/
  public TypeHolderDefault dateItem(LocalDate dateItem) {
    this.dateItem = dateItem;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("date_item")
  public LocalDate getDateItem() {
    return dateItem;
  }
  public void setDateItem(LocalDate dateItem) {
    this.dateItem = dateItem;
  }

  /**
   **/
  public TypeHolderDefault datetimeItem(Date datetimeItem) {
    this.datetimeItem = datetimeItem;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("datetime_item")
  public Date getDatetimeItem() {
    return datetimeItem;
  }
  public void setDatetimeItem(Date datetimeItem) {
    this.datetimeItem = datetimeItem;
  }

  /**
   **/
  public TypeHolderDefault arrayItem(List<Integer> arrayItem) {
    this.arrayItem = arrayItem;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("array_item")
  public List<Integer> getArrayItem() {
    return arrayItem;
  }
  public void setArrayItem(List<Integer> arrayItem) {
    this.arrayItem = arrayItem;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    TypeHolderDefault typeHolderDefault = (TypeHolderDefault) o;
    return Objects.equals(stringItem, typeHolderDefault.stringItem) &&
        Objects.equals(numberItem, typeHolderDefault.numberItem) &&
        Objects.equals(integerItem, typeHolderDefault.integerItem) &&
        Objects.equals(boolItem, typeHolderDefault.boolItem) &&
        Objects.equals(dateItem, typeHolderDefault.dateItem) &&
        Objects.equals(datetimeItem, typeHolderDefault.datetimeItem) &&
        Objects.equals(arrayItem, typeHolderDefault.arrayItem);
  }

  @Override
  public int hashCode() {
    return Objects.hash(stringItem, numberItem, integerItem, boolItem, dateItem, datetimeItem, arrayItem);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class TypeHolderDefault {\n");
    
    sb.append("    stringItem: ").append(toIndentedString(stringItem)).append("\n");
    sb.append("    numberItem: ").append(toIndentedString(numberItem)).append("\n");
    sb.append("    integerItem: ").append(toIndentedString(integerItem)).append("\n");
    sb.append("    boolItem: ").append(toIndentedString(boolItem)).append("\n");
    sb.append("    dateItem: ").append(toIndentedString(dateItem)).append("\n");
    sb.append("    datetimeItem: ").append(toIndentedString(datetimeItem)).append("\n");
    sb.append("    arrayItem: ").append(toIndentedString(arrayItem)).append("\n");
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

