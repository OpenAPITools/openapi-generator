package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.joda.time.LocalDate;
import org.openapitools.jackson.nullable.JsonNullable;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.annotation.JsonTypeName;



@JsonTypeName("NullableClass")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.14.0-SNAPSHOT")
public class NullableClass extends HashMap<String, Object> implements Serializable {
  private Integer integerProp;
  private BigDecimal numberProp;
  private Boolean booleanProp;
  private String stringProp;
  private LocalDate dateProp;
  private Date datetimeProp;
  private @Valid List<Object> arrayNullableProp;
  private @Valid List<Object> arrayAndItemsNullableProp;
  private @Valid List<Object> arrayItemsNullable = new ArrayList<>();
  private @Valid Map<String, Object> objectNullableProp;
  private @Valid Map<String, Object> objectAndItemsNullableProp;
  private @Valid Map<String, Object> objectItemsNullable = new HashMap<>();

  public NullableClass() {
  }

  /**
   **/
  public NullableClass integerProp(Integer integerProp) {
    this.integerProp = integerProp;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("integer_prop")
  public Integer getIntegerProp() {
    return integerProp;
  }

  @JsonProperty("integer_prop")
  public void setIntegerProp(Integer integerProp) {
    this.integerProp = integerProp;
  }

  /**
   **/
  public NullableClass numberProp(BigDecimal numberProp) {
    this.numberProp = numberProp;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("number_prop")
  @Valid public BigDecimal getNumberProp() {
    return numberProp;
  }

  @JsonProperty("number_prop")
  public void setNumberProp(BigDecimal numberProp) {
    this.numberProp = numberProp;
  }

  /**
   **/
  public NullableClass booleanProp(Boolean booleanProp) {
    this.booleanProp = booleanProp;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("boolean_prop")
  public Boolean getBooleanProp() {
    return booleanProp;
  }

  @JsonProperty("boolean_prop")
  public void setBooleanProp(Boolean booleanProp) {
    this.booleanProp = booleanProp;
  }

  /**
   **/
  public NullableClass stringProp(String stringProp) {
    this.stringProp = stringProp;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("string_prop")
  public String getStringProp() {
    return stringProp;
  }

  @JsonProperty("string_prop")
  public void setStringProp(String stringProp) {
    this.stringProp = stringProp;
  }

  /**
   **/
  public NullableClass dateProp(LocalDate dateProp) {
    this.dateProp = dateProp;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("date_prop")
  public LocalDate getDateProp() {
    return dateProp;
  }

  @JsonProperty("date_prop")
  public void setDateProp(LocalDate dateProp) {
    this.dateProp = dateProp;
  }

  /**
   **/
  public NullableClass datetimeProp(Date datetimeProp) {
    this.datetimeProp = datetimeProp;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("datetime_prop")
  public Date getDatetimeProp() {
    return datetimeProp;
  }

  @JsonProperty("datetime_prop")
  public void setDatetimeProp(Date datetimeProp) {
    this.datetimeProp = datetimeProp;
  }

  /**
   **/
  public NullableClass arrayNullableProp(List<Object> arrayNullableProp) {
    this.arrayNullableProp = arrayNullableProp;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("array_nullable_prop")
  public List<Object> getArrayNullableProp() {
    return arrayNullableProp;
  }

  @JsonProperty("array_nullable_prop")
  public void setArrayNullableProp(List<Object> arrayNullableProp) {
    this.arrayNullableProp = arrayNullableProp;
  }

  public NullableClass addArrayNullablePropItem(Object arrayNullablePropItem) {
    if (this.arrayNullableProp == null) {
      this.arrayNullableProp = new ArrayList<>();
    }

    this.arrayNullableProp.add(arrayNullablePropItem);
    return this;
  }

  public NullableClass removeArrayNullablePropItem(Object arrayNullablePropItem) {
    if (arrayNullablePropItem != null && this.arrayNullableProp != null) {
      this.arrayNullableProp.remove(arrayNullablePropItem);
    }

    return this;
  }
  /**
   **/
  public NullableClass arrayAndItemsNullableProp(List<Object> arrayAndItemsNullableProp) {
    this.arrayAndItemsNullableProp = arrayAndItemsNullableProp;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("array_and_items_nullable_prop")
  public List<Object> getArrayAndItemsNullableProp() {
    return arrayAndItemsNullableProp;
  }

  @JsonProperty("array_and_items_nullable_prop")
  public void setArrayAndItemsNullableProp(List<Object> arrayAndItemsNullableProp) {
    this.arrayAndItemsNullableProp = arrayAndItemsNullableProp;
  }

  public NullableClass addArrayAndItemsNullablePropItem(Object arrayAndItemsNullablePropItem) {
    if (this.arrayAndItemsNullableProp == null) {
      this.arrayAndItemsNullableProp = new ArrayList<>();
    }

    this.arrayAndItemsNullableProp.add(arrayAndItemsNullablePropItem);
    return this;
  }

  public NullableClass removeArrayAndItemsNullablePropItem(Object arrayAndItemsNullablePropItem) {
    if (arrayAndItemsNullablePropItem != null && this.arrayAndItemsNullableProp != null) {
      this.arrayAndItemsNullableProp.remove(arrayAndItemsNullablePropItem);
    }

    return this;
  }
  /**
   **/
  public NullableClass arrayItemsNullable(List<Object> arrayItemsNullable) {
    this.arrayItemsNullable = arrayItemsNullable;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("array_items_nullable")
  public List<Object> getArrayItemsNullable() {
    return arrayItemsNullable;
  }

  @JsonProperty("array_items_nullable")
  public void setArrayItemsNullable(List<Object> arrayItemsNullable) {
    this.arrayItemsNullable = arrayItemsNullable;
  }

  public NullableClass addArrayItemsNullableItem(Object arrayItemsNullableItem) {
    if (this.arrayItemsNullable == null) {
      this.arrayItemsNullable = new ArrayList<>();
    }

    this.arrayItemsNullable.add(arrayItemsNullableItem);
    return this;
  }

  public NullableClass removeArrayItemsNullableItem(Object arrayItemsNullableItem) {
    if (arrayItemsNullableItem != null && this.arrayItemsNullable != null) {
      this.arrayItemsNullable.remove(arrayItemsNullableItem);
    }

    return this;
  }
  /**
   **/
  public NullableClass objectNullableProp(Map<String, Object> objectNullableProp) {
    this.objectNullableProp = objectNullableProp;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("object_nullable_prop")
  public Map<String, Object> getObjectNullableProp() {
    return objectNullableProp;
  }

  @JsonProperty("object_nullable_prop")
  public void setObjectNullableProp(Map<String, Object> objectNullableProp) {
    this.objectNullableProp = objectNullableProp;
  }

  public NullableClass putObjectNullablePropItem(String key, Object objectNullablePropItem) {
    if (this.objectNullableProp == null) {
      this.objectNullableProp = new HashMap<>();
    }

    this.objectNullableProp.put(key, objectNullablePropItem);
    return this;
  }

  public NullableClass removeObjectNullablePropItem(String key) {
    if (this.objectNullableProp != null) {
      this.objectNullableProp.remove(key);
    }

    return this;
  }
  /**
   **/
  public NullableClass objectAndItemsNullableProp(Map<String, Object> objectAndItemsNullableProp) {
    this.objectAndItemsNullableProp = objectAndItemsNullableProp;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("object_and_items_nullable_prop")
  public Map<String, Object> getObjectAndItemsNullableProp() {
    return objectAndItemsNullableProp;
  }

  @JsonProperty("object_and_items_nullable_prop")
  public void setObjectAndItemsNullableProp(Map<String, Object> objectAndItemsNullableProp) {
    this.objectAndItemsNullableProp = objectAndItemsNullableProp;
  }

  public NullableClass putObjectAndItemsNullablePropItem(String key, Object objectAndItemsNullablePropItem) {
    if (this.objectAndItemsNullableProp == null) {
      this.objectAndItemsNullableProp = new HashMap<>();
    }

    this.objectAndItemsNullableProp.put(key, objectAndItemsNullablePropItem);
    return this;
  }

  public NullableClass removeObjectAndItemsNullablePropItem(String key) {
    if (this.objectAndItemsNullableProp != null) {
      this.objectAndItemsNullableProp.remove(key);
    }

    return this;
  }
  /**
   **/
  public NullableClass objectItemsNullable(Map<String, Object> objectItemsNullable) {
    this.objectItemsNullable = objectItemsNullable;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("object_items_nullable")
  public Map<String, Object> getObjectItemsNullable() {
    return objectItemsNullable;
  }

  @JsonProperty("object_items_nullable")
  public void setObjectItemsNullable(Map<String, Object> objectItemsNullable) {
    this.objectItemsNullable = objectItemsNullable;
  }

  public NullableClass putObjectItemsNullableItem(String key, Object objectItemsNullableItem) {
    if (this.objectItemsNullable == null) {
      this.objectItemsNullable = new HashMap<>();
    }

    this.objectItemsNullable.put(key, objectItemsNullableItem);
    return this;
  }

  public NullableClass removeObjectItemsNullableItem(String key) {
    if (this.objectItemsNullable != null) {
      this.objectItemsNullable.remove(key);
    }

    return this;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    NullableClass nullableClass = (NullableClass) o;
    return Objects.equals(this.integerProp, nullableClass.integerProp) &&
        Objects.equals(this.numberProp, nullableClass.numberProp) &&
        Objects.equals(this.booleanProp, nullableClass.booleanProp) &&
        Objects.equals(this.stringProp, nullableClass.stringProp) &&
        Objects.equals(this.dateProp, nullableClass.dateProp) &&
        Objects.equals(this.datetimeProp, nullableClass.datetimeProp) &&
        Objects.equals(this.arrayNullableProp, nullableClass.arrayNullableProp) &&
        Objects.equals(this.arrayAndItemsNullableProp, nullableClass.arrayAndItemsNullableProp) &&
        Objects.equals(this.arrayItemsNullable, nullableClass.arrayItemsNullable) &&
        Objects.equals(this.objectNullableProp, nullableClass.objectNullableProp) &&
        Objects.equals(this.objectAndItemsNullableProp, nullableClass.objectAndItemsNullableProp) &&
        Objects.equals(this.objectItemsNullable, nullableClass.objectItemsNullable) &&
        super.equals(o);
  }

  @Override
  public int hashCode() {
    return Objects.hash(integerProp, numberProp, booleanProp, stringProp, dateProp, datetimeProp, arrayNullableProp, arrayAndItemsNullableProp, arrayItemsNullable, objectNullableProp, objectAndItemsNullableProp, objectItemsNullable, super.hashCode());
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class NullableClass {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    integerProp: ").append(toIndentedString(integerProp)).append("\n");
    sb.append("    numberProp: ").append(toIndentedString(numberProp)).append("\n");
    sb.append("    booleanProp: ").append(toIndentedString(booleanProp)).append("\n");
    sb.append("    stringProp: ").append(toIndentedString(stringProp)).append("\n");
    sb.append("    dateProp: ").append(toIndentedString(dateProp)).append("\n");
    sb.append("    datetimeProp: ").append(toIndentedString(datetimeProp)).append("\n");
    sb.append("    arrayNullableProp: ").append(toIndentedString(arrayNullableProp)).append("\n");
    sb.append("    arrayAndItemsNullableProp: ").append(toIndentedString(arrayAndItemsNullableProp)).append("\n");
    sb.append("    arrayItemsNullable: ").append(toIndentedString(arrayItemsNullable)).append("\n");
    sb.append("    objectNullableProp: ").append(toIndentedString(objectNullableProp)).append("\n");
    sb.append("    objectAndItemsNullableProp: ").append(toIndentedString(objectAndItemsNullableProp)).append("\n");
    sb.append("    objectItemsNullable: ").append(toIndentedString(objectItemsNullable)).append("\n");
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

