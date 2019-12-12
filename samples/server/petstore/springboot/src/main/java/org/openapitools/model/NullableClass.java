package org.openapitools.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.v3.oas.annotations.media.Schema;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.openapitools.jackson.nullable.JsonNullable;
import javax.validation.Valid;
import javax.validation.constraints.*;

/**
 * NullableClass
 */

public class NullableClass extends HashMap<String, Object>  {
  @JsonProperty("integer_prop")
  private JsonNullable<Integer> integerProp = JsonNullable.undefined();

  @JsonProperty("number_prop")
  private JsonNullable<BigDecimal> numberProp = JsonNullable.undefined();

  @JsonProperty("boolean_prop")
  private JsonNullable<Boolean> booleanProp = JsonNullable.undefined();

  @JsonProperty("string_prop")
  private JsonNullable<String> stringProp = JsonNullable.undefined();

  @JsonProperty("date_prop")
  private JsonNullable<LocalDate> dateProp = JsonNullable.undefined();

  @JsonProperty("datetime_prop")
  private JsonNullable<OffsetDateTime> datetimeProp = JsonNullable.undefined();

  @JsonProperty("array_nullable_prop")
  @Valid
  private JsonNullable<List<Object>> arrayNullableProp = JsonNullable.undefined();

  @JsonProperty("array_and_items_nullable_prop")
  @Valid
  private JsonNullable<List<Object>> arrayAndItemsNullableProp = JsonNullable.undefined();

  @JsonProperty("array_items_nullable")
  @Valid
  private List<Object> arrayItemsNullable = null;

  @JsonProperty("object_nullable_prop")
  @Valid
  private JsonNullable<Map<String, Object>> objectNullableProp = JsonNullable.undefined();

  @JsonProperty("object_and_items_nullable_prop")
  @Valid
  private JsonNullable<Map<String, Object>> objectAndItemsNullableProp = JsonNullable.undefined();

  @JsonProperty("object_items_nullable")
  @Valid
  private Map<String, Object> objectItemsNullable = null;

  public NullableClass integerProp(Integer integerProp) {
    this.integerProp = JsonNullable.of(integerProp);
    return this;
  }

  /**
   * Get integerProp
   * @return integerProp
  */
  @Schema(description = "")


  public JsonNullable<Integer> getIntegerProp() {
    return integerProp;
  }

  public void setIntegerProp(JsonNullable<Integer> integerProp) {
    this.integerProp = integerProp;
  }

  public NullableClass numberProp(BigDecimal numberProp) {
    this.numberProp = JsonNullable.of(numberProp);
    return this;
  }

  /**
   * Get numberProp
   * @return numberProp
  */
  @Schema(description = "")

  @Valid

  public JsonNullable<BigDecimal> getNumberProp() {
    return numberProp;
  }

  public void setNumberProp(JsonNullable<BigDecimal> numberProp) {
    this.numberProp = numberProp;
  }

  public NullableClass booleanProp(Boolean booleanProp) {
    this.booleanProp = JsonNullable.of(booleanProp);
    return this;
  }

  /**
   * Get booleanProp
   * @return booleanProp
  */
  @Schema(description = "")


  public JsonNullable<Boolean> getBooleanProp() {
    return booleanProp;
  }

  public void setBooleanProp(JsonNullable<Boolean> booleanProp) {
    this.booleanProp = booleanProp;
  }

  public NullableClass stringProp(String stringProp) {
    this.stringProp = JsonNullable.of(stringProp);
    return this;
  }

  /**
   * Get stringProp
   * @return stringProp
  */
  @Schema(description = "")


  public JsonNullable<String> getStringProp() {
    return stringProp;
  }

  public void setStringProp(JsonNullable<String> stringProp) {
    this.stringProp = stringProp;
  }

  public NullableClass dateProp(LocalDate dateProp) {
    this.dateProp = JsonNullable.of(dateProp);
    return this;
  }

  /**
   * Get dateProp
   * @return dateProp
  */
  @Schema(description = "")

  @Valid

  public JsonNullable<LocalDate> getDateProp() {
    return dateProp;
  }

  public void setDateProp(JsonNullable<LocalDate> dateProp) {
    this.dateProp = dateProp;
  }

  public NullableClass datetimeProp(OffsetDateTime datetimeProp) {
    this.datetimeProp = JsonNullable.of(datetimeProp);
    return this;
  }

  /**
   * Get datetimeProp
   * @return datetimeProp
  */
  @Schema(description = "")

  @Valid

  public JsonNullable<OffsetDateTime> getDatetimeProp() {
    return datetimeProp;
  }

  public void setDatetimeProp(JsonNullable<OffsetDateTime> datetimeProp) {
    this.datetimeProp = datetimeProp;
  }

  public NullableClass arrayNullableProp(List<Object> arrayNullableProp) {
    this.arrayNullableProp = JsonNullable.of(arrayNullableProp);
    return this;
  }

  public NullableClass addArrayNullablePropItem(Object arrayNullablePropItem) {
    if (this.arrayNullableProp == null || !this.arrayNullableProp.isPresent()) {
      this.arrayNullableProp = JsonNullable.of(new ArrayList<>());
    }
    this.arrayNullableProp.get().add(arrayNullablePropItem);
    return this;
  }

  /**
   * Get arrayNullableProp
   * @return arrayNullableProp
  */
  @Schema(description = "")


  public JsonNullable<List<Object>> getArrayNullableProp() {
    return arrayNullableProp;
  }

  public void setArrayNullableProp(JsonNullable<List<Object>> arrayNullableProp) {
    this.arrayNullableProp = arrayNullableProp;
  }

  public NullableClass arrayAndItemsNullableProp(List<Object> arrayAndItemsNullableProp) {
    this.arrayAndItemsNullableProp = JsonNullable.of(arrayAndItemsNullableProp);
    return this;
  }

  public NullableClass addArrayAndItemsNullablePropItem(Object arrayAndItemsNullablePropItem) {
    if (this.arrayAndItemsNullableProp == null || !this.arrayAndItemsNullableProp.isPresent()) {
      this.arrayAndItemsNullableProp = JsonNullable.of(new ArrayList<>());
    }
    this.arrayAndItemsNullableProp.get().add(arrayAndItemsNullablePropItem);
    return this;
  }

  /**
   * Get arrayAndItemsNullableProp
   * @return arrayAndItemsNullableProp
  */
  @Schema(description = "")


  public JsonNullable<List<Object>> getArrayAndItemsNullableProp() {
    return arrayAndItemsNullableProp;
  }

  public void setArrayAndItemsNullableProp(JsonNullable<List<Object>> arrayAndItemsNullableProp) {
    this.arrayAndItemsNullableProp = arrayAndItemsNullableProp;
  }

  public NullableClass arrayItemsNullable(List<Object> arrayItemsNullable) {
    this.arrayItemsNullable = arrayItemsNullable;
    return this;
  }

  public NullableClass addArrayItemsNullableItem(Object arrayItemsNullableItem) {
    if (this.arrayItemsNullable == null) {
      this.arrayItemsNullable = new ArrayList<>();
    }
    this.arrayItemsNullable.add(arrayItemsNullableItem);
    return this;
  }

  /**
   * Get arrayItemsNullable
   * @return arrayItemsNullable
  */
  @Schema(description = "")


  public List<Object> getArrayItemsNullable() {
    return arrayItemsNullable;
  }

  public void setArrayItemsNullable(List<Object> arrayItemsNullable) {
    this.arrayItemsNullable = arrayItemsNullable;
  }

  public NullableClass objectNullableProp(Map<String, Object> objectNullableProp) {
    this.objectNullableProp = JsonNullable.of(objectNullableProp);
    return this;
  }

  public NullableClass putObjectNullablePropItem(String key, Object objectNullablePropItem) {
    if (this.objectNullableProp == null) {
      this.objectNullableProp = new HashMap<>();
    }
    this.objectNullableProp.put(key, objectNullablePropItem);
    return this;
  }

  /**
   * Get objectNullableProp
   * @return objectNullableProp
  */
  @Schema(description = "")


  public JsonNullable<Map<String, Object>> getObjectNullableProp() {
    return objectNullableProp;
  }

  public void setObjectNullableProp(JsonNullable<Map<String, Object>> objectNullableProp) {
    this.objectNullableProp = objectNullableProp;
  }

  public NullableClass objectAndItemsNullableProp(Map<String, Object> objectAndItemsNullableProp) {
    this.objectAndItemsNullableProp = JsonNullable.of(objectAndItemsNullableProp);
    return this;
  }

  public NullableClass putObjectAndItemsNullablePropItem(String key, Object objectAndItemsNullablePropItem) {
    if (this.objectAndItemsNullableProp == null) {
      this.objectAndItemsNullableProp = new HashMap<>();
    }
    this.objectAndItemsNullableProp.put(key, objectAndItemsNullablePropItem);
    return this;
  }

  /**
   * Get objectAndItemsNullableProp
   * @return objectAndItemsNullableProp
  */
  @Schema(description = "")


  public JsonNullable<Map<String, Object>> getObjectAndItemsNullableProp() {
    return objectAndItemsNullableProp;
  }

  public void setObjectAndItemsNullableProp(JsonNullable<Map<String, Object>> objectAndItemsNullableProp) {
    this.objectAndItemsNullableProp = objectAndItemsNullableProp;
  }

  public NullableClass objectItemsNullable(Map<String, Object> objectItemsNullable) {
    this.objectItemsNullable = objectItemsNullable;
    return this;
  }

  public NullableClass putObjectItemsNullableItem(String key, Object objectItemsNullableItem) {
    if (this.objectItemsNullable == null) {
      this.objectItemsNullable = new HashMap<>();
    }
    this.objectItemsNullable.put(key, objectItemsNullableItem);
    return this;
  }

  /**
   * Get objectItemsNullable
   * @return objectItemsNullable
  */
  @Schema(description = "")


  public Map<String, Object> getObjectItemsNullable() {
    return objectItemsNullable;
  }

  public void setObjectItemsNullable(Map<String, Object> objectItemsNullable) {
    this.objectItemsNullable = objectItemsNullable;
  }


  @Override
  public boolean equals(java.lang.Object o) {
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
  private String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

