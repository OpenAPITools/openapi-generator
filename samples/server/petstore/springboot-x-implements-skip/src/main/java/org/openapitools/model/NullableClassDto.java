package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.openapitools.jackson.nullable.JsonNullable;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.lang.Nullable;
import java.util.NoSuchElementException;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

import java.util.Map;
import java.util.HashMap;
import com.fasterxml.jackson.annotation.JsonAnyGetter;
import com.fasterxml.jackson.annotation.JsonAnySetter;
/**
 * NullableClassDto
 */

@JsonTypeName("NullableClass")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.20.0-SNAPSHOT")
public class NullableClassDto {

  private JsonNullable<Integer> integerProp = JsonNullable.<Integer>undefined();

  private JsonNullable<BigDecimal> numberProp = JsonNullable.<BigDecimal>undefined();

  private JsonNullable<Boolean> booleanProp = JsonNullable.<Boolean>undefined();

  private JsonNullable<String> stringProp = JsonNullable.<String>undefined();

  @DateTimeFormat(iso = DateTimeFormat.ISO.DATE)
  private JsonNullable<LocalDate> dateProp = JsonNullable.<LocalDate>undefined();

  @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME)
  private JsonNullable<OffsetDateTime> datetimeProp = JsonNullable.<OffsetDateTime>undefined();

  @Valid
  private JsonNullable<List<Object>> arrayNullableProp = JsonNullable.<List<Object>>undefined();

  @Valid
  private JsonNullable<List<Object>> arrayAndItemsNullableProp = JsonNullable.<List<Object>>undefined();

  @Valid
  private List<Object> arrayItemsNullable = new ArrayList<>();

  @Valid
  private JsonNullable<Map<String, Object>> objectNullableProp = JsonNullable.<Map<String, Object>>undefined();

  @Valid
  private JsonNullable<Map<String, Object>> objectAndItemsNullableProp = JsonNullable.<Map<String, Object>>undefined();

  @Valid
  private Map<String, Object> objectItemsNullable = new HashMap<>();

  public NullableClassDto integerProp(Integer integerProp) {
    this.integerProp = JsonNullable.of(integerProp);
    return this;
  }

  /**
   * Get integerProp
   * @return integerProp
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("integer_prop")
  public JsonNullable<Integer> getIntegerProp() {
    return integerProp;
  }

  public void setIntegerProp(JsonNullable<Integer> integerProp) {
    this.integerProp = integerProp;
  }

  public NullableClassDto numberProp(BigDecimal numberProp) {
    this.numberProp = JsonNullable.of(numberProp);
    return this;
  }

  /**
   * Get numberProp
   * @return numberProp
   */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("number_prop")
  public JsonNullable<BigDecimal> getNumberProp() {
    return numberProp;
  }

  public void setNumberProp(JsonNullable<BigDecimal> numberProp) {
    this.numberProp = numberProp;
  }

  public NullableClassDto booleanProp(Boolean booleanProp) {
    this.booleanProp = JsonNullable.of(booleanProp);
    return this;
  }

  /**
   * Get booleanProp
   * @return booleanProp
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("boolean_prop")
  public JsonNullable<Boolean> getBooleanProp() {
    return booleanProp;
  }

  public void setBooleanProp(JsonNullable<Boolean> booleanProp) {
    this.booleanProp = booleanProp;
  }

  public NullableClassDto stringProp(String stringProp) {
    this.stringProp = JsonNullable.of(stringProp);
    return this;
  }

  /**
   * Get stringProp
   * @return stringProp
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("string_prop")
  public JsonNullable<String> getStringProp() {
    return stringProp;
  }

  public void setStringProp(JsonNullable<String> stringProp) {
    this.stringProp = stringProp;
  }

  public NullableClassDto dateProp(LocalDate dateProp) {
    this.dateProp = JsonNullable.of(dateProp);
    return this;
  }

  /**
   * Get dateProp
   * @return dateProp
   */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("date_prop")
  public JsonNullable<LocalDate> getDateProp() {
    return dateProp;
  }

  public void setDateProp(JsonNullable<LocalDate> dateProp) {
    this.dateProp = dateProp;
  }

  public NullableClassDto datetimeProp(OffsetDateTime datetimeProp) {
    this.datetimeProp = JsonNullable.of(datetimeProp);
    return this;
  }

  /**
   * Get datetimeProp
   * @return datetimeProp
   */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("datetime_prop")
  public JsonNullable<OffsetDateTime> getDatetimeProp() {
    return datetimeProp;
  }

  public void setDatetimeProp(JsonNullable<OffsetDateTime> datetimeProp) {
    this.datetimeProp = datetimeProp;
  }

  public NullableClassDto arrayNullableProp(List<Object> arrayNullableProp) {
    this.arrayNullableProp = JsonNullable.of(arrayNullableProp);
    return this;
  }

  public NullableClassDto addArrayNullablePropItem(Object arrayNullablePropItem) {
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
  
  @ApiModelProperty(value = "")
  @JsonProperty("array_nullable_prop")
  public JsonNullable<List<Object>> getArrayNullableProp() {
    return arrayNullableProp;
  }

  public void setArrayNullableProp(JsonNullable<List<Object>> arrayNullableProp) {
    this.arrayNullableProp = arrayNullableProp;
  }

  public NullableClassDto arrayAndItemsNullableProp(List<Object> arrayAndItemsNullableProp) {
    this.arrayAndItemsNullableProp = JsonNullable.of(arrayAndItemsNullableProp);
    return this;
  }

  public NullableClassDto addArrayAndItemsNullablePropItem(Object arrayAndItemsNullablePropItem) {
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
  
  @ApiModelProperty(value = "")
  @JsonProperty("array_and_items_nullable_prop")
  public JsonNullable<List<Object>> getArrayAndItemsNullableProp() {
    return arrayAndItemsNullableProp;
  }

  public void setArrayAndItemsNullableProp(JsonNullable<List<Object>> arrayAndItemsNullableProp) {
    this.arrayAndItemsNullableProp = arrayAndItemsNullableProp;
  }

  public NullableClassDto arrayItemsNullable(List<Object> arrayItemsNullable) {
    this.arrayItemsNullable = arrayItemsNullable;
    return this;
  }

  public NullableClassDto addArrayItemsNullableItem(Object arrayItemsNullableItem) {
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
  
  @ApiModelProperty(value = "")
  @JsonProperty("array_items_nullable")
  public List<Object> getArrayItemsNullable() {
    return arrayItemsNullable;
  }

  public void setArrayItemsNullable(List<Object> arrayItemsNullable) {
    this.arrayItemsNullable = arrayItemsNullable;
  }

  public NullableClassDto objectNullableProp(Map<String, Object> objectNullableProp) {
    this.objectNullableProp = JsonNullable.of(objectNullableProp);
    return this;
  }

  public NullableClassDto putObjectNullablePropItem(String key, Object objectNullablePropItem) {
    if (this.objectNullableProp == null || !this.objectNullableProp.isPresent()) {
      this.objectNullableProp = JsonNullable.of(new HashMap<>());
    }
    this.objectNullableProp.get().put(key, objectNullablePropItem);
    return this;
  }

  /**
   * Get objectNullableProp
   * @return objectNullableProp
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("object_nullable_prop")
  public JsonNullable<Map<String, Object>> getObjectNullableProp() {
    return objectNullableProp;
  }

  public void setObjectNullableProp(JsonNullable<Map<String, Object>> objectNullableProp) {
    this.objectNullableProp = objectNullableProp;
  }

  public NullableClassDto objectAndItemsNullableProp(Map<String, Object> objectAndItemsNullableProp) {
    this.objectAndItemsNullableProp = JsonNullable.of(objectAndItemsNullableProp);
    return this;
  }

  public NullableClassDto putObjectAndItemsNullablePropItem(String key, Object objectAndItemsNullablePropItem) {
    if (this.objectAndItemsNullableProp == null || !this.objectAndItemsNullableProp.isPresent()) {
      this.objectAndItemsNullableProp = JsonNullable.of(new HashMap<>());
    }
    this.objectAndItemsNullableProp.get().put(key, objectAndItemsNullablePropItem);
    return this;
  }

  /**
   * Get objectAndItemsNullableProp
   * @return objectAndItemsNullableProp
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("object_and_items_nullable_prop")
  public JsonNullable<Map<String, Object>> getObjectAndItemsNullableProp() {
    return objectAndItemsNullableProp;
  }

  public void setObjectAndItemsNullableProp(JsonNullable<Map<String, Object>> objectAndItemsNullableProp) {
    this.objectAndItemsNullableProp = objectAndItemsNullableProp;
  }

  public NullableClassDto objectItemsNullable(Map<String, Object> objectItemsNullable) {
    this.objectItemsNullable = objectItemsNullable;
    return this;
  }

  public NullableClassDto putObjectItemsNullableItem(String key, Object objectItemsNullableItem) {
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
  
  @ApiModelProperty(value = "")
  @JsonProperty("object_items_nullable")
  public Map<String, Object> getObjectItemsNullable() {
    return objectItemsNullable;
  }

  public void setObjectItemsNullable(Map<String, Object> objectItemsNullable) {
    this.objectItemsNullable = objectItemsNullable;
  }
    /**
    * A container for additional, undeclared properties.
    * This is a holder for any undeclared properties as specified with
    * the 'additionalProperties' keyword in the OAS document.
    */
    private Map<String, Object> additionalProperties;

    /**
    * Set the additional (undeclared) property with the specified name and value.
    * If the property does not already exist, create it otherwise replace it.
    */
    @JsonAnySetter
    public NullableClassDto putAdditionalProperty(String key, Object value) {
        if (this.additionalProperties == null) {
            this.additionalProperties = new HashMap<String, Object>();
        }
        this.additionalProperties.put(key, value);
        return this;
    }

    /**
    * Return the additional (undeclared) property.
    */
    @JsonAnyGetter
    public Map<String, Object> getAdditionalProperties() {
        return additionalProperties;
    }

    /**
    * Return the additional (undeclared) property with the specified name.
    */
    public Object getAdditionalProperty(String key) {
        if (this.additionalProperties == null) {
            return null;
        }
        return this.additionalProperties.get(key);
    }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    NullableClassDto nullableClass = (NullableClassDto) o;
    return equalsNullable(this.integerProp, nullableClass.integerProp) &&
        equalsNullable(this.numberProp, nullableClass.numberProp) &&
        equalsNullable(this.booleanProp, nullableClass.booleanProp) &&
        equalsNullable(this.stringProp, nullableClass.stringProp) &&
        equalsNullable(this.dateProp, nullableClass.dateProp) &&
        equalsNullable(this.datetimeProp, nullableClass.datetimeProp) &&
        equalsNullable(this.arrayNullableProp, nullableClass.arrayNullableProp) &&
        equalsNullable(this.arrayAndItemsNullableProp, nullableClass.arrayAndItemsNullableProp) &&
        Objects.equals(this.arrayItemsNullable, nullableClass.arrayItemsNullable) &&
        equalsNullable(this.objectNullableProp, nullableClass.objectNullableProp) &&
        equalsNullable(this.objectAndItemsNullableProp, nullableClass.objectAndItemsNullableProp) &&
        Objects.equals(this.objectItemsNullable, nullableClass.objectItemsNullable) &&
    Objects.equals(this.additionalProperties, nullableClass.additionalProperties);
  }

  private static <T> boolean equalsNullable(JsonNullable<T> a, JsonNullable<T> b) {
    return a == b || (a != null && b != null && a.isPresent() && b.isPresent() && Objects.deepEquals(a.get(), b.get()));
  }

  @Override
  public int hashCode() {
    return Objects.hash(hashCodeNullable(integerProp), hashCodeNullable(numberProp), hashCodeNullable(booleanProp), hashCodeNullable(stringProp), hashCodeNullable(dateProp), hashCodeNullable(datetimeProp), hashCodeNullable(arrayNullableProp), hashCodeNullable(arrayAndItemsNullableProp), arrayItemsNullable, hashCodeNullable(objectNullableProp), hashCodeNullable(objectAndItemsNullableProp), objectItemsNullable, additionalProperties);
  }

  private static <T> int hashCodeNullable(JsonNullable<T> a) {
    if (a == null) {
      return 1;
    }
    return a.isPresent() ? Arrays.deepHashCode(new Object[]{a.get()}) : 31;
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class NullableClassDto {\n");
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
    
    sb.append("    additionalProperties: ").append(toIndentedString(additionalProperties)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private String toIndentedString(@Nullable Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

