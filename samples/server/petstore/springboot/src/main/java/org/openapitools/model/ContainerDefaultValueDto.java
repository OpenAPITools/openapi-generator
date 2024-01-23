package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.openapitools.jackson.nullable.JsonNullable;
import java.util.NoSuchElementException;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * ContainerDefaultValueDto
 */

@JsonTypeName("ContainerDefaultValue")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class ContainerDefaultValueDto {

  @Valid
  private JsonNullable<List<String>> nullableArray = JsonNullable.<List<String>>undefined();

  @Valid
  private JsonNullable<List<String>> nullableRequiredArray = JsonNullable.<List<String>>undefined();

  @Valid
  private List<String> requiredArray = new ArrayList<>();

  @Valid
  private JsonNullable<List<String>> nullableArrayWithDefault = JsonNullable.<List<String>>undefined();

  public ContainerDefaultValueDto() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public ContainerDefaultValueDto(List<String> nullableRequiredArray, List<String> requiredArray) {
    this.nullableRequiredArray = JsonNullable.of(nullableRequiredArray);
    this.requiredArray = requiredArray;
  }

  public ContainerDefaultValueDto nullableArray(List<String> nullableArray) {
    this.nullableArray = JsonNullable.of(nullableArray);
    return this;
  }

  public ContainerDefaultValueDto addNullableArrayItem(String nullableArrayItem) {
    if (this.nullableArray == null || !this.nullableArray.isPresent()) {
      this.nullableArray = JsonNullable.of(new ArrayList<>());
    }
    this.nullableArray.get().add(nullableArrayItem);
    return this;
  }

  /**
   * Get nullableArray
   * @return nullableArray
  */
  
  @ApiModelProperty(value = "")
  @JsonProperty("nullable_array")
  public JsonNullable<List<String>> getNullableArray() {
    return nullableArray;
  }

  public void setNullableArray(JsonNullable<List<String>> nullableArray) {
    this.nullableArray = nullableArray;
  }

  public ContainerDefaultValueDto nullableRequiredArray(List<String> nullableRequiredArray) {
    this.nullableRequiredArray = JsonNullable.of(nullableRequiredArray);
    return this;
  }

  public ContainerDefaultValueDto addNullableRequiredArrayItem(String nullableRequiredArrayItem) {
    if (this.nullableRequiredArray == null || !this.nullableRequiredArray.isPresent()) {
      this.nullableRequiredArray = JsonNullable.of(new ArrayList<>());
    }
    this.nullableRequiredArray.get().add(nullableRequiredArrayItem);
    return this;
  }

  /**
   * Get nullableRequiredArray
   * @return nullableRequiredArray
  */
  @NotNull 
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("nullable_required_array")
  public JsonNullable<List<String>> getNullableRequiredArray() {
    return nullableRequiredArray;
  }

  public void setNullableRequiredArray(JsonNullable<List<String>> nullableRequiredArray) {
    this.nullableRequiredArray = nullableRequiredArray;
  }

  public ContainerDefaultValueDto requiredArray(List<String> requiredArray) {
    this.requiredArray = requiredArray;
    return this;
  }

  public ContainerDefaultValueDto addRequiredArrayItem(String requiredArrayItem) {
    if (this.requiredArray == null) {
      this.requiredArray = new ArrayList<>();
    }
    this.requiredArray.add(requiredArrayItem);
    return this;
  }

  /**
   * Get requiredArray
   * @return requiredArray
  */
  @NotNull 
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("required_array")
  public List<String> getRequiredArray() {
    return requiredArray;
  }

  public void setRequiredArray(List<String> requiredArray) {
    this.requiredArray = requiredArray;
  }

  public ContainerDefaultValueDto nullableArrayWithDefault(List<String> nullableArrayWithDefault) {
    this.nullableArrayWithDefault = JsonNullable.of(nullableArrayWithDefault);
    return this;
  }

  public ContainerDefaultValueDto addNullableArrayWithDefaultItem(String nullableArrayWithDefaultItem) {
    if (this.nullableArrayWithDefault == null || !this.nullableArrayWithDefault.isPresent()) {
      this.nullableArrayWithDefault = JsonNullable.of(new ArrayList<>(Arrays.asList("foo", "bar")));
    }
    this.nullableArrayWithDefault.get().add(nullableArrayWithDefaultItem);
    return this;
  }

  /**
   * Get nullableArrayWithDefault
   * @return nullableArrayWithDefault
  */
  
  @ApiModelProperty(value = "")
  @JsonProperty("nullable_array_with_default")
  public JsonNullable<List<String>> getNullableArrayWithDefault() {
    return nullableArrayWithDefault;
  }

  public void setNullableArrayWithDefault(JsonNullable<List<String>> nullableArrayWithDefault) {
    this.nullableArrayWithDefault = nullableArrayWithDefault;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ContainerDefaultValueDto containerDefaultValue = (ContainerDefaultValueDto) o;
    return equalsNullable(this.nullableArray, containerDefaultValue.nullableArray) &&
        Objects.equals(this.nullableRequiredArray, containerDefaultValue.nullableRequiredArray) &&
        Objects.equals(this.requiredArray, containerDefaultValue.requiredArray) &&
        equalsNullable(this.nullableArrayWithDefault, containerDefaultValue.nullableArrayWithDefault);
  }

  private static <T> boolean equalsNullable(JsonNullable<T> a, JsonNullable<T> b) {
    return a == b || (a != null && b != null && a.isPresent() && b.isPresent() && Objects.deepEquals(a.get(), b.get()));
  }

  @Override
  public int hashCode() {
    return Objects.hash(hashCodeNullable(nullableArray), nullableRequiredArray, requiredArray, hashCodeNullable(nullableArrayWithDefault));
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
    sb.append("class ContainerDefaultValueDto {\n");
    sb.append("    nullableArray: ").append(toIndentedString(nullableArray)).append("\n");
    sb.append("    nullableRequiredArray: ").append(toIndentedString(nullableRequiredArray)).append("\n");
    sb.append("    requiredArray: ").append(toIndentedString(requiredArray)).append("\n");
    sb.append("    nullableArrayWithDefault: ").append(toIndentedString(nullableArrayWithDefault)).append("\n");
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

