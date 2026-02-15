package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.springframework.lang.Nullable;
import java.time.OffsetDateTime;
import jakarta.validation.constraints.NotNull;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * ContainerDefaultValueDto
 */

@JsonTypeName("ContainerDefaultValue")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.20.0-SNAPSHOT")
public class ContainerDefaultValueDto {

  
  private @Nullable List<String> nullableArray;

  
  private List<String> nullableRequiredArray;

  
  private List<String> requiredArray = new ArrayList<>();

  
  private @Nullable List<String> nullableArrayWithDefault = new ArrayList<>(Arrays.asList("foo", "bar"));

  public ContainerDefaultValueDto() {
    super();
  }

  public ContainerDefaultValueDto nullableArray(@Nullable List<String> nullableArray) {
    this.nullableArray = nullableArray;
    return this;
  }

  public ContainerDefaultValueDto addNullableArrayItem(String nullableArrayItem) {
    if (this.nullableArray == null) {
      this.nullableArray = new ArrayList<>();
    }
    this.nullableArray.add(nullableArrayItem);
    return this;
  }

  /**
   * Get nullableArray
   * @return nullableArray
   */
  
  @JsonProperty("nullable_array")
  public @Nullable List<String> getNullableArray() {
    return nullableArray;
  }

  public void setNullableArray(@Nullable List<String> nullableArray) {
    this.nullableArray = nullableArray;
  }

  public ContainerDefaultValueDto nullableRequiredArray(List<String> nullableRequiredArray) {
    this.nullableRequiredArray = nullableRequiredArray;
    return this;
  }

  public ContainerDefaultValueDto addNullableRequiredArrayItem(String nullableRequiredArrayItem) {
    if (this.nullableRequiredArray == null) {
      this.nullableRequiredArray = new ArrayList<>();
    }
    this.nullableRequiredArray.add(nullableRequiredArrayItem);
    return this;
  }

  /**
   * Get nullableRequiredArray
   * @return nullableRequiredArray
   */
  @NotNull
  @JsonProperty("nullable_required_array")
  public List<String> getNullableRequiredArray() {
    return nullableRequiredArray;
  }

  public void setNullableRequiredArray(List<String> nullableRequiredArray) {
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
  @JsonProperty("required_array")
  public List<String> getRequiredArray() {
    return requiredArray;
  }

  public void setRequiredArray(List<String> requiredArray) {
    this.requiredArray = requiredArray;
  }

  public ContainerDefaultValueDto nullableArrayWithDefault(@Nullable List<String> nullableArrayWithDefault) {
    this.nullableArrayWithDefault = nullableArrayWithDefault;
    return this;
  }

  public ContainerDefaultValueDto addNullableArrayWithDefaultItem(String nullableArrayWithDefaultItem) {
    if (this.nullableArrayWithDefault == null) {
      this.nullableArrayWithDefault = new ArrayList<>(Arrays.asList("foo", "bar"));
    }
    this.nullableArrayWithDefault.add(nullableArrayWithDefaultItem);
    return this;
  }

  /**
   * Get nullableArrayWithDefault
   * @return nullableArrayWithDefault
   */
  
  @JsonProperty("nullable_array_with_default")
  public @Nullable List<String> getNullableArrayWithDefault() {
    return nullableArrayWithDefault;
  }

  public void setNullableArrayWithDefault(@Nullable List<String> nullableArrayWithDefault) {
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
    return Objects.equals(this.nullableArray, containerDefaultValue.nullableArray) &&
        Objects.equals(this.nullableRequiredArray, containerDefaultValue.nullableRequiredArray) &&
        Objects.equals(this.requiredArray, containerDefaultValue.requiredArray) &&
        Objects.equals(this.nullableArrayWithDefault, containerDefaultValue.nullableArrayWithDefault);
  }

  @Override
  public int hashCode() {
    return Objects.hash(nullableArray, nullableRequiredArray, requiredArray, nullableArrayWithDefault);
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
  private String toIndentedString(@Nullable Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

