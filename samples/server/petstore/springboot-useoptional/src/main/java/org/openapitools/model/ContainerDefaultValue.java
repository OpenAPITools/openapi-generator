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
import org.openapitools.jackson.nullable.JsonNullable;
import java.util.NoSuchElementException;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * ContainerDefaultValue
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.9.0-SNAPSHOT")
public class ContainerDefaultValue {

  private JsonNullable<List<Optional<String>>> nullableArray = JsonNullable.undefined();

  private JsonNullable<List<Optional<String>>> nullableRequiredArray = JsonNullable.undefined();

  private @NotNull List<Optional<String>> requiredArray = new ArrayList<>();

  private JsonNullable<List<Optional<String>>> nullableArrayWithDefault = JsonNullable.of(new ArrayList<>(Arrays.asList("foo", "bar")));

  public ContainerDefaultValue() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public ContainerDefaultValue(JsonNullable<List<Optional<String>>> nullableRequiredArray, List<Optional<String>> requiredArray) {
    this.nullableRequiredArray = nullableRequiredArray;
    this.requiredArray = requiredArray;
  }

  public ContainerDefaultValue nullableArray(JsonNullable<List<Optional<String>>> nullableArray) {
    this.nullableArray = nullableArray;
    return this;
  }

  public ContainerDefaultValue addNullableArrayItem(Optional<String> nullableArrayItem) {
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
  public JsonNullable<List<Optional<String>>> getNullableArray() {
    return nullableArray;
  }

  public void setNullableArray(JsonNullable<List<Optional<String>>> nullableArray) {
    this.nullableArray = nullableArray;
  }

  public ContainerDefaultValue nullableRequiredArray(JsonNullable<List<Optional<String>>> nullableRequiredArray) {
    this.nullableRequiredArray = nullableRequiredArray;
    return this;
  }

  public ContainerDefaultValue addNullableRequiredArrayItem(Optional<String> nullableRequiredArrayItem) {
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
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("nullable_required_array")
  public JsonNullable<List<Optional<String>>> getNullableRequiredArray() {
    return nullableRequiredArray;
  }

  public void setNullableRequiredArray(JsonNullable<List<Optional<String>>> nullableRequiredArray) {
    this.nullableRequiredArray = nullableRequiredArray;
  }

  public ContainerDefaultValue requiredArray(List<Optional<String>> requiredArray) {
    this.requiredArray = requiredArray;
    return this;
  }

  public ContainerDefaultValue addRequiredArrayItem(Optional<String> requiredArrayItem) {
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
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("required_array")
  public @NotNull List<Optional<String>> getRequiredArray() {
    return requiredArray;
  }

  public void setRequiredArray(List<Optional<String>> requiredArray) {
    this.requiredArray = requiredArray;
  }

  public ContainerDefaultValue nullableArrayWithDefault(JsonNullable<List<Optional<String>>> nullableArrayWithDefault) {
    this.nullableArrayWithDefault = nullableArrayWithDefault;
    return this;
  }

  public ContainerDefaultValue addNullableArrayWithDefaultItem(Optional<String> nullableArrayWithDefaultItem) {
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
  public JsonNullable<List<Optional<String>>> getNullableArrayWithDefault() {
    return nullableArrayWithDefault;
  }

  public void setNullableArrayWithDefault(JsonNullable<List<Optional<String>>> nullableArrayWithDefault) {
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
    ContainerDefaultValue containerDefaultValue = (ContainerDefaultValue) o;
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
    sb.append("class ContainerDefaultValue {\n");
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
  
  public static class Builder {

    private ContainerDefaultValue instance;

    public Builder() {
      this(new ContainerDefaultValue());
    }

    protected Builder(ContainerDefaultValue instance) {
      this.instance = instance;
    }

    protected Builder copyOf(ContainerDefaultValue value) { 
      this.instance.setNullableArray(value.nullableArray);
      this.instance.setNullableRequiredArray(value.nullableRequiredArray);
      this.instance.setRequiredArray(value.requiredArray);
      this.instance.setNullableArrayWithDefault(value.nullableArrayWithDefault);
      return this;
    }

    public ContainerDefaultValue.Builder nullableArray(JsonNullable<List<Optional<String>>> nullableArray) {
      this.instance.nullableArray(nullableArray);
      return this;
    }
    public ContainerDefaultValue.Builder nullableRequiredArray(JsonNullable<List<Optional<String>>> nullableRequiredArray) {
      this.instance.nullableRequiredArray(nullableRequiredArray);
      return this;
    }
    public ContainerDefaultValue.Builder requiredArray(List<Optional<String>> requiredArray) {
      this.instance.requiredArray(requiredArray);
      return this;
    }
    public ContainerDefaultValue.Builder nullableArrayWithDefault(JsonNullable<List<Optional<String>>> nullableArrayWithDefault) {
      this.instance.nullableArrayWithDefault(nullableArrayWithDefault);
      return this;
    }
    /**
    * returns a built ContainerDefaultValue instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public ContainerDefaultValue build() {
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
  public static ContainerDefaultValue.Builder builder() {
    return new ContainerDefaultValue.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public ContainerDefaultValue.Builder toBuilder() {
    ContainerDefaultValue.Builder builder = new ContainerDefaultValue.Builder();
    return builder.copyOf(this);
  }

}
