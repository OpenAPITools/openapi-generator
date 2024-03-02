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
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * ContainerDefaultValue
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class ContainerDefaultValue {

  @Valid
  private List<String> nullableArray;

  @Valid
  private List<String> nullableRequiredArray;

  @Valid
  private List<String> requiredArray = new ArrayList<>();

  @Valid
  private List<String> nullableArrayWithDefault = new ArrayList<>(Arrays.asList("foo", "bar"));

  public ContainerDefaultValue() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public ContainerDefaultValue(List<String> nullableRequiredArray, List<String> requiredArray) {
    this.nullableRequiredArray = nullableRequiredArray;
    this.requiredArray = requiredArray;
  }

  public ContainerDefaultValue nullableArray(List<String> nullableArray) {
    this.nullableArray = nullableArray;
    return this;
  }

  public ContainerDefaultValue addNullableArrayItem(String nullableArrayItem) {
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
  
  @ApiModelProperty(value = "")
  @JsonProperty("nullable_array")
  public List<String> getNullableArray() {
    return nullableArray;
  }

  public void setNullableArray(List<String> nullableArray) {
    this.nullableArray = nullableArray;
  }

  public ContainerDefaultValue nullableRequiredArray(List<String> nullableRequiredArray) {
    this.nullableRequiredArray = nullableRequiredArray;
    return this;
  }

  public ContainerDefaultValue addNullableRequiredArrayItem(String nullableRequiredArrayItem) {
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
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("nullable_required_array")
  public List<String> getNullableRequiredArray() {
    return nullableRequiredArray;
  }

  public void setNullableRequiredArray(List<String> nullableRequiredArray) {
    this.nullableRequiredArray = nullableRequiredArray;
  }

  public ContainerDefaultValue requiredArray(List<String> requiredArray) {
    this.requiredArray = requiredArray;
    return this;
  }

  public ContainerDefaultValue addRequiredArrayItem(String requiredArrayItem) {
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

  public ContainerDefaultValue nullableArrayWithDefault(List<String> nullableArrayWithDefault) {
    this.nullableArrayWithDefault = nullableArrayWithDefault;
    return this;
  }

  public ContainerDefaultValue addNullableArrayWithDefaultItem(String nullableArrayWithDefaultItem) {
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
  
  @ApiModelProperty(value = "")
  @JsonProperty("nullable_array_with_default")
  public List<String> getNullableArrayWithDefault() {
    return nullableArrayWithDefault;
  }

  public void setNullableArrayWithDefault(List<String> nullableArrayWithDefault) {
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

    public ContainerDefaultValue.Builder nullableArray(List<String> nullableArray) {
      this.instance.nullableArray(nullableArray);
      return this;
    }
    public ContainerDefaultValue.Builder nullableRequiredArray(List<String> nullableRequiredArray) {
      this.instance.nullableRequiredArray(nullableRequiredArray);
      return this;
    }
    public ContainerDefaultValue.Builder requiredArray(List<String> requiredArray) {
      this.instance.requiredArray(requiredArray);
      return this;
    }
    public ContainerDefaultValue.Builder nullableArrayWithDefault(List<String> nullableArrayWithDefault) {
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
  * Create a builder with no initialized field.
  */
  public static ContainerDefaultValue.Builder builder() {
    return new ContainerDefaultValue.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public ContainerDefaultValue.Builder toBuilder() {
    ContainerDefaultValue.Builder builder = new ContainerDefaultValue.Builder();
    builder.instance.setNullableArray(nullableArray);
    builder.instance.setNullableRequiredArray(nullableRequiredArray);
    builder.instance.setRequiredArray(requiredArray);
    builder.instance.setNullableArrayWithDefault(nullableArrayWithDefault);
    return builder;
  }

}

