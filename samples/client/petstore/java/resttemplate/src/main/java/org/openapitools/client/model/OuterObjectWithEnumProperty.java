/*
 * OpenAPI Petstore
 * This spec is mainly for testing Petstore server and contains fake endpoints, models. Please do not use this for any other purpose. Special characters: \" \\
 *
 * The version of the OpenAPI document: 1.0.0
 * 
 *
 * NOTE: This class is auto generated by OpenAPI Generator (https://openapi-generator.tech).
 * https://openapi-generator.tech
 * Do not edit the class manually.
 */


package org.openapitools.client.model;

import java.util.Objects;
import java.util.Arrays;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import com.fasterxml.jackson.annotation.JsonValue;
import org.openapitools.client.model.OuterEnumInteger;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import com.fasterxml.jackson.annotation.JsonTypeName;

/**
 * OuterObjectWithEnumProperty
 */
@JsonPropertyOrder({
  OuterObjectWithEnumProperty.JSON_PROPERTY_VALUE
})
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen", comments = "Generator version: 7.15.0-SNAPSHOT")
public class OuterObjectWithEnumProperty {
  public static final String JSON_PROPERTY_VALUE = "value";
  @javax.annotation.Nonnull
  private OuterEnumInteger value;

  public OuterObjectWithEnumProperty() {
  }

  /**
   * Constructor with all args parameters
   */
  public OuterObjectWithEnumProperty(@JsonProperty(JSON_PROPERTY_VALUE) OuterEnumInteger value) {
    this.value = value;
  }

  public OuterObjectWithEnumProperty value(@javax.annotation.Nonnull OuterEnumInteger value) {
    
    this.value = value;
    return this;
  }

  /**
   * Get value
   * @return value
   */
  @javax.annotation.Nonnull
  @JsonProperty(JSON_PROPERTY_VALUE)
  @JsonInclude(value = JsonInclude.Include.ALWAYS)

  public OuterEnumInteger getValue() {
    return value;
  }


  @JsonProperty(JSON_PROPERTY_VALUE)
  @JsonInclude(value = JsonInclude.Include.ALWAYS)
  public void setValue(@javax.annotation.Nonnull OuterEnumInteger value) {
    this.value = value;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    OuterObjectWithEnumProperty outerObjectWithEnumProperty = (OuterObjectWithEnumProperty) o;
    return Objects.equals(this.value, outerObjectWithEnumProperty.value);
  }

  @Override
  public int hashCode() {
    return Objects.hash(value);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class OuterObjectWithEnumProperty {\n");
    sb.append("    value: ").append(toIndentedString(value)).append("\n");
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

    private OuterObjectWithEnumProperty instance;

    public Builder() {
      this(new OuterObjectWithEnumProperty());
    }

    protected Builder(OuterObjectWithEnumProperty instance) {
      this.instance = instance;
    }

    public OuterObjectWithEnumProperty.Builder value(OuterEnumInteger value) {
      this.instance.value = value;
      return this;
    }


    /**
    * returns a built OuterObjectWithEnumProperty instance.
    *
    * The builder is not reusable.
    */
    public OuterObjectWithEnumProperty build() {
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
  public static OuterObjectWithEnumProperty.Builder builder() {
    return new OuterObjectWithEnumProperty.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public OuterObjectWithEnumProperty.Builder toBuilder() {
    return new OuterObjectWithEnumProperty.Builder()
      .value(getValue());
  }


}

