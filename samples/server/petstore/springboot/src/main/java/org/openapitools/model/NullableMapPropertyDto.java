package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import org.openapitools.jackson.nullable.JsonNullable;
import java.util.NoSuchElementException;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * NullableMapPropertyDto
 */

@JsonTypeName("NullableMapProperty")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.6.0-SNAPSHOT")
public class NullableMapPropertyDto {

  @Valid
  private JsonNullable<Map<String, String>> languageValues = JsonNullable.<Map<String, String>>undefined();

  public NullableMapPropertyDto languageValues(Map<String, String> languageValues) {
    this.languageValues = JsonNullable.of(languageValues);
    return this;
  }

  public NullableMapPropertyDto putLanguageValuesItem(String key, String languageValuesItem) {
    if (this.languageValues == null || !this.languageValues.isPresent()) {
      this.languageValues = JsonNullable.of(new HashMap<>());
    }
    this.languageValues.get().put(key, languageValuesItem);
    return this;
  }

  /**
   * Get languageValues
   * @return languageValues
  */
  
  @ApiModelProperty(value = "")
  @JsonProperty("languageValues")
  public JsonNullable<Map<String, String>> getLanguageValues() {
    return languageValues;
  }

  public void setLanguageValues(JsonNullable<Map<String, String>> languageValues) {
    this.languageValues = languageValues;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    NullableMapPropertyDto nullableMapProperty = (NullableMapPropertyDto) o;
    return equalsNullable(this.languageValues, nullableMapProperty.languageValues);
  }

  private static <T> boolean equalsNullable(JsonNullable<T> a, JsonNullable<T> b) {
    return a == b || (a != null && b != null && a.isPresent() && b.isPresent() && Objects.deepEquals(a.get(), b.get()));
  }

  @Override
  public int hashCode() {
    return Objects.hash(hashCodeNullable(languageValues));
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
    sb.append("class NullableMapPropertyDto {\n");
    sb.append("    languageValues: ").append(toIndentedString(languageValues)).append("\n");
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

    private NullableMapPropertyDto instance;

    public Builder() {
      this(new NullableMapPropertyDto());
    }

    protected Builder(NullableMapPropertyDto instance) {
      this.instance = instance;
    }

    protected Builder copyOf(NullableMapPropertyDto value) { 
      this.instance.setLanguageValues(value.languageValues);
      return this;
    }

    public NullableMapPropertyDto.Builder languageValues(Map<String, String> languageValues) {
      this.instance.languageValues(languageValues);
      return this;
    }
    
    public NullableMapPropertyDto.Builder languageValues(JsonNullable<Map<String, String>> languageValues) {
      this.instance.languageValues = languageValues;
      return this;
    }
    
    /**
    * returns a built NullableMapPropertyDto instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public NullableMapPropertyDto build() {
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
  public static NullableMapPropertyDto.Builder builder() {
    return new NullableMapPropertyDto.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public NullableMapPropertyDto.Builder toBuilder() {
    NullableMapPropertyDto.Builder builder = new NullableMapPropertyDto.Builder();
    return builder.copyOf(this);
  }

}

