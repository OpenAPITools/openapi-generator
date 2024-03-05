package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import org.openapitools.jackson.nullable.JsonNullable;
import java.util.NoSuchElementException;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.constraints.NotNull;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * NullableMapProperty
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class NullableMapProperty {

  
  private JsonNullable<Map<String, String>> languageValues = JsonNullable.<Map<String, String>>undefined();

  public NullableMapProperty languageValues(Map<String, String> languageValues) {
    this.languageValues = JsonNullable.of(languageValues);
    return this;
  }

  public NullableMapProperty putLanguageValuesItem(String key, String languageValuesItem) {
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
    NullableMapProperty nullableMapProperty = (NullableMapProperty) o;
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
    sb.append("class NullableMapProperty {\n");
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
}

