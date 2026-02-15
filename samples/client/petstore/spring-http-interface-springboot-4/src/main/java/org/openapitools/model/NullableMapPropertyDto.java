package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import java.util.HashMap;
import java.util.Map;
import org.springframework.lang.Nullable;
import java.time.OffsetDateTime;
import jakarta.validation.constraints.NotNull;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * NullableMapPropertyDto
 */

@JsonTypeName("NullableMapProperty")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.20.0-SNAPSHOT")
public class NullableMapPropertyDto {

  
  private @Nullable Map<String, String> languageValues;

  public NullableMapPropertyDto languageValues(@Nullable Map<String, String> languageValues) {
    this.languageValues = languageValues;
    return this;
  }

  public NullableMapPropertyDto putLanguageValuesItem(String key, String languageValuesItem) {
    if (this.languageValues == null) {
      this.languageValues = new HashMap<>();
    }
    this.languageValues.put(key, languageValuesItem);
    return this;
  }

  /**
   * Get languageValues
   * @return languageValues
   */
  
  @JsonProperty("languageValues")
  public @Nullable Map<String, String> getLanguageValues() {
    return languageValues;
  }

  public void setLanguageValues(@Nullable Map<String, String> languageValues) {
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
    return Objects.equals(this.languageValues, nullableMapProperty.languageValues);
  }

  @Override
  public int hashCode() {
    return Objects.hash(languageValues);
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
  private String toIndentedString(@Nullable Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

