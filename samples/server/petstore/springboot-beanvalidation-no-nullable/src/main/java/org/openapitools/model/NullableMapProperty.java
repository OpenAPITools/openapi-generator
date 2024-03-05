package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.HashMap;
import java.util.Map;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * NullableMapProperty
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class NullableMapProperty {

  @Valid
  private Map<String, String> languageValues;

  public NullableMapProperty languageValues(Map<String, String> languageValues) {
    this.languageValues = languageValues;
    return this;
  }

  public NullableMapProperty putLanguageValuesItem(String key, String languageValuesItem) {
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
  
  @ApiModelProperty(value = "")
  @JsonProperty("languageValues")
  public Map<String, String> getLanguageValues() {
    return languageValues;
  }

  public void setLanguageValues(Map<String, String> languageValues) {
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
    return Objects.equals(this.languageValues, nullableMapProperty.languageValues);
  }

  @Override
  public int hashCode() {
    return Objects.hash(languageValues);
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

