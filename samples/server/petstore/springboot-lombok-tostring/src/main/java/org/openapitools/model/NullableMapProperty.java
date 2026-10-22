package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import org.openapitools.jackson.nullable.JsonNullable;
import org.springframework.lang.Nullable;
import java.util.NoSuchElementException;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import org.hibernate.validator.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * NullableMapProperty
 */
@lombok.Getter
@lombok.Setter
@lombok.ToString
@lombok.EqualsAndHashCode

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public class NullableMapProperty {

  
  @Schema(name = "languageValues", requiredMode = Schema.RequiredMode.NOT_REQUIRED, nullable = true)
  @JsonProperty("languageValues")
  private JsonNullable<Map<String, String>> languageValues = JsonNullable.<Map<String, String>>undefined();

  public NullableMapProperty languageValues(Map<String, String> languageValues) {
    this.languageValues = JsonNullable.of(languageValues);
    return this;
  }

  public NullableMapProperty putLanguageValuesItem(String key, String languageValuesItem) {
    if (this.languageValues == null || !this.languageValues.isPresent() || this.languageValues.get() == null) {
      this.languageValues = JsonNullable.of(new HashMap<>());
    }
    this.languageValues.get().put(key, languageValuesItem);
    return this;
  }



}

