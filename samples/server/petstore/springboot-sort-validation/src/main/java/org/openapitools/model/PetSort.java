package org.openapitools.model;

import com.fasterxml.jackson.annotation.JsonValue;
import jakarta.annotation.Generated;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import java.io.Serializable;
import java.net.URI;
import java.time.OffsetDateTime;
import java.util.*;
import java.util.Objects;
import org.openapitools.configuration.ValuedEnum;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

/**
 * Gets or Sets PetSort
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.28.0-SNAPSHOT")
public enum PetSort implements Serializable, ValuedEnum<String> {
  
  ID_ASC("id,asc"),
  
  ID_DESC("id,desc"),
  
  CREATED_AT_ASC("createdAt,asc"),
  
  CREATED_AT_DESC("createdAt,desc");

  private final String value;

  PetSort(String value) {
    this.value = value;
  }

  @JsonValue
  public String getValue() {
    return value;
  }

  @Override
  public String toString() {
    return String.valueOf(value);
  }

  @JsonCreator
  public static PetSort fromValue(String value) {
    for (PetSort b : PetSort.values()) {
      if (b.value.equals(value)) {
        return b;
      }
    }
    throw new IllegalArgumentException("Unexpected value '" + value + "'");
  }
}

