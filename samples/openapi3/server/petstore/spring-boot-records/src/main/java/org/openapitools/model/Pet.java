package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import org.openapitools.jackson.nullable.JsonNullable;
import org.openapitools.model.Owner;
import org.openapitools.model.PetKind;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.lang.Nullable;
import java.util.NoSuchElementException;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * A pet with an owner
 *
 * @param name Get name
 * @param tag Get tag
 * @param nickname Get nickname
 * @param age Get age
 * @param kind Get kind
 * @param status Get status
 * @param labels Get labels
 * @param colors Get colors
 * @param aliases Get aliases
 * @param owner Get owner
 * @param born Get born
 */

@Schema(name = "Pet", description = "A pet with an owner")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public record Pet(
    @JsonInclude(JsonInclude.Include.NON_NULL)
    @NotNull @Size(min = 2) 
    @Schema(name = "name", requiredMode = Schema.RequiredMode.REQUIRED)
    @JsonProperty("name")
    String name,

    @JsonInclude(JsonInclude.Include.NON_NULL)
    
    @Schema(name = "tag", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
    @JsonProperty("tag")
    @Nullable String tag,

    
    @Schema(name = "nickname", requiredMode = Schema.RequiredMode.NOT_REQUIRED, nullable = true)
    @JsonProperty("nickname")
    JsonNullable<String> nickname,

    @JsonInclude(JsonInclude.Include.NON_NULL)
    
    @Schema(name = "age", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
    @JsonProperty("age")
    Integer age,

    @JsonInclude(JsonInclude.Include.NON_NULL)
    @Valid 
    @Schema(name = "kind", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
    @JsonProperty("kind")
    @Nullable PetKind kind,

    @JsonInclude(JsonInclude.Include.NON_NULL)
    
    @Schema(name = "status", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
    @JsonProperty("status")
    StatusEnum status,

    @JsonInclude(JsonInclude.Include.NON_NULL)
    
    @Schema(name = "labels", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
    @JsonProperty("labels")
    List<String> labels,

    @JsonInclude(JsonInclude.Include.NON_NULL)
    
    @Schema(name = "colors", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
    @JsonProperty("colors")
    List<String> colors,

    @JsonInclude(JsonInclude.Include.NON_NULL)
    @JsonDeserialize(as = LinkedHashSet.class)
    
    @Schema(name = "aliases", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
    @JsonProperty("aliases")
    Set<String> aliases,

    @JsonInclude(JsonInclude.Include.NON_NULL)
    @Valid 
    @Schema(name = "owner", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
    @JsonProperty("owner")
    @Nullable Owner owner,

    @JsonInclude(JsonInclude.Include.NON_NULL)
    @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME)
    @Valid 
    @Schema(name = "born", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
    @JsonProperty("born")
    @Nullable OffsetDateTime born
) {

  /**
   * Gets or Sets status
   */
  public enum StatusEnum {
    AVAILABLE("available"),
    
    SOLD("sold");

    private final String value;

    StatusEnum(String value) {
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
    public static StatusEnum fromValue(String value) {
      for (StatusEnum b : StatusEnum.values()) {
        if (b.value.equals(value)) {
          return b;
        }
      }
      throw new IllegalArgumentException("Unexpected value '" + value + "'");
    }
  }

  public Pet {
    if (nickname == null) {
      nickname = JsonNullable.<String>undefined();
    }
    if (age == null) {
      age = 1;
    }
    if (status == null) {
      status = StatusEnum.AVAILABLE;
    }
    if (labels == null) {
      labels = new ArrayList<>();
    }
    if (colors == null) {
      colors = new ArrayList<>(Arrays.asList("red", "blue"));
    }
    if (aliases == null) {
      aliases = new LinkedHashSet<>();
    }
  }
}

