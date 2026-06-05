package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import com.fasterxml.jackson.annotation.JsonValue;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.openapitools.model.Category;
import org.openapitools.model.Tag;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * A pet for sale in the pet store
 */
@lombok.Data
@lombok.Builder
@lombok.NoArgsConstructor
@lombok.AllArgsConstructor

@Schema(name = "Pet", description = "A pet for sale in the pet store")
@JsonPropertyOrder({
    Pet.JSON_PROPERTY_ID,
    Pet.JSON_PROPERTY_CATEGORY,
    Pet.JSON_PROPERTY_NAME,
    Pet.JSON_PROPERTY_PHOTO_URLS,
    Pet.JSON_PROPERTY_TAGS,
    Pet.JSON_PROPERTY_STATUS
})
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.23.0-SNAPSHOT")
public class Pet {

    public static final String JSON_PROPERTY_ID = "id";
  
  @Schema(name = "id", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("id")

  private @Nullable Long id;

    public static final String JSON_PROPERTY_CATEGORY = "category";
  @Valid 
  @Schema(name = "category", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("category")

  private @Nullable Category category;

    public static final String JSON_PROPERTY_NAME = "name";
  @NotNull 
  @Schema(name = "name", example = "doggie", requiredMode = Schema.RequiredMode.REQUIRED)
  @JsonProperty("name")

  private String name;

    public static final String JSON_PROPERTY_PHOTO_URLS = "photoUrls";
  
  @Schema(name = "photoUrls", requiredMode = Schema.RequiredMode.REQUIRED)
  @JsonProperty("photoUrls")

  @lombok.Builder.Default
  private List<String> photoUrls = new ArrayList<>();

    public static final String JSON_PROPERTY_TAGS = "tags";
  
  @Schema(name = "tags", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("tags")

  @lombok.Builder.Default
  private List<@Valid Tag> tags = new ArrayList<>();

  /**
   * pet status in the store
   */
  public enum StatusEnum {
    AVAILABLE("available"),
    
    PENDING("pending"),
    
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

    public static final String JSON_PROPERTY_STATUS = "status";
  
  @Schema(name = "status", description = "pet status in the store", deprecated = true, requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("status")

  @Deprecated
  private @Nullable StatusEnum status;

}

