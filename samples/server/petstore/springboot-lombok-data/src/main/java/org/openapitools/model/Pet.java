package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.openapitools.model.Category;
import org.openapitools.model.Tag;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import javax.annotation.Generated;

/**
 * A pet for sale in the pet store
 */
@lombok.Data
@lombok.Builder
@lombok.NoArgsConstructor
@lombok.AllArgsConstructor

@Schema(name = "Pet", description = "A pet for sale in the pet store")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class Pet {

  private Long id;

  private Category category;

  @lombok.NonNull
  private String name;

  @lombok.NonNull
  @Valid
  private List<String> photoUrls = new ArrayList<>();

  @Valid
  private List<Tag> tags;

  /**
   * pet status in the store
   */
  public enum StatusEnum {
    AVAILABLE("available"),
    
    PENDING("pending"),
    
    SOLD("sold");

    private String value;

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

  @Deprecated
  private StatusEnum status;

  
  public static class Builder {

    private Pet instance;

    public Builder() {
      this(new Pet());
    }

    protected Builder(Pet instance) {
      this.instance = instance;
    }

    public Pet.Builder id(Long id) {
      this.instance.id(id);
      return this;
    }
    public Pet.Builder category(Category category) {
      this.instance.category(category);
      return this;
    }
    public Pet.Builder name(String name) {
      this.instance.name(name);
      return this;
    }
    public Pet.Builder photoUrls(List<String> photoUrls) {
      this.instance.photoUrls(photoUrls);
      return this;
    }
    public Pet.Builder tags(List<Tag> tags) {
      this.instance.tags(tags);
      return this;
    }
    public Pet.Builder status(StatusEnum status) {
      this.instance.status(status);
      return this;
    }
    /**
    * returns a built Pet instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public Pet build() {
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
  public static Pet.Builder builder() {
    return new Pet.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public Pet.Builder toBuilder() {
    Pet.Builder builder = new Pet.Builder();
    builder.instance.setId(id);
    builder.instance.setCategory(category);
    builder.instance.setName(name);
    builder.instance.setPhotoUrls(photoUrls);
    builder.instance.setTags(tags);
    builder.instance.setStatus(status);
    return builder;
  }

}

