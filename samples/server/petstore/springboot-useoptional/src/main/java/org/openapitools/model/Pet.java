package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import org.openapitools.model.Category;
import org.openapitools.model.Tag;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * Pet
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.14.0-SNAPSHOT")
public class Pet {

  private Optional<Long> id = Optional.empty();

  private Optional<Category> category = Optional.empty();

  private String name;

  @Valid
  private Set<String> photoUrls = new LinkedHashSet<>();

  @Valid
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

  @Deprecated
  private Optional<StatusEnum> status = Optional.empty();

  public Pet() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public Pet(String name, Set<String> photoUrls) {
    this.name = name;
    this.photoUrls = photoUrls;
  }

  public Pet id(Long id) {
    this.id = Optional.ofNullable(id);
    return this;
  }

  /**
   * Get id
   * @return id
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("id")
  public Optional<Long> getId() {
    return id;
  }

  public void setId(Optional<Long> id) {
    this.id = id;
  }

  public Pet category(Category category) {
    this.category = Optional.ofNullable(category);
    return this;
  }

  /**
   * Get category
   * @return category
   */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("category")
  public Optional<Category> getCategory() {
    return category;
  }

  public void setCategory(Optional<Category> category) {
    this.category = category;
  }

  public Pet name(String name) {
    this.name = name;
    return this;
  }

  /**
   * Get name
   * @return name
   */
  @NotNull 
  @ApiModelProperty(example = "doggie", required = true, value = "")
  @JsonProperty("name")
  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public Pet photoUrls(Set<String> photoUrls) {
    this.photoUrls = photoUrls;
    return this;
  }

  public Pet addPhotoUrlsItem(String photoUrlsItem) {
    if (this.photoUrls == null) {
      this.photoUrls = new LinkedHashSet<>();
    }
    this.photoUrls.add(photoUrlsItem);
    return this;
  }

  /**
   * Get photoUrls
   * @return photoUrls
   */
  @NotNull 
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("photoUrls")
  public Set<String> getPhotoUrls() {
    return photoUrls;
  }

  @JsonDeserialize(as = LinkedHashSet.class)
  public void setPhotoUrls(Set<String> photoUrls) {
    this.photoUrls = photoUrls;
  }

  public Pet tags(List<@Valid Tag> tags) {
    this.tags = tags;
    return this;
  }

  public Pet addTagsItem(Tag tagsItem) {
    if (this.tags == null) {
      this.tags = new ArrayList<>();
    }
    this.tags.add(tagsItem);
    return this;
  }

  /**
   * Get tags
   * @return tags
   */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("tags")
  public List<@Valid Tag> getTags() {
    return tags;
  }

  public void setTags(List<@Valid Tag> tags) {
    this.tags = tags;
  }

  public Pet status(StatusEnum status) {
    this.status = Optional.ofNullable(status);
    return this;
  }

  /**
   * pet status in the store
   * @return status
   * @deprecated
   */
  
  @ApiModelProperty(value = "pet status in the store")
  @JsonProperty("status")
  @Deprecated
  public Optional<StatusEnum> getStatus() {
    return status;
  }

  /**
   * @deprecated
   */
  @Deprecated
  public void setStatus(Optional<StatusEnum> status) {
    this.status = status;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Pet pet = (Pet) o;
    return Objects.equals(this.id, pet.id) &&
        Objects.equals(this.category, pet.category) &&
        Objects.equals(this.name, pet.name) &&
        Objects.equals(this.photoUrls, pet.photoUrls) &&
        Objects.equals(this.tags, pet.tags) &&
        Objects.equals(this.status, pet.status);
  }

  @Override
  public int hashCode() {
    return Objects.hash(id, category, name, photoUrls, tags, status);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Pet {\n");
    sb.append("    id: ").append(toIndentedString(id)).append("\n");
    sb.append("    category: ").append(toIndentedString(category)).append("\n");
    sb.append("    name: ").append(toIndentedString(name)).append("\n");
    sb.append("    photoUrls: ").append(toIndentedString(photoUrls)).append("\n");
    sb.append("    tags: ").append(toIndentedString(tags)).append("\n");
    sb.append("    status: ").append(toIndentedString(status)).append("\n");
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

    private Pet instance;

    public Builder() {
      this(new Pet());
    }

    protected Builder(Pet instance) {
      this.instance = instance;
    }

    protected Builder copyOf(Pet value) { 
      this.instance.setId(value.id);
      this.instance.setCategory(value.category);
      this.instance.setName(value.name);
      this.instance.setPhotoUrls(value.photoUrls);
      this.instance.setTags(value.tags);
      this.instance.setStatus(value.status);
      return this;
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
    
    public Pet.Builder photoUrls(Set<String> photoUrls) {
      this.instance.photoUrls(photoUrls);
      return this;
    }
    
    public Pet.Builder tags(List<Tag> tags) {
      this.instance.tags(tags);
      return this;
    }
    
    @Deprecated
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
  * Create a builder with no initialized field (except for the default values).
  */
  public static Pet.Builder builder() {
    return new Pet.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public Pet.Builder toBuilder() {
    Pet.Builder builder = new Pet.Builder();
    return builder.copyOf(this);
  }

}

