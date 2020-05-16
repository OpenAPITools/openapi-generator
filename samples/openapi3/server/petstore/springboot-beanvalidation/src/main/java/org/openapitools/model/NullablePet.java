package org.openapitools.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.ArrayList;
import java.util.List;
import org.openapitools.model.Category;
import org.openapitools.model.Tag;
import org.openapitools.jackson.nullable.JsonNullable;
import javax.validation.Valid;
import javax.validation.constraints.*;

/**
 * A pet for sale in the pet store
 */
@ApiModel(description = "A pet for sale in the pet store")

public class NullablePet   {
  @JsonProperty("id")
  private JsonNullable<Long> id = JsonNullable.undefined();

  @JsonProperty("category")
  private Category category;

  @JsonProperty("name")
  private JsonNullable<String> name = JsonNullable.undefined();

  @JsonProperty("photoUrls")
  @Valid
  private JsonNullable<List<String>> photoUrls = JsonNullable.undefined();

  @JsonProperty("tags")
  @Valid
  private List<Tag> tags = null;

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
      return null;
    }
  }

  @JsonProperty("status")
  private JsonNullable<StatusEnum> status = JsonNullable.undefined();

  public NullablePet id(Long id) {
    this.id = JsonNullable.of(id);
    return this;
  }

  /**
   * Get id
   * @return id
  */
  @ApiModelProperty(value = "")


  public JsonNullable<Long> getId() {
    return id;
  }

  public void setId(JsonNullable<Long> id) {
    this.id = id;
  }

  public NullablePet category(Category category) {
    this.category = category;
    return this;
  }

  /**
   * Get category
   * @return category
  */
  @ApiModelProperty(value = "")

  @Valid

  public Category getCategory() {
    return category;
  }

  public void setCategory(Category category) {
    this.category = category;
  }

  public NullablePet name(String name) {
    this.name = JsonNullable.of(name);
    return this;
  }

  /**
   * Get name
   * @return name
  */
  @ApiModelProperty(example = "doggie", required = true, value = "")
  @NotNull


  public JsonNullable<String> getName() {
    return name;
  }

  public void setName(JsonNullable<String> name) {
    this.name = name;
  }

  public NullablePet photoUrls(List<String> photoUrls) {
    this.photoUrls = JsonNullable.of(photoUrls);
    return this;
  }

  public NullablePet addPhotoUrlsItem(String photoUrlsItem) {
    this.photoUrls.get().add(photoUrlsItem);
    return this;
  }

  /**
   * Get photoUrls
   * @return photoUrls
  */
  @ApiModelProperty(required = true, value = "")
  @NotNull


  public JsonNullable<List<String>> getPhotoUrls() {
    return photoUrls;
  }

  public void setPhotoUrls(JsonNullable<List<String>> photoUrls) {
    this.photoUrls = photoUrls;
  }

  public NullablePet tags(List<Tag> tags) {
    this.tags = tags;
    return this;
  }

  public NullablePet addTagsItem(Tag tagsItem) {
    if (this.tags == null) {
      this.tags = new ArrayList<Tag>();
    }
    this.tags.add(tagsItem);
    return this;
  }

  /**
   * Get tags
   * @return tags
  */
  @ApiModelProperty(value = "")

  @Valid

  public List<Tag> getTags() {
    return tags;
  }

  public void setTags(List<Tag> tags) {
    this.tags = tags;
  }

  public NullablePet status(StatusEnum status) {
    this.status = JsonNullable.of(status);
    return this;
  }

  /**
   * pet status in the store
   * @return status
  */
  @ApiModelProperty(value = "pet status in the store")


  public JsonNullable<StatusEnum> getStatus() {
    return status;
  }

  public void setStatus(JsonNullable<StatusEnum> status) {
    this.status = status;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    NullablePet nullablePet = (NullablePet) o;
    return Objects.equals(this.id, nullablePet.id) &&
        Objects.equals(this.category, nullablePet.category) &&
        Objects.equals(this.name, nullablePet.name) &&
        Objects.equals(this.photoUrls, nullablePet.photoUrls) &&
        Objects.equals(this.tags, nullablePet.tags) &&
        Objects.equals(this.status, nullablePet.status);
  }

  @Override
  public int hashCode() {
    return Objects.hash(id, category, name, photoUrls, tags, status);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class NullablePet {\n");
    
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
  private String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

