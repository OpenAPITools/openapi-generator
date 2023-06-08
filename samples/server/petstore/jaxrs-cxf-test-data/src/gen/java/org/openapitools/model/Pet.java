package org.openapitools.model;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import org.openapitools.model.Category;
import org.openapitools.model.Tag;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.ApiModelProperty;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;


public class Pet  {
  
  @ApiModelProperty(value = "")
  private Long id;

  @ApiModelProperty(value = "")
  @Valid
  private Category category;

  @ApiModelProperty(example = "doggie", required = true, value = "")
  private String name;

  @ApiModelProperty(required = true, value = "")
  private Set<String> photoUrls = new LinkedHashSet<>();

  @ApiModelProperty(value = "")
  @Valid
  private List<Tag> tags = null;

public enum StatusEnum {

    @JsonProperty("available") AVAILABLE(String.valueOf("available")),
    @JsonProperty("pending") PENDING(String.valueOf("pending")),
    @JsonProperty("sold") SOLD(String.valueOf("sold"));

    private String value;

    StatusEnum (String v) {
        value = v;
    }

    public String value() {
        return value;
    }

    @Override
    public String toString() {
        return String.valueOf(value);
    }

    public static StatusEnum fromValue(String value) {
        for (StatusEnum b : StatusEnum.values()) {
            if (b.value.equals(value)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected value '" + value + "'");
    }
}

 /**
  * pet status in the store
  */
  @ApiModelProperty(value = "pet status in the store")
  private StatusEnum status;
 /**
  * Get id
  * @return id
  */
  @JsonProperty("id")
  public Long getId() {
    return id;
  }

  /**
   * Sets the <code>id</code> property.
   */
 public void setId(Long id) {
    this.id = id;
  }

  /**
   * Sets the <code>id</code> property.
   */
  public Pet id(Long id) {
    this.id = id;
    return this;
  }

 /**
  * Get category
  * @return category
  */
  @JsonProperty("category")
  public Category getCategory() {
    return category;
  }

  /**
   * Sets the <code>category</code> property.
   */
 public void setCategory(Category category) {
    this.category = category;
  }

  /**
   * Sets the <code>category</code> property.
   */
  public Pet category(Category category) {
    this.category = category;
    return this;
  }

 /**
  * Get name
  * @return name
  */
  @JsonProperty("name")
  @NotNull
  public String getName() {
    return name;
  }

  /**
   * Sets the <code>name</code> property.
   */
 public void setName(String name) {
    this.name = name;
  }

  /**
   * Sets the <code>name</code> property.
   */
  public Pet name(String name) {
    this.name = name;
    return this;
  }

 /**
  * Get photoUrls
  * @return photoUrls
  */
  @JsonProperty("photoUrls")
  @NotNull
  public Set<String> getPhotoUrls() {
    return photoUrls;
  }

  /**
   * Sets the <code>photoUrls</code> property.
   */
 @JsonDeserialize(as = LinkedHashSet.class)
 public void setPhotoUrls(Set<String> photoUrls) {
    this.photoUrls = photoUrls;
  }

  /**
   * Sets the <code>photoUrls</code> property.
   */
  public Pet photoUrls(Set<String> photoUrls) {
    this.photoUrls = photoUrls;
    return this;
  }

  /**
   * Adds a new item to the <code>photoUrls</code> list.
   */
  public Pet addPhotoUrlsItem(String photoUrlsItem) {
    this.photoUrls.add(photoUrlsItem);
    return this;
  }

 /**
  * Get tags
  * @return tags
  */
  @JsonProperty("tags")
  public List<Tag> getTags() {
    return tags;
  }

  /**
   * Sets the <code>tags</code> property.
   */
 public void setTags(List<Tag> tags) {
    this.tags = tags;
  }

  /**
   * Sets the <code>tags</code> property.
   */
  public Pet tags(List<Tag> tags) {
    this.tags = tags;
    return this;
  }

  /**
   * Adds a new item to the <code>tags</code> list.
   */
  public Pet addTagsItem(Tag tagsItem) {
    this.tags.add(tagsItem);
    return this;
  }

 /**
  * pet status in the store
  * @return status
  */
  @JsonProperty("status")
  public String getStatus() {
    return status == null ? null : status.value();
  }

  /**
   * Sets the <code>status</code> property.
   */
 public void setStatus(StatusEnum status) {
    this.status = status;
  }

  /**
   * Sets the <code>status</code> property.
   */
  public Pet status(StatusEnum status) {
    this.status = status;
    return this;
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
  private static String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

