package io.swagger.client.model;

import io.swagger.client.model.Category;
import io.swagger.client.model.Tag;
import java.util.*;

import io.swagger.annotations.*;
import com.google.gson.annotations.SerializedName;


@ApiModel(description = "")
public class Pet  {
  
  @SerializedName("id")
  private Long id = null;
  @SerializedName("category")
  private Category category = null;
  @SerializedName("name")
  private String name = null;
  @SerializedName("photoUrls")
  private List<String> photoUrls = null;
  @SerializedName("tags")
  private List<Tag> tags = null;
  public enum StatusEnum {
     available,  pending,  sold, 
  };
  @SerializedName("status")
  private StatusEnum status = null;

  /**
   **/
  @ApiModelProperty(value = "")
  public Long getId() {
    return id;
  }
  public void setId(Long id) {
    this.id = id;
  }

  /**
   **/
  @ApiModelProperty(value = "")
  public Category getCategory() {
    return category;
  }
  public void setCategory(Category category) {
    this.category = category;
  }

  /**
   **/
  @ApiModelProperty(required = true, value = "")
  public String getName() {
    return name;
  }
  public void setName(String name) {
    this.name = name;
  }

  /**
   **/
  @ApiModelProperty(required = true, value = "")
  public List<String> getPhotoUrls() {
    return photoUrls;
  }
  public void setPhotoUrls(List<String> photoUrls) {
    this.photoUrls = photoUrls;
  }

  /**
   **/
  @ApiModelProperty(value = "")
  public List<Tag> getTags() {
    return tags;
  }
  public void setTags(List<Tag> tags) {
    this.tags = tags;
  }

  /**
   * pet status in the store
   **/
  @ApiModelProperty(value = "pet status in the store")
  public StatusEnum getStatus() {
    return status;
  }
  public void setStatus(StatusEnum status) {
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
    return (id == null ? pet.id == null : id.equals(pet.id)) &&
        (category == null ? pet.category == null : category.equals(pet.category)) &&
        (name == null ? pet.name == null : name.equals(pet.name)) &&
        (photoUrls == null ? pet.photoUrls == null : photoUrls.equals(pet.photoUrls)) &&
        (tags == null ? pet.tags == null : tags.equals(pet.tags)) &&
        (status == null ? pet.status == null : status.equals(pet.status));
  }

  @Override 
  public int hashCode() {
    int result = 17;
    result = 31 * result + (id == null ? 0: id.hashCode());
    result = 31 * result + (category == null ? 0: category.hashCode());
    result = 31 * result + (name == null ? 0: name.hashCode());
    result = 31 * result + (photoUrls == null ? 0: photoUrls.hashCode());
    result = 31 * result + (tags == null ? 0: tags.hashCode());
    result = 31 * result + (status == null ? 0: status.hashCode());
    return result;
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class Pet {\n");
    
    sb.append("  id: ").append(id).append("\n");
    sb.append("  category: ").append(category).append("\n");
    sb.append("  name: ").append(name).append("\n");
    sb.append("  photoUrls: ").append(photoUrls).append("\n");
    sb.append("  tags: ").append(tags).append("\n");
    sb.append("  status: ").append(status).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
