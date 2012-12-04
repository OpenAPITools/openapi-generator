package com.wordnik.petstore.model;

import java.util.*;
import com.wordnik.petstore.model.Category;
import com.wordnik.petstore.model.Tag;
public class Pet {
  private List<Tag> tags = new ArrayList<Tag>();
  private Long id = null;
  private Category category = null;
  /* pet status in the store */
  private String status = null;
  private String name = null;
  private List<String> photoUrls = new ArrayList<String>();
  public List<Tag> getTags() {
    return tags;
  }
  public void setTags(List<Tag> tags) {
    this.tags = tags;
  }

  public Long getId() {
    return id;
  }
  public void setId(Long id) {
    this.id = id;
  }

  public Category getCategory() {
    return category;
  }
  public void setCategory(Category category) {
    this.category = category;
  }

  public String getStatus() {
    return status;
  }
  public void setStatus(String status) {
    this.status = status;
  }

  public String getName() {
    return name;
  }
  public void setName(String name) {
    this.name = name;
  }

  public List<String> getPhotoUrls() {
    return photoUrls;
  }
  public void setPhotoUrls(List<String> photoUrls) {
    this.photoUrls = photoUrls;
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class Pet {\n");
    sb.append("  tags: ").append(tags).append("\n");
    sb.append("  id: ").append(id).append("\n");
    sb.append("  category: ").append(category).append("\n");
    sb.append("  status: ").append(status).append("\n");
    sb.append("  name: ").append(name).append("\n");
    sb.append("  photoUrls: ").append(photoUrls).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}

