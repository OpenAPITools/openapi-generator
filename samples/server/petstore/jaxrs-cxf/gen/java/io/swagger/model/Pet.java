package io.swagger.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import io.swagger.model.Category;
import io.swagger.model.Tag;
import java.util.*;

import javax.xml.bind.annotation.XmlRootElement;



@XmlRootElement(name="Pet")
public class Pet  {
  
  
  private Long id = null;
  
  private Category category = null;
  
  private String name = null;
  
  private List<String> photoUrls = new ArrayList<String>();
  
  private List<Tag> tags = new ArrayList<Tag>();
  
  public enum StatusEnum {
     available,  pending,  sold, 
  };
  private StatusEnum status = null;

  
  /**
   **/
  @JsonProperty("id")
  public Long getId() {
    return id;
  }
  public void setId(Long id) {
    this.id = id;
  }

  
  /**
   **/
  @JsonProperty("category")
  public Category getCategory() {
    return category;
  }
  public void setCategory(Category category) {
    this.category = category;
  }

  
  /**
   **/
  @JsonProperty("name")
  public String getName() {
    return name;
  }
  public void setName(String name) {
    this.name = name;
  }

  
  /**
   **/
  @JsonProperty("photoUrls")
  public List<String> getPhotoUrls() {
    return photoUrls;
  }
  public void setPhotoUrls(List<String> photoUrls) {
    this.photoUrls = photoUrls;
  }

  
  /**
   **/
  @JsonProperty("tags")
  public List<Tag> getTags() {
    return tags;
  }
  public void setTags(List<Tag> tags) {
    this.tags = tags;
  }

  
  /**
   * pet status in the store
   **/
  @JsonProperty("status")
  public StatusEnum getStatus() {
    return status;
  }
  public void setStatus(StatusEnum status) {
    this.status = status;
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
