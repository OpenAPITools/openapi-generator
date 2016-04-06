package io.swagger.client.model;

import io.swagger.client.model.Tag;
import java.util.*;

import io.swagger.annotations.*;
import com.google.gson.annotations.SerializedName;


@ApiModel(description = "")
public class InlineResponse200  {
  
  @SerializedName("tags")
  private List<Tag> tags = null;
  @SerializedName("id")
  private Long id = null;
  @SerializedName("category")
  private Object category = null;
  public enum StatusEnum {
     available,  pending,  sold, 
  };
  @SerializedName("status")
  private StatusEnum status = null;
  @SerializedName("name")
  private String name = null;
  @SerializedName("photoUrls")
  private List<String> photoUrls = null;

  
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
   **/
  @ApiModelProperty(required = true, value = "")
  public Long getId() {
    return id;
  }
  public void setId(Long id) {
    this.id = id;
  }

  
  /**
   **/
  @ApiModelProperty(value = "")
  public Object getCategory() {
    return category;
  }
  public void setCategory(Object category) {
    this.category = category;
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

  
  /**
   **/
  @ApiModelProperty(value = "")
  public String getName() {
    return name;
  }
  public void setName(String name) {
    this.name = name;
  }

  
  /**
   **/
  @ApiModelProperty(value = "")
  public List<String> getPhotoUrls() {
    return photoUrls;
  }
  public void setPhotoUrls(List<String> photoUrls) {
    this.photoUrls = photoUrls;
  }

  

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class InlineResponse200 {\n");
    
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
