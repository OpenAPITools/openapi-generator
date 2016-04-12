package io.swagger.client.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import io.swagger.client.model.Tag;
import java.util.ArrayList;
import java.util.List;


/**
 * InlineResponse200
 */
@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaClientCodegen", date = "2016-04-12T23:06:03.099+08:00")
public class InlineResponse200   {
  
  private List<String> photoUrls = new ArrayList<String>();
  private String name = null;
  private Long id = null;
  private Object category = null;
  private List<Tag> tags = new ArrayList<Tag>();

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

    @Override
    @JsonValue
    public String toString() {
      return String.valueOf(value);
    }
  }

  private StatusEnum status = null;

  
  /**
   **/
  public InlineResponse200 photoUrls(List<String> photoUrls) {
    this.photoUrls = photoUrls;
    return this;
  }
  
  @ApiModelProperty(example = "null", value = "")
  @JsonProperty("photoUrls")
  public List<String> getPhotoUrls() {
    return photoUrls;
  }
  public void setPhotoUrls(List<String> photoUrls) {
    this.photoUrls = photoUrls;
  }

  
  /**
   **/
  public InlineResponse200 name(String name) {
    this.name = name;
    return this;
  }
  
  @ApiModelProperty(example = "doggie", value = "")
  @JsonProperty("name")
  public String getName() {
    return name;
  }
  public void setName(String name) {
    this.name = name;
  }

  
  /**
   **/
  public InlineResponse200 id(Long id) {
    this.id = id;
    return this;
  }
  
  @ApiModelProperty(example = "null", required = true, value = "")
  @JsonProperty("id")
  public Long getId() {
    return id;
  }
  public void setId(Long id) {
    this.id = id;
  }

  
  /**
   **/
  public InlineResponse200 category(Object category) {
    this.category = category;
    return this;
  }
  
  @ApiModelProperty(example = "null", value = "")
  @JsonProperty("category")
  public Object getCategory() {
    return category;
  }
  public void setCategory(Object category) {
    this.category = category;
  }

  
  /**
   **/
  public InlineResponse200 tags(List<Tag> tags) {
    this.tags = tags;
    return this;
  }
  
  @ApiModelProperty(example = "null", value = "")
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
  public InlineResponse200 status(StatusEnum status) {
    this.status = status;
    return this;
  }
  
  @ApiModelProperty(example = "null", value = "pet status in the store")
  @JsonProperty("status")
  public StatusEnum getStatus() {
    return status;
  }
  public void setStatus(StatusEnum status) {
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
    InlineResponse200 inlineResponse200 = (InlineResponse200) o;
    return Objects.equals(this.photoUrls, inlineResponse200.photoUrls) &&
        Objects.equals(this.name, inlineResponse200.name) &&
        Objects.equals(this.id, inlineResponse200.id) &&
        Objects.equals(this.category, inlineResponse200.category) &&
        Objects.equals(this.tags, inlineResponse200.tags) &&
        Objects.equals(this.status, inlineResponse200.status);
  }

  @Override
  public int hashCode() {
    return Objects.hash(photoUrls, name, id, category, tags, status);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class InlineResponse200 {\n");
    
    sb.append("    photoUrls: ").append(toIndentedString(photoUrls)).append("\n");
    sb.append("    name: ").append(toIndentedString(name)).append("\n");
    sb.append("    id: ").append(toIndentedString(id)).append("\n");
    sb.append("    category: ").append(toIndentedString(category)).append("\n");
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

