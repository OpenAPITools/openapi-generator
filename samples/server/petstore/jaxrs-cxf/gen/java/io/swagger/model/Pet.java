package io.swagger.model;

import io.swagger.model.Category;
import io.swagger.model.Tag;
import java.util.ArrayList;
import java.util.List;



import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlEnum;

@XmlAccessorType(XmlAccessType.FIELD)
 @XmlType(name = "Pet", propOrder =
	{ "id", "category", "name", "photoUrls", "tags", "status"
})

@XmlRootElement(name="Pet")
public class Pet  {
  

  @XmlElement(name="id")
  private Long id = null;

  @XmlElement(name="category")
  private Category category = null;

  @XmlElement(name="name")
  private String name = null;

  @XmlElement(name="photoUrls")
  private List<String> photoUrls = new ArrayList<String>();

  @XmlElement(name="tags")
  private List<Tag> tags = new ArrayList<Tag>();

@XmlType(name="StatusEnum")
@XmlEnum
public enum StatusEnum {

    AVAILABLE(String.valueOf("&quot;available&quot;")), PENDING(String.valueOf("&quot;pending&quot;")), SOLD(String.valueOf("&quot;sold&quot;"));


    private String value;

    StatusEnum (String v) {
        value = v;
    }

    public String value() {
        return value;
    }

    public static StatusEnum fromValue(String v) {
        return valueOf(v);
    }
}

  @XmlElement(name="status")
  private StatusEnum status = null;

  /**
   **/
  
  public Long getId() {
    return id;
  }
  public void setId(Long id) {
    this.id = id;
  }
  /**
   **/
  
  public Category getCategory() {
    return category;
  }
  public void setCategory(Category category) {
    this.category = category;
  }
  /**
   **/
  
  public String getName() {
    return name;
  }
  public void setName(String name) {
    this.name = name;
  }
  /**
   **/
  
  public List<String> getPhotoUrls() {
    return photoUrls;
  }
  public void setPhotoUrls(List<String> photoUrls) {
    this.photoUrls = photoUrls;
  }
  /**
   **/
  
  public List<Tag> getTags() {
    return tags;
  }
  public void setTags(List<Tag> tags) {
    this.tags = tags;
  }
  /**
   * pet status in the store
   **/
  
  public StatusEnum getStatus() {
    return status;
  }
  public void setStatus(StatusEnum status) {
    this.status = status;
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

