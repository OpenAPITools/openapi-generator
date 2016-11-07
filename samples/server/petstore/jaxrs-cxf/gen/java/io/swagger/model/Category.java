package io.swagger.model;

import io.swagger.annotations.ApiModel;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;

@XmlAccessorType(XmlAccessType.FIELD)
 @XmlType(name = "Category", propOrder =
	{ "id", "name"
})

@XmlRootElement(name="Category")
public class Category  {
  

  @XmlElement(name="id")
  private Long id = null;

  @XmlElement(name="name")
  private String name = null;

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
  
  public String getName() {
    return name;
  }
  public void setName(String name) {
    this.name = name;
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Category {\n");
    
    sb.append("    id: ").append(toIndentedString(id)).append("\n");
    sb.append("    name: ").append(toIndentedString(name)).append("\n");
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

