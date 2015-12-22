package io.swagger.client.model;

import io.swagger.client.StringUtil;

import com.google.gson.annotations.SerializedName;


import java.util.Objects;

import io.swagger.annotations.*;



@ApiModel(description = "")
public class Tag   {
  
  @SerializedName("id")
  private Long id = null;
  
  @SerializedName("name")
  private String name = null;
  

  
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
  public String getName() {
    return name;
  }
  public void setName(String name) {
    this.name = name;
  }

  

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Tag tag = (Tag) o;
    return Objects.equals(id, tag.id) &&
        Objects.equals(name, tag.name);
  }

  @Override
  public int hashCode() {
    return Objects.hash(id, name);
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class Tag {\n");
    
    sb.append("    id: ").append(StringUtil.toIndentedString(id)).append("\n");
    sb.append("    name: ").append(StringUtil.toIndentedString(name)).append("\n");
    sb.append("}");
    return sb.toString();
  }
}
