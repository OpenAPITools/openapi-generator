package org.openapitools.client.model;


import io.swagger.annotations.*;
import com.google.gson.annotations.SerializedName;


@ApiModel(description = "")
public class InlineObject  {
  
  @SerializedName("name")
  private String name = null;
  @SerializedName("status")
  private String status = null;

  /**
   * Updated name of the pet
   **/
  @ApiModelProperty(value = "Updated name of the pet")
  public String getName() {
    return name;
  }
  public void setName(String name) {
    this.name = name;
  }

  /**
   * Updated status of the pet
   **/
  @ApiModelProperty(value = "Updated status of the pet")
  public String getStatus() {
    return status;
  }
  public void setStatus(String status) {
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
    InlineObject inlineObject = (InlineObject) o;
    return (this.name == null ? inlineObject.name == null : this.name.equals(inlineObject.name)) &&
        (this.status == null ? inlineObject.status == null : this.status.equals(inlineObject.status));
  }

  @Override
  public int hashCode() {
    int result = 17;
    result = 31 * result + (this.name == null ? 0: this.name.hashCode());
    result = 31 * result + (this.status == null ? 0: this.status.hashCode());
    return result;
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class InlineObject {\n");
    
    sb.append("  name: ").append(name).append("\n");
    sb.append("  status: ").append(status).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
