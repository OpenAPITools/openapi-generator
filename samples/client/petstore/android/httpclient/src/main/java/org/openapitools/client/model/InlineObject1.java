package org.openapitools.client.model;

import java.io.File;

import io.swagger.annotations.*;
import com.google.gson.annotations.SerializedName;


@ApiModel(description = "")
public class InlineObject1  {
  
  @SerializedName("additionalMetadata")
  private String additionalMetadata = null;
  @SerializedName("file")
  private File file = null;

  /**
   * Additional data to pass to server
   **/
  @ApiModelProperty(value = "Additional data to pass to server")
  public String getAdditionalMetadata() {
    return additionalMetadata;
  }
  public void setAdditionalMetadata(String additionalMetadata) {
    this.additionalMetadata = additionalMetadata;
  }

  /**
   * file to upload
   **/
  @ApiModelProperty(value = "file to upload")
  public File getFile() {
    return file;
  }
  public void setFile(File file) {
    this.file = file;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    InlineObject1 inlineObject1 = (InlineObject1) o;
    return (this.additionalMetadata == null ? inlineObject1.additionalMetadata == null : this.additionalMetadata.equals(inlineObject1.additionalMetadata)) &&
        (this.file == null ? inlineObject1.file == null : this.file.equals(inlineObject1.file));
  }

  @Override
  public int hashCode() {
    int result = 17;
    result = 31 * result + (this.additionalMetadata == null ? 0: this.additionalMetadata.hashCode());
    result = 31 * result + (this.file == null ? 0: this.file.hashCode());
    return result;
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class InlineObject1 {\n");
    
    sb.append("  additionalMetadata: ").append(additionalMetadata).append("\n");
    sb.append("  file: ").append(file).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
