package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.io.File;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;



public class UploadFileBody  implements Serializable {
  
  private @Valid String additionalMetadata;
  private @Valid File file;

  /**
   * Additional data to pass to server
   **/
  public UploadFileBody additionalMetadata(String additionalMetadata) {
    this.additionalMetadata = additionalMetadata;
    return this;
  }

  
  @ApiModelProperty(value = "Additional data to pass to server")
  @JsonProperty("additionalMetadata")
  public String getAdditionalMetadata() {
    return additionalMetadata;
  }
  public void setAdditionalMetadata(String additionalMetadata) {
    this.additionalMetadata = additionalMetadata;
  }

  /**
   * file to upload
   **/
  public UploadFileBody file(File file) {
    this.file = file;
    return this;
  }

  
  @ApiModelProperty(value = "file to upload")
  @JsonProperty("file")
  public File getFile() {
    return file;
  }
  public void setFile(File file) {
    this.file = file;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    UploadFileBody uploadFileBody = (UploadFileBody) o;
    return Objects.equals(this.additionalMetadata, uploadFileBody.additionalMetadata) &&
        Objects.equals(this.file, uploadFileBody.file);
  }

  @Override
  public int hashCode() {
    return Objects.hash(additionalMetadata, file);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class UploadFileBody {\n");
    
    sb.append("    additionalMetadata: ").append(toIndentedString(additionalMetadata)).append("\n");
    sb.append("    file: ").append(toIndentedString(file)).append("\n");
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

