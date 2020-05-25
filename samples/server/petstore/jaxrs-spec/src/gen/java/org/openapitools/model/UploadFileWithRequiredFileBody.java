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



public class UploadFileWithRequiredFileBody  implements Serializable {
  
  private @Valid String additionalMetadata;
  private @Valid File requiredFile;

  /**
   * Additional data to pass to server
   **/
  public UploadFileWithRequiredFileBody additionalMetadata(String additionalMetadata) {
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
  public UploadFileWithRequiredFileBody requiredFile(File requiredFile) {
    this.requiredFile = requiredFile;
    return this;
  }

  
  @ApiModelProperty(required = true, value = "file to upload")
  @JsonProperty("requiredFile")
  @NotNull
  public File getRequiredFile() {
    return requiredFile;
  }
  public void setRequiredFile(File requiredFile) {
    this.requiredFile = requiredFile;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    UploadFileWithRequiredFileBody uploadFileWithRequiredFileBody = (UploadFileWithRequiredFileBody) o;
    return Objects.equals(this.additionalMetadata, uploadFileWithRequiredFileBody.additionalMetadata) &&
        Objects.equals(this.requiredFile, uploadFileWithRequiredFileBody.requiredFile);
  }

  @Override
  public int hashCode() {
    return Objects.hash(additionalMetadata, requiredFile);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class UploadFileWithRequiredFileBody {\n");
    
    sb.append("    additionalMetadata: ").append(toIndentedString(additionalMetadata)).append("\n");
    sb.append("    requiredFile: ").append(toIndentedString(requiredFile)).append("\n");
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

