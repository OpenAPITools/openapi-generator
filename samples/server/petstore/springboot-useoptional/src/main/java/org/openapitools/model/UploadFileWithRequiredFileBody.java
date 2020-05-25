package org.openapitools.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.springframework.core.io.Resource;
import org.openapitools.jackson.nullable.JsonNullable;
import javax.validation.Valid;
import javax.validation.constraints.*;

/**
 * UploadFileWithRequiredFileBody
 */

public class UploadFileWithRequiredFileBody   {
  @JsonProperty("additionalMetadata")
  private String additionalMetadata;

  @JsonProperty("requiredFile")
  private Resource requiredFile;

  public UploadFileWithRequiredFileBody additionalMetadata(String additionalMetadata) {
    this.additionalMetadata = additionalMetadata;
    return this;
  }

  /**
   * Additional data to pass to server
   * @return additionalMetadata
  */
  @ApiModelProperty(value = "Additional data to pass to server")


  public String getAdditionalMetadata() {
    return additionalMetadata;
  }

  public void setAdditionalMetadata(String additionalMetadata) {
    this.additionalMetadata = additionalMetadata;
  }

  public UploadFileWithRequiredFileBody requiredFile(Resource requiredFile) {
    this.requiredFile = requiredFile;
    return this;
  }

  /**
   * file to upload
   * @return requiredFile
  */
  @ApiModelProperty(required = true, value = "file to upload")
  @NotNull

  @Valid

  public Resource getRequiredFile() {
    return requiredFile;
  }

  public void setRequiredFile(Resource requiredFile) {
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

