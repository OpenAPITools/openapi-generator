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



public class Body5  implements Serializable {
  
  private @Valid String additionalMetadata = null;
  private @Valid File requiredFile = null;

  /**
   * Additional data to pass to server
   **/
  public Body5 additionalMetadata(String additionalMetadata) {
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
  public Body5 requiredFile(File requiredFile) {
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
    Body5 body5 = (Body5) o;
    return Objects.equals(additionalMetadata, body5.additionalMetadata) &&
        Objects.equals(requiredFile, body5.requiredFile);
  }

  @Override
  public int hashCode() {
    return Objects.hash(additionalMetadata, requiredFile);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Body5 {\n");
    
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

