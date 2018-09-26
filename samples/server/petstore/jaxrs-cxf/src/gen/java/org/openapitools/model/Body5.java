package org.openapitools.model;

import java.io.File;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.ApiModelProperty;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
import com.fasterxml.jackson.annotation.JsonProperty;

public class Body5  {
  
  @ApiModelProperty(value = "Additional data to pass to server")
 /**
   * Additional data to pass to server
  **/
  private String additionalMetadata = null;

  @ApiModelProperty(required = true, value = "file to upload")
 /**
   * file to upload
  **/
  private File requiredFile = null;
 /**
   * Additional data to pass to server
   * @return additionalMetadata
  **/
  @JsonProperty("additionalMetadata")
  public String getAdditionalMetadata() {
    return additionalMetadata;
  }

  public void setAdditionalMetadata(String additionalMetadata) {
    this.additionalMetadata = additionalMetadata;
  }

  public Body5 additionalMetadata(String additionalMetadata) {
    this.additionalMetadata = additionalMetadata;
    return this;
  }

 /**
   * file to upload
   * @return requiredFile
  **/
  @JsonProperty("requiredFile")
  @NotNull
  public File getRequiredFile() {
    return requiredFile;
  }

  public void setRequiredFile(File requiredFile) {
    this.requiredFile = requiredFile;
  }

  public Body5 requiredFile(File requiredFile) {
    this.requiredFile = requiredFile;
    return this;
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
  private static String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

