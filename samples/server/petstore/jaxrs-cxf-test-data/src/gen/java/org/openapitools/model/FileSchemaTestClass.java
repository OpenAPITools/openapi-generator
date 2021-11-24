package org.openapitools.model;

import java.util.ArrayList;
import java.util.List;
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
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;


public class FileSchemaTestClass  {
  
  @ApiModelProperty(value = "")
  @Valid
  private java.io.File file;

  @ApiModelProperty(value = "")
  @Valid
  private List<java.io.File> files = null;
 /**
  * Get file
  * @return file
  */
  @JsonProperty("file")
  public java.io.File getFile() {
    return file;
  }

  /**
   * Sets the <code>file</code> property.
   */
  public void setFile(java.io.File file) {
    this.file = file;
  }

  /**
   * Sets the <code>file</code> property.
   */
  public FileSchemaTestClass file(java.io.File file) {
    this.file = file;
    return this;
  }

 /**
  * Get files
  * @return files
  */
  @JsonProperty("files")
  public List<java.io.File> getFiles() {
    return files;
  }

  /**
   * Sets the <code>files</code> property.
   */
  public void setFiles(List<java.io.File> files) {
    this.files = files;
  }

  /**
   * Sets the <code>files</code> property.
   */
  public FileSchemaTestClass files(List<java.io.File> files) {
    this.files = files;
    return this;
  }

  /**
   * Adds a new item to the <code>files</code> list.
   */
  public FileSchemaTestClass addFilesItem(java.io.File filesItem) {
    this.files.add(filesItem);
    return this;
  }


  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class FileSchemaTestClass {\n");
    
    sb.append("    file: ").append(toIndentedString(file)).append("\n");
    sb.append("    files: ").append(toIndentedString(files)).append("\n");
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

