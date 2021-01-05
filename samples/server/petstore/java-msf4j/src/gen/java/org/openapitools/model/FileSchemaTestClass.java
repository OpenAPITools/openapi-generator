package org.openapitools.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.ArrayList;
import java.util.List;

/**
 * FileSchemaTestClass
 */
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaMSF4JServerCodegen")
public class FileSchemaTestClass   {
  @JsonProperty("file")
  private java.io.File file;

  @JsonProperty("files")
  private List<java.io.File> files = null;

  public FileSchemaTestClass file(java.io.File file) {
    this.file = file;
    return this;
  }

   /**
   * Get file
   * @return file
  **/
  @ApiModelProperty(value = "")
  public java.io.File getFile() {
    return file;
  }

  public void setFile(java.io.File file) {
    this.file = file;
  }

  public FileSchemaTestClass files(List<java.io.File> files) {
    this.files = files;
    return this;
  }

  public FileSchemaTestClass addFilesItem(java.io.File filesItem) {
    if (this.files == null) {
      this.files = new ArrayList<java.io.File>();
    }
    this.files.add(filesItem);
    return this;
  }

   /**
   * Get files
   * @return files
  **/
  @ApiModelProperty(value = "")
  public List<java.io.File> getFiles() {
    return files;
  }

  public void setFiles(List<java.io.File> files) {
    this.files = files;
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
  private String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

