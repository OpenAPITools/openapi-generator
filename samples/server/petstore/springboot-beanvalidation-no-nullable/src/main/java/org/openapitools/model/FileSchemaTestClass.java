package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * FileSchemaTestClass
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class FileSchemaTestClass   {

  @JsonProperty("file")
  private File file;

  @JsonProperty("files")
  @Valid
  private List<File> files = null;

  public FileSchemaTestClass file(File file) {
    this.file = file;
    return this;
  }

  /**
   * Get file
   * @return file
  */
  @Valid 
  @ApiModelProperty(value = "")
  public File getFile() {
    return file;
  }

  public void setFile(File file) {
    this.file = file;
  }

  public FileSchemaTestClass files(List<File> files) {
    this.files = files;
    return this;
  }

  public FileSchemaTestClass addFilesItem(File filesItem) {
    if (this.files == null) {
      this.files = new ArrayList<>();
    }
    this.files.add(filesItem);
    return this;
  }

  /**
   * Get files
   * @return files
  */
  @Valid 
  @ApiModelProperty(value = "")
  public List<File> getFiles() {
    return files;
  }

  public void setFiles(List<File> files) {
    this.files = files;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    FileSchemaTestClass fileSchemaTestClass = (FileSchemaTestClass) o;
    return Objects.equals(this.file, fileSchemaTestClass.file) &&
        Objects.equals(this.files, fileSchemaTestClass.files);
  }

  @Override
  public int hashCode() {
    return Objects.hash(file, files);
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

