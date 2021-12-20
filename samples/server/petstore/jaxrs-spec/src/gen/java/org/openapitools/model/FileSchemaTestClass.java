package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.ArrayList;
import java.util.List;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.annotation.JsonTypeName;



@JsonTypeName("FileSchemaTestClass")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")public class FileSchemaTestClass  implements Serializable {
  
  private @Valid java.io.File file;
  private @Valid List<java.io.File> files = new ArrayList<java.io.File>();

  /**
   **/
  public FileSchemaTestClass file(java.io.File file) {
    this.file = file;
    return this;
  }

  

  
  @ApiModelProperty(value = "")
  @JsonProperty("file")
  public java.io.File getFile() {
    return file;
  }

  @JsonProperty("file")
  public void setFile(java.io.File file) {
    this.file = file;
  }

/**
   **/
  public FileSchemaTestClass files(List<java.io.File> files) {
    this.files = files;
    return this;
  }

  

  
  @ApiModelProperty(value = "")
  @JsonProperty("files")
  public List<java.io.File> getFiles() {
    return files;
  }

  @JsonProperty("files")
  public void setFiles(List<java.io.File> files) {
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

