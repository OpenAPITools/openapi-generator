package org.openapitools.model;

import java.util.ArrayList;
import java.util.List;
import org.openapitools.model.ModelFile;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.ApiModelProperty;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;


public class FileSchemaTestClass  {
  
  @ApiModelProperty(value = "")
  @Valid
  private ModelFile _file;

  @ApiModelProperty(value = "")
  @Valid
  private List<ModelFile> files = null;
 /**
  * Get _file
  * @return _file
  */
  @JsonProperty("file")
  public ModelFile getFile() {
    return _file;
  }

  /**
   * Sets the <code>_file</code> property.
   */
 public void setFile(ModelFile _file) {
    this._file = _file;
  }

  /**
   * Sets the <code>_file</code> property.
   */
  public FileSchemaTestClass _file(ModelFile _file) {
    this._file = _file;
    return this;
  }

 /**
  * Get files
  * @return files
  */
  @JsonProperty("files")
  public List<ModelFile> getFiles() {
    return files;
  }

  /**
   * Sets the <code>files</code> property.
   */
 public void setFiles(List<ModelFile> files) {
    this.files = files;
  }

  /**
   * Sets the <code>files</code> property.
   */
  public FileSchemaTestClass files(List<ModelFile> files) {
    this.files = files;
    return this;
  }

  /**
   * Adds a new item to the <code>files</code> list.
   */
  public FileSchemaTestClass addFilesItem(ModelFile filesItem) {
    this.files.add(filesItem);
    return this;
  }


  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class FileSchemaTestClass {\n");
    
    sb.append("    _file: ").append(toIndentedString(_file)).append("\n");
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

