package org.openapitools.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.ArrayList;
import java.util.List;
import org.openapitools.model.ModelFile;





@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaInflectorServerCodegen")
public class FileSchemaTestClass   {
  @JsonProperty("file")
  private ModelFile _file;

  @JsonProperty("files")
  private List<ModelFile> files = null;

  /**
   **/
  public FileSchemaTestClass _file(ModelFile _file) {
    this._file = _file;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("file")
  public ModelFile getFile() {
    return _file;
  }
  public void setFile(ModelFile _file) {
    this._file = _file;
  }

  /**
   **/
  public FileSchemaTestClass files(List<ModelFile> files) {
    this.files = files;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("files")
  public List<ModelFile> getFiles() {
    return files;
  }
  public void setFiles(List<ModelFile> files) {
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
    return Objects.equals(_file, fileSchemaTestClass._file) &&
        Objects.equals(files, fileSchemaTestClass.files);
  }

  @Override
  public int hashCode() {
    return Objects.hash(_file, files);
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
  private String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

