package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.openapitools.model.ModelFile;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.annotation.JsonTypeName;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;



@JsonTypeName("FileSchemaTestClass")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")    @XmlAccessorType(XmlAccessType.FIELD)
     @XmlType(name = "FileSchemaTestClass", propOrder =
    { "_file", "files"
    })
    
    @XmlRootElement(name="FileSchemaTestClass")

public class FileSchemaTestClass  implements Serializable {
  private ModelFile _file;
  private @Valid List<@Valid ModelFile> files = new ArrayList<>();

  protected FileSchemaTestClass(FileSchemaTestClassBuilder<?, ?> b) {
    this._file = b._file;
    this.files = b.files;
  }

  public FileSchemaTestClass() {
  }

  /**
   **/
  public FileSchemaTestClass _file(ModelFile _file) {
    this._file = _file;
    return this;
  }

      @XmlElement(name="file")
  
  @ApiModelProperty(value = "")
  @JsonProperty("file")
  @Valid public ModelFile getFile() {
    return _file;
  }

  @JsonProperty("file")
  public void setFile(ModelFile _file) {
    this._file = _file;
  }

  /**
   **/
  public FileSchemaTestClass files(List<@Valid ModelFile> files) {
    this.files = files;
    return this;
  }

      @XmlElement(name="files")
  
  @ApiModelProperty(value = "")
  @JsonProperty("files")
  @Valid public List<@Valid ModelFile> getFiles() {
    return files;
  }

  @JsonProperty("files")
  public void setFiles(List<@Valid ModelFile> files) {
    this.files = files;
  }

  public FileSchemaTestClass addFilesItem(ModelFile filesItem) {
    if (this.files == null) {
      this.files = new ArrayList<>();
    }

    this.files.add(filesItem);
    return this;
  }

  public FileSchemaTestClass removeFilesItem(ModelFile filesItem) {
    if (filesItem != null && this.files != null) {
      this.files.remove(filesItem);
    }

    return this;
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
    return Objects.equals(this._file, fileSchemaTestClass._file) &&
        Objects.equals(this.files, fileSchemaTestClass.files);
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


  public static FileSchemaTestClassBuilder<?, ?> builder() {
    return new FileSchemaTestClassBuilderImpl();
  }

  private static final class FileSchemaTestClassBuilderImpl extends FileSchemaTestClassBuilder<FileSchemaTestClass, FileSchemaTestClassBuilderImpl> {

    @Override
    protected FileSchemaTestClassBuilderImpl self() {
      return this;
    }

    @Override
    public FileSchemaTestClass build() {
      return new FileSchemaTestClass(this);
    }
  }

  public static abstract class FileSchemaTestClassBuilder<C extends FileSchemaTestClass, B extends FileSchemaTestClassBuilder<C, B>>  {
    private ModelFile _file;
    private List<ModelFile> files = new ArrayList<>();
    protected abstract B self();

    public abstract C build();

    public B _file(ModelFile _file) {
      this._file = _file;
      return self();
    }
    public B files(List<ModelFile> files) {
      this.files = files;
      return self();
    }
  }
}

