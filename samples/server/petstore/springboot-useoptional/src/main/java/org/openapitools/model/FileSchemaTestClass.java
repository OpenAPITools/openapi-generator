package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * FileSchemaTestClass
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.21.0-SNAPSHOT")
public class FileSchemaTestClass {

  private Optional<File> file = Optional.empty();

  @Valid
  private List<@Valid File> files = new ArrayList<>();

  public FileSchemaTestClass file(File file) {
    this.file = Optional.ofNullable(file);
    return this;
  }

  /**
   * Get file
   * @return file
   */
  @Valid 
  @Schema(name = "file", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("file")
  public Optional<File> getFile() {
    return file;
  }

  @JsonProperty("file")
  public void setFile(Optional<File> file) {
    this.file = file;
  }

  public FileSchemaTestClass files(List<@Valid File> files) {
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
  @Schema(name = "files", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("files")
  public List<@Valid File> getFiles() {
    return files;
  }

  @JsonProperty("files")
  public void setFiles(List<@Valid File> files) {
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
    return o == null ? "null" : o.toString().replace("\n", "\n    ");
  }
  
  public static class Builder {

    private FileSchemaTestClass instance;

    public Builder() {
      this(new FileSchemaTestClass());
    }

    protected Builder(FileSchemaTestClass instance) {
      this.instance = instance;
    }

    protected Builder copyOf(FileSchemaTestClass value) { 
      this.instance.setFile(value.file);
      this.instance.setFiles(value.files);
      return this;
    }

    public FileSchemaTestClass.Builder file(File file) {
      this.instance.file(file);
      return this;
    }
    
    public FileSchemaTestClass.Builder files(List<File> files) {
      this.instance.files(files);
      return this;
    }
    
    /**
    * returns a built FileSchemaTestClass instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public FileSchemaTestClass build() {
      try {
        return this.instance;
      } finally {
        // ensure that this.instance is not reused
        this.instance = null;
      }
    }

    @Override
    public String toString() {
      return getClass() + "=(" + instance + ")";
    }
  }

  /**
  * Create a builder with no initialized field (except for the default values).
  */
  public static FileSchemaTestClass.Builder builder() {
    return new FileSchemaTestClass.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public FileSchemaTestClass.Builder toBuilder() {
    FileSchemaTestClass.Builder builder = new FileSchemaTestClass.Builder();
    return builder.copyOf(this);
  }

}

