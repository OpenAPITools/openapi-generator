package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * Must be named &#x60;File&#x60; for test.
 */

@ApiModel(description = "Must be named `File` for test.")
@JsonTypeName("File")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.6.0-SNAPSHOT")
public class FileDto {

  private String sourceURI;

  public FileDto sourceURI(String sourceURI) {
    this.sourceURI = sourceURI;
    return this;
  }

  /**
   * Test capitalization
   * @return sourceURI
  */
  
  @ApiModelProperty(value = "Test capitalization")
  @JsonProperty("sourceURI")
  public String getSourceURI() {
    return sourceURI;
  }

  public void setSourceURI(String sourceURI) {
    this.sourceURI = sourceURI;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    FileDto file = (FileDto) o;
    return Objects.equals(this.sourceURI, file.sourceURI);
  }

  @Override
  public int hashCode() {
    return Objects.hash(sourceURI);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class FileDto {\n");
    sb.append("    sourceURI: ").append(toIndentedString(sourceURI)).append("\n");
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
  
  public static class Builder {

    private FileDto instance;

    public Builder() {
      this(new FileDto());
    }

    protected Builder(FileDto instance) {
      this.instance = instance;
    }

    protected Builder copyOf(FileDto value) { 
      this.instance.setSourceURI(value.sourceURI);
      return this;
    }

    public FileDto.Builder sourceURI(String sourceURI) {
      this.instance.sourceURI(sourceURI);
      return this;
    }
    
    /**
    * returns a built FileDto instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public FileDto build() {
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
  public static FileDto.Builder builder() {
    return new FileDto.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public FileDto.Builder toBuilder() {
    FileDto.Builder builder = new FileDto.Builder();
    return builder.copyOf(this);
  }

}

