package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.springframework.lang.Nullable;
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
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.14.0-SNAPSHOT")
public class File {

  private Optional<String> sourceURI = Optional.empty();

  public File sourceURI(String sourceURI) {
    this.sourceURI = Optional.ofNullable(sourceURI);
    return this;
  }

  /**
   * Test capitalization
   * @return sourceURI
   */
  
  @ApiModelProperty(value = "Test capitalization")
  @JsonProperty("sourceURI")
  public Optional<String> getSourceURI() {
    return sourceURI;
  }

  public void setSourceURI(Optional<String> sourceURI) {
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
    File file = (File) o;
    return Objects.equals(this.sourceURI, file.sourceURI);
  }

  @Override
  public int hashCode() {
    return Objects.hash(sourceURI);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class File {\n");
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

    private File instance;

    public Builder() {
      this(new File());
    }

    protected Builder(File instance) {
      this.instance = instance;
    }

    protected Builder copyOf(File value) { 
      this.instance.setSourceURI(value.sourceURI);
      return this;
    }

    public File.Builder sourceURI(String sourceURI) {
      this.instance.sourceURI(sourceURI);
      return this;
    }
    
    /**
    * returns a built File instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public File build() {
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
  public static File.Builder builder() {
    return new File.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public File.Builder toBuilder() {
    File.Builder builder = new File.Builder();
    return builder.copyOf(this);
  }

}

