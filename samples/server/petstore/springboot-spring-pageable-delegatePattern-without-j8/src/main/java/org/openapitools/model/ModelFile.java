package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.openapitools.jackson.nullable.JsonNullable;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;

/**
 * Must be named &#x60;File&#x60; for test.
 */
@ApiModel(description = "Must be named `File` for test.")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class ModelFile   {
  @JsonProperty("sourceURI")
  private String sourceURI;

  public ModelFile sourceURI(String sourceURI) {
    this.sourceURI = sourceURI;
    return this;
  }

  /**
   * Test capitalization
   * @return sourceURI
  */
  @ApiModelProperty(value = "Test capitalization")


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
    ModelFile _file = (ModelFile) o;
    return Objects.equals(this.sourceURI, _file.sourceURI);
  }

  @Override
  public int hashCode() {
    return Objects.hash(sourceURI);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ModelFile {\n");
    
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
}

