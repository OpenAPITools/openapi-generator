package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;


/**
 * Must be named &#x60;Resource&#x60; for test.
 **/
@ApiModel(description = "Must be named `Resource` for test.")
public class Resource  implements Serializable {
  
  private @Valid String sourceURI;

  /**
   * Test resource
   **/
  public Resource sourceURI(String sourceURI) {
    this.sourceURI = sourceURI;
    return this;
  }

  
  @ApiModelProperty(value = "Test resource")
  @JsonProperty("sourceURI")
  public String getSourceURI() {
    return sourceURI;
  }
  public void setSourceURI(String sourceURI) {
    this.sourceURI = sourceURI;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Resource resource = (Resource) o;
    return Objects.equals(this.sourceURI, resource.sourceURI);
  }

  @Override
  public int hashCode() {
    return Objects.hash(sourceURI);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Resource {\n");
    
    sb.append("    sourceURI: ").append(toIndentedString(sourceURI)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

