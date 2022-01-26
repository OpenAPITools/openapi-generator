package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.ApiModelProperty;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;


/**
 * Must be named `File` for test.
 */
@ApiModel(description="Must be named `File` for test.")

public class ModelFile  {
  
 /**
  * Test capitalization
  */
  @ApiModelProperty(value = "Test capitalization")
  private String sourceURI;
 /**
  * Test capitalization
  * @return sourceURI
  */
  @JsonProperty("sourceURI")
  public String getSourceURI() {
    return sourceURI;
  }

  /**
   * Sets the <code>sourceURI</code> property.
   */
 public void setSourceURI(String sourceURI) {
    this.sourceURI = sourceURI;
  }

  /**
   * Sets the <code>sourceURI</code> property.
   */
  public ModelFile sourceURI(String sourceURI) {
    this.sourceURI = sourceURI;
    return this;
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
  private static String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

