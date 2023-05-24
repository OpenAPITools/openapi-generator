package org.openapitools.model;

import com.fasterxml.jackson.annotation.JsonTypeName;
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
import com.fasterxml.jackson.annotation.JsonTypeName;

/**
 * Must be named &#x60;File&#x60; for test.
 **/
@ApiModel(description = "Must be named `File` for test.")
@org.eclipse.microprofile.openapi.annotations.media.Schema(description="Must be named `File` for test.")
@JsonTypeName("File")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")
public class ModelFile  implements Serializable {
  private @Valid String sourceURI;

  protected ModelFile(ModelFileBuilder<?, ?> b) {
    this.sourceURI = b.sourceURI;
  }

  public ModelFile() {
  }

  /**
   * Test capitalization
   **/
  public ModelFile sourceURI(String sourceURI) {
    this.sourceURI = sourceURI;
    return this;
  }

  
  @ApiModelProperty(value = "Test capitalization")
  @org.eclipse.microprofile.openapi.annotations.media.Schema(description = "Test capitalization")
  @JsonProperty("sourceURI")
  public String getSourceURI() {
    return sourceURI;
  }

  @JsonProperty("sourceURI")
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


  public static ModelFileBuilder<?, ?> builder() {
    return new ModelFileBuilderImpl();
  }

  private static final class ModelFileBuilderImpl extends ModelFileBuilder<ModelFile, ModelFileBuilderImpl> {

    @Override
    protected ModelFileBuilderImpl self() {
      return this;
    }

    @Override
    public ModelFile build() {
      return new ModelFile(this);
    }
  }

  public static abstract class ModelFileBuilder<C extends ModelFile, B extends ModelFileBuilder<C, B>>  {
    private String sourceURI;
    protected abstract B self();

    public abstract C build();

    public B sourceURI(String sourceURI) {
      this.sourceURI = sourceURI;
      return self();
    }
  }
}

