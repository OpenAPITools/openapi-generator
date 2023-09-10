package org.openapitools.model;

import com.fasterxml.jackson.annotation.JsonTypeName;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.annotation.JsonTypeName;

/**
 * Model for testing model name starting with number
 **/

@org.eclipse.microprofile.openapi.annotations.media.Schema(description="Model for testing model name starting with number")
@JsonTypeName("200_response")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")
public class Model200Response  implements Serializable {
  private @Valid Integer name;
  private @Valid String propertyClass;

  protected Model200Response(Model200ResponseBuilder<?, ?> b) {
    this.name = b.name;
    this.propertyClass = b.propertyClass;
  }

  public Model200Response() {
  }

  /**
   **/
  public Model200Response name(Integer name) {
    this.name = name;
    return this;
  }

  
  @org.eclipse.microprofile.openapi.annotations.media.Schema(description = "")
  @JsonProperty("name")
  public Integer getName() {
    return name;
  }

  @JsonProperty("name")
  public void setName(Integer name) {
    this.name = name;
  }

  /**
   **/
  public Model200Response propertyClass(String propertyClass) {
    this.propertyClass = propertyClass;
    return this;
  }

  
  @org.eclipse.microprofile.openapi.annotations.media.Schema(description = "")
  @JsonProperty("class")
  public String getPropertyClass() {
    return propertyClass;
  }

  @JsonProperty("class")
  public void setPropertyClass(String propertyClass) {
    this.propertyClass = propertyClass;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Model200Response _200response = (Model200Response) o;
    return Objects.equals(this.name, _200response.name) &&
        Objects.equals(this.propertyClass, _200response.propertyClass);
  }

  @Override
  public int hashCode() {
    return Objects.hash(name, propertyClass);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Model200Response {\n");
    
    sb.append("    name: ").append(toIndentedString(name)).append("\n");
    sb.append("    propertyClass: ").append(toIndentedString(propertyClass)).append("\n");
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


  public static Model200ResponseBuilder<?, ?> builder() {
    return new Model200ResponseBuilderImpl();
  }

  private static final class Model200ResponseBuilderImpl extends Model200ResponseBuilder<Model200Response, Model200ResponseBuilderImpl> {

    @Override
    protected Model200ResponseBuilderImpl self() {
      return this;
    }

    @Override
    public Model200Response build() {
      return new Model200Response(this);
    }
  }

  public static abstract class Model200ResponseBuilder<C extends Model200Response, B extends Model200ResponseBuilder<C, B>>  {
    private Integer name;
    private String propertyClass;
    protected abstract B self();

    public abstract C build();

    public B name(Integer name) {
      this.name = name;
      return self();
    }
    public B propertyClass(String propertyClass) {
      this.propertyClass = propertyClass;
      return self();
    }
  }
}

