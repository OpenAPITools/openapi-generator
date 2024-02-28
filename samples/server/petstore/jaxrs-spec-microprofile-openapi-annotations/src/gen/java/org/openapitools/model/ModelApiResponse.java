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



@org.eclipse.microprofile.openapi.annotations.media.Schema(description="")
@JsonTypeName("ApiResponse")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")
public class ModelApiResponse  implements Serializable {
  private @Valid Integer code;
  private @Valid String type;
  private @Valid String message;

  protected ModelApiResponse(ModelApiResponseBuilder<?, ?> b) {
    this.code = b.code;
    this.type = b.type;
    this.message = b.message;
  }

  public ModelApiResponse() {
  }

  /**
   **/
  public ModelApiResponse code(Integer code) {
    this.code = code;
    return this;
  }

  
  @org.eclipse.microprofile.openapi.annotations.media.Schema(description = "")
  @JsonProperty("code")
  public Integer getCode() {
    return code;
  }

  @JsonProperty("code")
  public void setCode(Integer code) {
    this.code = code;
  }

  /**
   **/
  public ModelApiResponse type(String type) {
    this.type = type;
    return this;
  }

  
  @org.eclipse.microprofile.openapi.annotations.media.Schema(description = "")
  @JsonProperty("type")
  public String getType() {
    return type;
  }

  @JsonProperty("type")
  public void setType(String type) {
    this.type = type;
  }

  /**
   **/
  public ModelApiResponse message(String message) {
    this.message = message;
    return this;
  }

  
  @org.eclipse.microprofile.openapi.annotations.media.Schema(description = "")
  @JsonProperty("message")
  public String getMessage() {
    return message;
  }

  @JsonProperty("message")
  public void setMessage(String message) {
    this.message = message;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ModelApiResponse _apiResponse = (ModelApiResponse) o;
    return Objects.equals(this.code, _apiResponse.code) &&
        Objects.equals(this.type, _apiResponse.type) &&
        Objects.equals(this.message, _apiResponse.message);
  }

  @Override
  public int hashCode() {
    return Objects.hash(code, type, message);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ModelApiResponse {\n");
    
    sb.append("    code: ").append(toIndentedString(code)).append("\n");
    sb.append("    type: ").append(toIndentedString(type)).append("\n");
    sb.append("    message: ").append(toIndentedString(message)).append("\n");
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


  public static ModelApiResponseBuilder<?, ?> builder() {
    return new ModelApiResponseBuilderImpl();
  }

  private static final class ModelApiResponseBuilderImpl extends ModelApiResponseBuilder<ModelApiResponse, ModelApiResponseBuilderImpl> {

    @Override
    protected ModelApiResponseBuilderImpl self() {
      return this;
    }

    @Override
    public ModelApiResponse build() {
      return new ModelApiResponse(this);
    }
  }

  public static abstract class ModelApiResponseBuilder<C extends ModelApiResponse, B extends ModelApiResponseBuilder<C, B>>  {
    private Integer code;
    private String type;
    private String message;
    protected abstract B self();

    public abstract C build();

    public B code(Integer code) {
      this.code = code;
      return self();
    }
    public B type(String type) {
      this.type = type;
      return self();
    }
    public B message(String message) {
      this.message = message;
      return self();
    }
  }
}

