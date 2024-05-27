package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlRootElement;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import io.swagger.v3.oas.annotations.media.Schema;

import jakarta.xml.bind.annotation.*;

import java.util.*;
import jakarta.annotation.Generated;

/**
 * Describes the result of uploading an image resource
 */

@Schema(name = "ApiResponse", description = "Describes the result of uploading an image resource")
@JsonTypeName("ApiResponse")
@JacksonXmlRootElement(localName = "ModelApiResponse")
@XmlRootElement(name = "ModelApiResponse")
@XmlAccessorType(XmlAccessType.FIELD)

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.7.0-SNAPSHOT")
public class ModelApiResponse {

  private Integer code;

  private String type;

  private String message;

  public ModelApiResponse() {
    super();
  }

  /**
  * Constructor with all args parameters
  */
  public ModelApiResponse(Integer code, String type, String message) {
      this.code = code;
      this.type = type;
      this.message = message;
  }

  public ModelApiResponse code(Integer code) {
    this.code = code;
    return this;
  }

  /**
   * Get code
   * @return code
  */
  
  @Schema(name = "code", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("code")
  @JacksonXmlProperty(localName = "code")
  public Integer getCode() {
    return code;
  }

  public void setCode(Integer code) {
    this.code = code;
  }

  public ModelApiResponse type(String type) {
    this.type = type;
    return this;
  }

  /**
   * Get type
   * @return type
  */
  
  @Schema(name = "type", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("type")
  @JacksonXmlProperty(localName = "type")
  public String getType() {
    return type;
  }

  public void setType(String type) {
    this.type = type;
  }

  public ModelApiResponse message(String message) {
    this.message = message;
    return this;
  }

  /**
   * Get message
   * @return message
  */
  
  @Schema(name = "message", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("message")
  @JacksonXmlProperty(localName = "message")
  public String getMessage() {
    return message;
  }

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
  
  public static class Builder {

    private ModelApiResponse instance;

    public Builder() {
      this(new ModelApiResponse());
    }

    protected Builder(ModelApiResponse instance) {
      this.instance = instance;
    }

    protected Builder copyOf(ModelApiResponse value) { 
      this.instance.setCode(value.code);
      this.instance.setType(value.type);
      this.instance.setMessage(value.message);
      return this;
    }

    public ModelApiResponse.Builder code(Integer code) {
      this.instance.code(code);
      return this;
    }
    
    public ModelApiResponse.Builder type(String type) {
      this.instance.type(type);
      return this;
    }
    
    public ModelApiResponse.Builder message(String message) {
      this.instance.message(message);
      return this;
    }
    
    /**
    * returns a built ModelApiResponse instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public ModelApiResponse build() {
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
  public static ModelApiResponse.Builder builder() {
    return new ModelApiResponse.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public ModelApiResponse.Builder toBuilder() {
    ModelApiResponse.Builder builder = new ModelApiResponse.Builder();
    return builder.copyOf(this);
  }

}

