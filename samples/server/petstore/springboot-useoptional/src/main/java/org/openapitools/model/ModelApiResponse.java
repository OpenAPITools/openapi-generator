package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
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
 * ModelApiResponse
 */

@JsonTypeName("ApiResponse")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
public class ModelApiResponse {

  private Optional<Integer> code = Optional.empty();

  private Optional<String> type = Optional.empty();

  private Optional<String> message = Optional.empty();

  public ModelApiResponse code(Integer code) {
    this.code = Optional.ofNullable(code);
    return this;
  }

  /**
   * Get code
   * @return code
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("code")
  public Optional<Integer> getCode() {
    return code;
  }

  public void setCode(Optional<Integer> code) {
    this.code = code;
  }

  public ModelApiResponse type(String type) {
    this.type = Optional.ofNullable(type);
    return this;
  }

  /**
   * Get type
   * @return type
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("type")
  public Optional<String> getType() {
    return type;
  }

  public void setType(Optional<String> type) {
    this.type = type;
  }

  public ModelApiResponse message(String message) {
    this.message = Optional.ofNullable(message);
    return this;
  }

  /**
   * Get message
   * @return message
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("message")
  public Optional<String> getMessage() {
    return message;
  }

  public void setMessage(Optional<String> message) {
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

