package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import java.util.Optional;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import javax.annotation.Generated;

/**
 * ModelApiResponse
 */

@JsonTypeName("ApiResponse")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class ModelApiResponse   {

  @JsonProperty("code")
  private Optional<Integer> code = Optional.empty();

  @JsonProperty("type")
  private Optional<String> type = Optional.empty();

  @JsonProperty("message")
  private Optional<String> message = Optional.empty();

  public ModelApiResponse code(Integer code) {
    this.code = Optional.ofNullable(code);
    return this;
  }

  /**
   * Get code
   * @return code
  */
  
  @Schema(name = "code", required = false)
  public Optional<Integer> getCode() {
    return code;
  }

  public void setCode(Integer code) {
    this.code = Optional.ofNullable(code);
  }

  public ModelApiResponse type(String type) {
    this.type = Optional.ofNullable(type);
    return this;
  }

  /**
   * Get type
   * @return type
  */
  
  @Schema(name = "type", required = false)
  public Optional<String> getType() {
    return type;
  }

  public void setType(String type) {
    this.type = Optional.ofNullable(type);
  }

  public ModelApiResponse message(String message) {
    this.message = Optional.ofNullable(message);
    return this;
  }

  /**
   * Get message
   * @return message
  */
  
  @Schema(name = "message", required = false)
  public Optional<String> getMessage() {
    return message;
  }

  public void setMessage(String message) {
    this.message = Optional.ofNullable(message);
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
}

