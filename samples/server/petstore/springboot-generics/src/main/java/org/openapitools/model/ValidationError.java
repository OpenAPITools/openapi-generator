package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.io.Serializable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * Validation error details
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.23.0-SNAPSHOT")
public class ValidationError implements Serializable {

  private static final long serialVersionUID = 1L;

  private String field;

  private String message;

  private @Nullable String code;

  public ValidationError() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public ValidationError(String field, String message) {
    this.field = field;
    this.message = message;
  }

  public ValidationError field(String field) {
    this.field = field;
    return this;
  }

  /**
   * Name of the field that failed validation
   * @return field
   */
  @NotNull 
  @JsonProperty("field")
  public String getField() {
    return field;
  }

  @JsonProperty("field")
  public void setField(String field) {
    this.field = field;
  }

  public ValidationError message(String message) {
    this.message = message;
    return this;
  }

  /**
   * Human-readable validation error message
   * @return message
   */
  @NotNull 
  @JsonProperty("message")
  public String getMessage() {
    return message;
  }

  @JsonProperty("message")
  public void setMessage(String message) {
    this.message = message;
  }

  public ValidationError code(@Nullable String code) {
    this.code = code;
    return this;
  }

  /**
   * Machine-readable error code
   * @return code
   */
  
  @JsonProperty("code")
  public @Nullable String getCode() {
    return code;
  }

  @JsonProperty("code")
  public void setCode(@Nullable String code) {
    this.code = code;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ValidationError validationError = (ValidationError) o;
    return Objects.equals(this.field, validationError.field) &&
        Objects.equals(this.message, validationError.message) &&
        Objects.equals(this.code, validationError.code);
  }

  @Override
  public int hashCode() {
    return Objects.hash(field, message, code);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ValidationError {\n");
    sb.append("    field: ").append(toIndentedString(field)).append("\n");
    sb.append("    message: ").append(toIndentedString(message)).append("\n");
    sb.append("    code: ").append(toIndentedString(code)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private String toIndentedString(@Nullable Object o) {
    return o == null ? "null" : o.toString().replace("\n", "\n    ");
  }
}

