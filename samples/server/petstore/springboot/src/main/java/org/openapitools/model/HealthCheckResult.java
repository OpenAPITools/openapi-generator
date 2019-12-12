package org.openapitools.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.v3.oas.annotations.media.Schema;
import org.openapitools.jackson.nullable.JsonNullable;
import javax.validation.Valid;
import javax.validation.constraints.*;

/**
 * Just a string to inform instance is up and running. Make it nullable in hope to get it as pointer in generated model.
 */
@Schema(description = "Just a string to inform instance is up and running. Make it nullable in hope to get it as pointer in generated model.")

public class HealthCheckResult   {
  @JsonProperty("NullableMessage")
  private JsonNullable<String> nullableMessage = JsonNullable.undefined();

  public HealthCheckResult nullableMessage(String nullableMessage) {
    this.nullableMessage = JsonNullable.of(nullableMessage);
    return this;
  }

  /**
   * Get nullableMessage
   * @return nullableMessage
  */
  @Schema(description = "")


  public JsonNullable<String> getNullableMessage() {
    return nullableMessage;
  }

  public void setNullableMessage(JsonNullable<String> nullableMessage) {
    this.nullableMessage = nullableMessage;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    HealthCheckResult healthCheckResult = (HealthCheckResult) o;
    return Objects.equals(this.nullableMessage, healthCheckResult.nullableMessage);
  }

  @Override
  public int hashCode() {
    return Objects.hash(nullableMessage);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class HealthCheckResult {\n");
    
    sb.append("    nullableMessage: ").append(toIndentedString(nullableMessage)).append("\n");
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

