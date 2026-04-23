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
 * Payload for a log entry
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.22.0-SNAPSHOT")
public class LogEntryData implements Serializable {

  private static final long serialVersionUID = 1L;

  private @Nullable String level;

  private @Nullable String message;

  private @Nullable String source;

  public LogEntryData level(@Nullable String level) {
    this.level = level;
    return this;
  }

  /**
   * Get level
   * @return level
   */
  
  @JsonProperty("level")
  public @Nullable String getLevel() {
    return level;
  }

  @JsonProperty("level")
  public void setLevel(@Nullable String level) {
    this.level = level;
  }

  public LogEntryData message(@Nullable String message) {
    this.message = message;
    return this;
  }

  /**
   * Get message
   * @return message
   */
  
  @JsonProperty("message")
  public @Nullable String getMessage() {
    return message;
  }

  @JsonProperty("message")
  public void setMessage(@Nullable String message) {
    this.message = message;
  }

  public LogEntryData source(@Nullable String source) {
    this.source = source;
    return this;
  }

  /**
   * Get source
   * @return source
   */
  
  @JsonProperty("source")
  public @Nullable String getSource() {
    return source;
  }

  @JsonProperty("source")
  public void setSource(@Nullable String source) {
    this.source = source;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    LogEntryData logEntryData = (LogEntryData) o;
    return Objects.equals(this.level, logEntryData.level) &&
        Objects.equals(this.message, logEntryData.message) &&
        Objects.equals(this.source, logEntryData.source);
  }

  @Override
  public int hashCode() {
    return Objects.hash(level, message, source);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class LogEntryData {\n");
    sb.append("    level: ").append(toIndentedString(level)).append("\n");
    sb.append("    message: ").append(toIndentedString(message)).append("\n");
    sb.append("    source: ").append(toIndentedString(source)).append("\n");
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

