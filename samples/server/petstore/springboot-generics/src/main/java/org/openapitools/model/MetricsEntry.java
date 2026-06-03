package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import java.time.OffsetDateTime;
import org.openapitools.model.MetricsEntryData;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.io.Serializable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * Metrics entry wrapper. Same structure as LogEntry except &#39;data&#39; points to MetricsEntryData. These two form a Tier 3 cluster suggestion. 
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.23.0-SNAPSHOT")
public class MetricsEntry implements Serializable {

  private static final long serialVersionUID = 1L;

  private @Nullable MetricsEntryData data;

  private @Nullable String severity;

  @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME)
  private @Nullable OffsetDateTime timestamp;

  public MetricsEntry data(@Nullable MetricsEntryData data) {
    this.data = data;
    return this;
  }

  /**
   * Get data
   * @return data
   */
  @Valid 
  @JsonProperty("data")
  public @Nullable MetricsEntryData getData() {
    return data;
  }

  @JsonProperty("data")
  public void setData(@Nullable MetricsEntryData data) {
    this.data = data;
  }

  public MetricsEntry severity(@Nullable String severity) {
    this.severity = severity;
    return this;
  }

  /**
   * Get severity
   * @return severity
   */
  
  @JsonProperty("severity")
  public @Nullable String getSeverity() {
    return severity;
  }

  @JsonProperty("severity")
  public void setSeverity(@Nullable String severity) {
    this.severity = severity;
  }

  public MetricsEntry timestamp(@Nullable OffsetDateTime timestamp) {
    this.timestamp = timestamp;
    return this;
  }

  /**
   * Get timestamp
   * @return timestamp
   */
  @Valid 
  @JsonProperty("timestamp")
  public @Nullable OffsetDateTime getTimestamp() {
    return timestamp;
  }

  @JsonProperty("timestamp")
  public void setTimestamp(@Nullable OffsetDateTime timestamp) {
    this.timestamp = timestamp;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    MetricsEntry metricsEntry = (MetricsEntry) o;
    return Objects.equals(this.data, metricsEntry.data) &&
        Objects.equals(this.severity, metricsEntry.severity) &&
        Objects.equals(this.timestamp, metricsEntry.timestamp);
  }

  @Override
  public int hashCode() {
    return Objects.hash(data, severity, timestamp);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class MetricsEntry {\n");
    sb.append("    data: ").append(toIndentedString(data)).append("\n");
    sb.append("    severity: ").append(toIndentedString(severity)).append("\n");
    sb.append("    timestamp: ").append(toIndentedString(timestamp)).append("\n");
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

