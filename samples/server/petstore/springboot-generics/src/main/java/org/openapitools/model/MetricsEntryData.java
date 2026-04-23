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
 * Payload for a metrics entry
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.22.0-SNAPSHOT")
public class MetricsEntryData implements Serializable {

  private static final long serialVersionUID = 1L;

  private @Nullable String metricName;

  private @Nullable Double value;

  private @Nullable String unit;

  public MetricsEntryData metricName(@Nullable String metricName) {
    this.metricName = metricName;
    return this;
  }

  /**
   * Get metricName
   * @return metricName
   */
  
  @JsonProperty("metricName")
  public @Nullable String getMetricName() {
    return metricName;
  }

  @JsonProperty("metricName")
  public void setMetricName(@Nullable String metricName) {
    this.metricName = metricName;
  }

  public MetricsEntryData value(@Nullable Double value) {
    this.value = value;
    return this;
  }

  /**
   * Get value
   * @return value
   */
  
  @JsonProperty("value")
  public @Nullable Double getValue() {
    return value;
  }

  @JsonProperty("value")
  public void setValue(@Nullable Double value) {
    this.value = value;
  }

  public MetricsEntryData unit(@Nullable String unit) {
    this.unit = unit;
    return this;
  }

  /**
   * Get unit
   * @return unit
   */
  
  @JsonProperty("unit")
  public @Nullable String getUnit() {
    return unit;
  }

  @JsonProperty("unit")
  public void setUnit(@Nullable String unit) {
    this.unit = unit;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    MetricsEntryData metricsEntryData = (MetricsEntryData) o;
    return Objects.equals(this.metricName, metricsEntryData.metricName) &&
        Objects.equals(this.value, metricsEntryData.value) &&
        Objects.equals(this.unit, metricsEntryData.unit);
  }

  @Override
  public int hashCode() {
    return Objects.hash(metricName, value, unit);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class MetricsEntryData {\n");
    sb.append("    metricName: ").append(toIndentedString(metricName)).append("\n");
    sb.append("    value: ").append(toIndentedString(value)).append("\n");
    sb.append("    unit: ").append(toIndentedString(unit)).append("\n");
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

