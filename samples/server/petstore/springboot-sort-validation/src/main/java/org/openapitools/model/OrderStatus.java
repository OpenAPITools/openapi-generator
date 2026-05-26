package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.Set;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.io.Serializable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * OrderStatus
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.23.0-SNAPSHOT")
public class OrderStatus implements Serializable {

  private static final long serialVersionUID = 1L;

  private Set<String> statuses = new LinkedHashSet<>();

  public OrderStatus statuses(Set<String> statuses) {
    this.statuses = statuses;
    return this;
  }

  public OrderStatus addStatusesItem(String statusesItem) {
    if (this.statuses == null) {
      this.statuses = new LinkedHashSet<>();
    }
    this.statuses.add(statusesItem);
    return this;
  }

  /**
   * Get statuses
   * @return statuses
   */
  
  @JsonProperty("statuses")
  public Set<String> getStatuses() {
    return statuses;
  }

  @JsonDeserialize(as = LinkedHashSet.class)
  @JsonProperty("statuses")
  public void setStatuses(Set<String> statuses) {
    this.statuses = statuses;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    OrderStatus orderStatus = (OrderStatus) o;
    return Objects.equals(this.statuses, orderStatus.statuses);
  }

  @Override
  public int hashCode() {
    return Objects.hash(statuses);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class OrderStatus {\n");
    sb.append("    statuses: ").append(toIndentedString(statuses)).append("\n");
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

