package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.springframework.lang.Nullable;
import org.openapitools.model.User;
import org.openapitools.configuration.ApiResponse;
import org.openapitools.jackson.nullable.JsonNullable;
import java.io.Serializable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * A non-generic model with an array property of a generic-instance type. Tests array property substitution: responses type → List&lt;ApiResponse&lt;User&gt;&gt; 
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.23.0-SNAPSHOT")
public class NotificationBatch implements Serializable {

  private static final long serialVersionUID = 1L;

  private List<@Valid ApiResponse<User>> responses = new ArrayList<>();

  private @Nullable String batchId;

  public NotificationBatch responses(List<@Valid ApiResponse<User>> responses) {
    this.responses = responses;
    return this;
  }

  public NotificationBatch addResponsesItem(ApiResponse<User> responsesItem) {
    if (this.responses == null) {
      this.responses = new ArrayList<>();
    }
    this.responses.add(responsesItem);
    return this;
  }

  /**
   * Get responses
   * @return responses
   */
  @Valid 
  @JsonProperty("responses")
  public List<@Valid ApiResponse<User>> getResponses() {
    return responses;
  }

  @JsonProperty("responses")
  public void setResponses(List<@Valid ApiResponse<User>> responses) {
    this.responses = responses;
  }

  public NotificationBatch batchId(@Nullable String batchId) {
    this.batchId = batchId;
    return this;
  }

  /**
   * Get batchId
   * @return batchId
   */
  
  @JsonProperty("batchId")
  public @Nullable String getBatchId() {
    return batchId;
  }

  @JsonProperty("batchId")
  public void setBatchId(@Nullable String batchId) {
    this.batchId = batchId;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    NotificationBatch notificationBatch = (NotificationBatch) o;
    return Objects.equals(this.responses, notificationBatch.responses) &&
        Objects.equals(this.batchId, notificationBatch.batchId);
  }

  @Override
  public int hashCode() {
    return Objects.hash(responses, batchId);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class NotificationBatch {\n");
    sb.append("    responses: ").append(toIndentedString(responses)).append("\n");
    sb.append("    batchId: ").append(toIndentedString(batchId)).append("\n");
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

