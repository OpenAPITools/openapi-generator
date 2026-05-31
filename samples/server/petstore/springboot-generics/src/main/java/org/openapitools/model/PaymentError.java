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
 * Payment processing error details
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.23.0-SNAPSHOT")
public class PaymentError implements Serializable {

  private static final long serialVersionUID = 1L;

  private String reason;

  private Double amount;

  private @Nullable Boolean retryable;

  public PaymentError() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public PaymentError(String reason, Double amount) {
    this.reason = reason;
    this.amount = amount;
  }

  public PaymentError reason(String reason) {
    this.reason = reason;
    return this;
  }

  /**
   * Reason for payment failure
   * @return reason
   */
  @NotNull 
  @JsonProperty("reason")
  public String getReason() {
    return reason;
  }

  @JsonProperty("reason")
  public void setReason(String reason) {
    this.reason = reason;
  }

  public PaymentError amount(Double amount) {
    this.amount = amount;
    return this;
  }

  /**
   * Amount that failed to process
   * @return amount
   */
  @NotNull 
  @JsonProperty("amount")
  public Double getAmount() {
    return amount;
  }

  @JsonProperty("amount")
  public void setAmount(Double amount) {
    this.amount = amount;
  }

  public PaymentError retryable(@Nullable Boolean retryable) {
    this.retryable = retryable;
    return this;
  }

  /**
   * Whether the payment can be retried
   * @return retryable
   */
  
  @JsonProperty("retryable")
  public @Nullable Boolean getRetryable() {
    return retryable;
  }

  @JsonProperty("retryable")
  public void setRetryable(@Nullable Boolean retryable) {
    this.retryable = retryable;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    PaymentError paymentError = (PaymentError) o;
    return Objects.equals(this.reason, paymentError.reason) &&
        Objects.equals(this.amount, paymentError.amount) &&
        Objects.equals(this.retryable, paymentError.retryable);
  }

  @Override
  public int hashCode() {
    return Objects.hash(reason, amount, retryable);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class PaymentError {\n");
    sb.append("    reason: ").append(toIndentedString(reason)).append("\n");
    sb.append("    amount: ").append(toIndentedString(amount)).append("\n");
    sb.append("    retryable: ").append(toIndentedString(retryable)).append("\n");
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

