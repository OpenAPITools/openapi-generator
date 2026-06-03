package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import org.openapitools.model.Pet;
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
 * A non-generic model whose userResult property references a generic instance. Tests property-level substitution: userResult type → ApiResponse&lt;User&gt; while pet (a plain domain type) is left unchanged. 
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.23.0-SNAPSHOT")
public class OrderDetails implements Serializable {

  private static final long serialVersionUID = 1L;

  private @Nullable ApiResponse<User> userResult;

  private @Nullable Pet pet;

  private @Nullable String orderId;

  public OrderDetails userResult(@Nullable ApiResponse<User> userResult) {
    this.userResult = userResult;
    return this;
  }

  /**
   * Get userResult
   * @return userResult
   */
  @Valid 
  @JsonProperty("userResult")
  public @Nullable ApiResponse<User> getUserResult() {
    return userResult;
  }

  @JsonProperty("userResult")
  public void setUserResult(@Nullable ApiResponse<User> userResult) {
    this.userResult = userResult;
  }

  public OrderDetails pet(@Nullable Pet pet) {
    this.pet = pet;
    return this;
  }

  /**
   * Get pet
   * @return pet
   */
  @Valid 
  @JsonProperty("pet")
  public @Nullable Pet getPet() {
    return pet;
  }

  @JsonProperty("pet")
  public void setPet(@Nullable Pet pet) {
    this.pet = pet;
  }

  public OrderDetails orderId(@Nullable String orderId) {
    this.orderId = orderId;
    return this;
  }

  /**
   * Get orderId
   * @return orderId
   */
  
  @JsonProperty("orderId")
  public @Nullable String getOrderId() {
    return orderId;
  }

  @JsonProperty("orderId")
  public void setOrderId(@Nullable String orderId) {
    this.orderId = orderId;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    OrderDetails orderDetails = (OrderDetails) o;
    return Objects.equals(this.userResult, orderDetails.userResult) &&
        Objects.equals(this.pet, orderDetails.pet) &&
        Objects.equals(this.orderId, orderDetails.orderId);
  }

  @Override
  public int hashCode() {
    return Objects.hash(userResult, pet, orderId);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class OrderDetails {\n");
    sb.append("    userResult: ").append(toIndentedString(userResult)).append("\n");
    sb.append("    pet: ").append(toIndentedString(pet)).append("\n");
    sb.append("    orderId: ").append(toIndentedString(orderId)).append("\n");
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

