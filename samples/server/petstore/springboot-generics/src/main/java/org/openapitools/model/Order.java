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
 * Order
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.22.0-SNAPSHOT")
public class Order implements Serializable {

  private static final long serialVersionUID = 1L;

  private String orderId;

  private @Nullable Integer quantity;

  private @Nullable Double totalPrice;

  public Order() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public Order(String orderId) {
    this.orderId = orderId;
  }

  public Order orderId(String orderId) {
    this.orderId = orderId;
    return this;
  }

  /**
   * Get orderId
   * @return orderId
   */
  @NotNull 
  @JsonProperty("orderId")
  public String getOrderId() {
    return orderId;
  }

  @JsonProperty("orderId")
  public void setOrderId(String orderId) {
    this.orderId = orderId;
  }

  public Order quantity(@Nullable Integer quantity) {
    this.quantity = quantity;
    return this;
  }

  /**
   * Get quantity
   * @return quantity
   */
  
  @JsonProperty("quantity")
  public @Nullable Integer getQuantity() {
    return quantity;
  }

  @JsonProperty("quantity")
  public void setQuantity(@Nullable Integer quantity) {
    this.quantity = quantity;
  }

  public Order totalPrice(@Nullable Double totalPrice) {
    this.totalPrice = totalPrice;
    return this;
  }

  /**
   * Get totalPrice
   * @return totalPrice
   */
  
  @JsonProperty("totalPrice")
  public @Nullable Double getTotalPrice() {
    return totalPrice;
  }

  @JsonProperty("totalPrice")
  public void setTotalPrice(@Nullable Double totalPrice) {
    this.totalPrice = totalPrice;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Order order = (Order) o;
    return Objects.equals(this.orderId, order.orderId) &&
        Objects.equals(this.quantity, order.quantity) &&
        Objects.equals(this.totalPrice, order.totalPrice);
  }

  @Override
  public int hashCode() {
    return Objects.hash(orderId, quantity, totalPrice);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Order {\n");
    sb.append("    orderId: ").append(toIndentedString(orderId)).append("\n");
    sb.append("    quantity: ").append(toIndentedString(quantity)).append("\n");
    sb.append("    totalPrice: ").append(toIndentedString(totalPrice)).append("\n");
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

