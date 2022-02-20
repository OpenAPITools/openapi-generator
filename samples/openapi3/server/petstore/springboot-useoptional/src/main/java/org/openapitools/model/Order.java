package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import java.time.OffsetDateTime;
import java.util.Optional;
import org.springframework.format.annotation.DateTimeFormat;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import javax.annotation.Generated;

/**
 * Order
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class Order   {

  @JsonProperty("id")
  private Optional<Long> id = Optional.empty();

  @JsonProperty("petId")
  private Optional<Long> petId = Optional.empty();

  @JsonProperty("quantity")
  private Optional<Integer> quantity = Optional.empty();

  @JsonProperty("shipDate")
  @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME)
  private Optional<OffsetDateTime> shipDate = Optional.empty();

  /**
   * Order Status
   */
  public enum StatusEnum {
    PLACED("placed"),
    
    APPROVED("approved"),
    
    DELIVERED("delivered");

    private String value;

    StatusEnum(String value) {
      this.value = value;
    }

    @JsonValue
    public String getValue() {
      return value;
    }

    @Override
    public String toString() {
      return String.valueOf(value);
    }

    @JsonCreator
    public static StatusEnum fromValue(String value) {
      for (StatusEnum b : StatusEnum.values()) {
        if (b.value.equals(value)) {
          return b;
        }
      }
      throw new IllegalArgumentException("Unexpected value '" + value + "'");
    }
  }

  @JsonProperty("status")
  private Optional<StatusEnum> status = Optional.empty();

  @JsonProperty("complete")
  private Optional<Boolean> complete = Optional.of(false);

  public Order id(Long id) {
    this.id = Optional.ofNullable(id);
    return this;
  }

  /**
   * Get id
   * @return id
  */
  @Schema(name = "id", required = false)
  public Optional<Long> getId() {
    return id;
  }

  @JsonIgnore
  public void setId(Long id) {
    this.id = Optional.ofNullable(id);
  }

  public Order petId(Long petId) {
    this.petId = Optional.ofNullable(petId);
    return this;
  }

  /**
   * Get petId
   * @return petId
  */
  @Schema(name = "petId", required = false)
  public Optional<Long> getPetId() {
    return petId;
  }

  @JsonIgnore
  public void setPetId(Long petId) {
    this.petId = Optional.ofNullable(petId);
  }

  public Order quantity(Integer quantity) {
    this.quantity = Optional.ofNullable(quantity);
    return this;
  }

  /**
   * Get quantity
   * @return quantity
  */
  @Schema(name = "quantity", required = false)
  public Optional<Integer> getQuantity() {
    return quantity;
  }

  @JsonIgnore
  public void setQuantity(Integer quantity) {
    this.quantity = Optional.ofNullable(quantity);
  }

  public Order shipDate(OffsetDateTime shipDate) {
    this.shipDate = Optional.ofNullable(shipDate);
    return this;
  }

  /**
   * Get shipDate
   * @return shipDate
  */
  @Schema(name = "shipDate", required = false)
  public Optional<OffsetDateTime> getShipDate() {
    return shipDate;
  }

  @JsonIgnore
  public void setShipDate(OffsetDateTime shipDate) {
    this.shipDate = Optional.ofNullable(shipDate);
  }

  public Order status(StatusEnum status) {
    this.status = Optional.ofNullable(status);
    return this;
  }

  /**
   * Order Status
   * @return status
  */
  @Schema(name = "status", description = "Order Status", required = false)
  public Optional<StatusEnum> getStatus() {
    return status;
  }

  @JsonIgnore
  public void setStatus(StatusEnum status) {
    this.status = Optional.ofNullable(status);
  }

  public Order complete(Boolean complete) {
    this.complete = Optional.ofNullable(complete);
    return this;
  }

  /**
   * Get complete
   * @return complete
  */
  @Schema(name = "complete", required = false)
  public Optional<Boolean> getComplete() {
    return complete;
  }

  @JsonIgnore
  public void setComplete(Boolean complete) {
    this.complete = Optional.ofNullable(complete);
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
    return Objects.equals(this.id, order.id) &&
        Objects.equals(this.petId, order.petId) &&
        Objects.equals(this.quantity, order.quantity) &&
        Objects.equals(this.shipDate, order.shipDate) &&
        Objects.equals(this.status, order.status) &&
        Objects.equals(this.complete, order.complete);
  }

  @Override
  public int hashCode() {
    return Objects.hash(id, petId, quantity, shipDate, status, complete);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Order {\n");
    sb.append("    id: ").append(toIndentedString(id)).append("\n");
    sb.append("    petId: ").append(toIndentedString(petId)).append("\n");
    sb.append("    quantity: ").append(toIndentedString(quantity)).append("\n");
    sb.append("    shipDate: ").append(toIndentedString(shipDate)).append("\n");
    sb.append("    status: ").append(toIndentedString(status)).append("\n");
    sb.append("    complete: ").append(toIndentedString(complete)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

