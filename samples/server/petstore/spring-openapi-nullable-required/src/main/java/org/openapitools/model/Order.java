package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import java.time.OffsetDateTime;
import java.util.Arrays;
import org.openapitools.jackson.nullable.JsonNullable;
import org.springframework.format.annotation.DateTimeFormat;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import javax.annotation.Generated;

/**
 * An order for a pets from the pet store
 */

@Schema(name = "Order", description = "An order for a pets from the pet store")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class Order {

  @JsonProperty("id")
  private JsonNullable<Long> id = JsonNullable.undefined();

  @JsonProperty("petId")
  private JsonNullable<Long> petId = JsonNullable.undefined();

  @JsonProperty("quantity")
  private JsonNullable<Integer> quantity = JsonNullable.undefined();

  @JsonProperty("shipDate")
  @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME)
  private JsonNullable<OffsetDateTime> shipDate = JsonNullable.undefined();

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
  private JsonNullable<StatusEnum> status = JsonNullable.undefined();

  @JsonProperty("complete")
  private JsonNullable<Boolean> complete = JsonNullable.undefined();

  public Order id(Long id) {
    this.id = JsonNullable.of(id);
    return this;
  }

  /**
   * Get id
   * @return id
  */
  @NotNull 
  @Schema(name = "id", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  public JsonNullable<Long> getId() {
    return id;
  }

  public void setId(JsonNullable<Long> id) {
    this.id = id;
  }

  public Order petId(Long petId) {
    this.petId = JsonNullable.of(petId);
    return this;
  }

  /**
   * Get petId
   * @return petId
  */
  @NotNull 
  @Schema(name = "petId", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  public JsonNullable<Long> getPetId() {
    return petId;
  }

  public void setPetId(JsonNullable<Long> petId) {
    this.petId = petId;
  }

  public Order quantity(Integer quantity) {
    this.quantity = JsonNullable.of(quantity);
    return this;
  }

  /**
   * Get quantity
   * @return quantity
  */
  @NotNull 
  @Schema(name = "quantity", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  public JsonNullable<Integer> getQuantity() {
    return quantity;
  }

  public void setQuantity(JsonNullable<Integer> quantity) {
    this.quantity = quantity;
  }

  public Order shipDate(OffsetDateTime shipDate) {
    this.shipDate = JsonNullable.of(shipDate);
    return this;
  }

  /**
   * Get shipDate
   * @return shipDate
  */
  @NotNull @Valid 
  @Schema(name = "shipDate", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  public JsonNullable<OffsetDateTime> getShipDate() {
    return shipDate;
  }

  public void setShipDate(JsonNullable<OffsetDateTime> shipDate) {
    this.shipDate = shipDate;
  }

  public Order status(StatusEnum status) {
    this.status = JsonNullable.of(status);
    return this;
  }

  /**
   * Order Status
   * @return status
  */
  @NotNull 
  @Schema(name = "status", description = "Order Status", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  public JsonNullable<StatusEnum> getStatus() {
    return status;
  }

  public void setStatus(JsonNullable<StatusEnum> status) {
    this.status = status;
  }

  public Order complete(Boolean complete) {
    this.complete = JsonNullable.of(complete);
    return this;
  }

  /**
   * Get complete
   * @return complete
  */
  @NotNull 
  @Schema(name = "complete", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  public JsonNullable<Boolean> getComplete() {
    return complete;
  }

  public void setComplete(JsonNullable<Boolean> complete) {
    this.complete = complete;
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

  private static <T> boolean equalsNullable(JsonNullable<T> a, JsonNullable<T> b) {
    return a == b || (a != null && b != null && a.isPresent() && b.isPresent() && Objects.deepEquals(a.get(), b.get()));
  }

  @Override
  public int hashCode() {
    return Objects.hash(id, petId, quantity, shipDate, status, complete);
  }

  private static <T> int hashCodeNullable(JsonNullable<T> a) {
    if (a == null) {
      return 1;
    }
    return a.isPresent() ? Arrays.deepHashCode(new Object[]{a.get()}) : 31;
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

