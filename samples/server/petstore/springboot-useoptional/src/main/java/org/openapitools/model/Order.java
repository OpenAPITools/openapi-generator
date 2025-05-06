package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.time.OffsetDateTime;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * Order
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.14.0-SNAPSHOT")
public class Order {

  private Optional<Long> id = Optional.empty();

  private Optional<Long> petId = Optional.empty();

  private Optional<Integer> quantity = Optional.empty();

  @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME)
  private Optional<OffsetDateTime> shipDate = Optional.empty();

  /**
   * Order Status
   */
  public enum StatusEnum {
    PLACED("placed"),
    
    APPROVED("approved"),
    
    DELIVERED("delivered");

    private final String value;

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

  private Optional<StatusEnum> status = Optional.empty();

  private Optional<Boolean> complete = Optional.of(false);

  public Order id(Long id) {
    this.id = Optional.ofNullable(id);
    return this;
  }

  /**
   * Get id
   * @return id
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("id")
  public Optional<Long> getId() {
    return id;
  }

  public void setId(Optional<Long> id) {
    this.id = id;
  }

  public Order petId(Long petId) {
    this.petId = Optional.ofNullable(petId);
    return this;
  }

  /**
   * Get petId
   * @return petId
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("petId")
  public Optional<Long> getPetId() {
    return petId;
  }

  public void setPetId(Optional<Long> petId) {
    this.petId = petId;
  }

  public Order quantity(Integer quantity) {
    this.quantity = Optional.ofNullable(quantity);
    return this;
  }

  /**
   * Get quantity
   * @return quantity
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("quantity")
  public Optional<Integer> getQuantity() {
    return quantity;
  }

  public void setQuantity(Optional<Integer> quantity) {
    this.quantity = quantity;
  }

  public Order shipDate(OffsetDateTime shipDate) {
    this.shipDate = Optional.ofNullable(shipDate);
    return this;
  }

  /**
   * Get shipDate
   * @return shipDate
   */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("shipDate")
  public Optional<OffsetDateTime> getShipDate() {
    return shipDate;
  }

  public void setShipDate(Optional<OffsetDateTime> shipDate) {
    this.shipDate = shipDate;
  }

  public Order status(StatusEnum status) {
    this.status = Optional.ofNullable(status);
    return this;
  }

  /**
   * Order Status
   * @return status
   */
  
  @ApiModelProperty(value = "Order Status")
  @JsonProperty("status")
  public Optional<StatusEnum> getStatus() {
    return status;
  }

  public void setStatus(Optional<StatusEnum> status) {
    this.status = status;
  }

  public Order complete(Boolean complete) {
    this.complete = Optional.ofNullable(complete);
    return this;
  }

  /**
   * Get complete
   * @return complete
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("complete")
  public Optional<Boolean> getComplete() {
    return complete;
  }

  public void setComplete(Optional<Boolean> complete) {
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
  
  public static class Builder {

    private Order instance;

    public Builder() {
      this(new Order());
    }

    protected Builder(Order instance) {
      this.instance = instance;
    }

    protected Builder copyOf(Order value) { 
      this.instance.setId(value.id);
      this.instance.setPetId(value.petId);
      this.instance.setQuantity(value.quantity);
      this.instance.setShipDate(value.shipDate);
      this.instance.setStatus(value.status);
      this.instance.setComplete(value.complete);
      return this;
    }

    public Order.Builder id(Long id) {
      this.instance.id(id);
      return this;
    }
    
    public Order.Builder petId(Long petId) {
      this.instance.petId(petId);
      return this;
    }
    
    public Order.Builder quantity(Integer quantity) {
      this.instance.quantity(quantity);
      return this;
    }
    
    public Order.Builder shipDate(OffsetDateTime shipDate) {
      this.instance.shipDate(shipDate);
      return this;
    }
    
    public Order.Builder status(StatusEnum status) {
      this.instance.status(status);
      return this;
    }
    
    public Order.Builder complete(Boolean complete) {
      this.instance.complete(complete);
      return this;
    }
    
    /**
    * returns a built Order instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public Order build() {
      try {
        return this.instance;
      } finally {
        // ensure that this.instance is not reused
        this.instance = null;
      }
    }

    @Override
    public String toString() {
      return getClass() + "=(" + instance + ")";
    }
  }

  /**
  * Create a builder with no initialized field (except for the default values).
  */
  public static Order.Builder builder() {
    return new Order.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public Order.Builder toBuilder() {
    Order.Builder builder = new Order.Builder();
    return builder.copyOf(this);
  }

}

