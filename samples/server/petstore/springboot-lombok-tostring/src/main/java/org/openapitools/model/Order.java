package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import java.time.OffsetDateTime;
import org.springframework.format.annotation.DateTimeFormat;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import org.hibernate.validator.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * An order for a pets from the pet store
 */
@lombok.Getter
@lombok.Setter
@lombok.ToString
@lombok.EqualsAndHashCode

@Schema(name = "Order", description = "An order for a pets from the pet store")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.6.0-SNAPSHOT")
public class Order {

  private Long id;

  private Long petId;

  private Integer quantity;

  @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME)
  private OffsetDateTime shipDate;

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

  private StatusEnum status;

  private Boolean complete = false;

  public Order() {
    super();
  }

  public Order id(Long id) {
    this.id = id;
    return this;
  }


  public Order petId(Long petId) {
    this.petId = petId;
    return this;
  }


  public Order quantity(Integer quantity) {
    this.quantity = quantity;
    return this;
  }


  public Order shipDate(OffsetDateTime shipDate) {
    this.shipDate = shipDate;
    return this;
  }


  public Order status(StatusEnum status) {
    this.status = status;
    return this;
  }


  public Order complete(Boolean complete) {
    this.complete = complete;
    return this;
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

