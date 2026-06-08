package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import com.fasterxml.jackson.annotation.JsonValue;
import java.time.OffsetDateTime;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.lang.Nullable;
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
@JsonPropertyOrder({
    Order.JSON_PROPERTY_ID,
    Order.JSON_PROPERTY_PET_ID,
    Order.JSON_PROPERTY_QUANTITY,
    Order.JSON_PROPERTY_SHIP_DATE,
    Order.JSON_PROPERTY_STATUS,
    Order.JSON_PROPERTY_COMPLETE
})
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.23.0-SNAPSHOT")
public class Order {

    public static final String JSON_PROPERTY_ID = "id";
  private @Nullable Long id;

    public static final String JSON_PROPERTY_PET_ID = "petId";
  private @Nullable Long petId;

    public static final String JSON_PROPERTY_QUANTITY = "quantity";
  private @Nullable Integer quantity;

    public static final String JSON_PROPERTY_SHIP_DATE = "shipDate";
  @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME)
  private @Nullable OffsetDateTime shipDate;

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

    public static final String JSON_PROPERTY_STATUS = "status";
  private @Nullable StatusEnum status;

    public static final String JSON_PROPERTY_COMPLETE = "complete";
  private Boolean complete = false;

  public Order id(@Nullable Long id) {
    this.id = id;
    return this;
  }


  public Order petId(@Nullable Long petId) {
    this.petId = petId;
    return this;
  }


  public Order quantity(@Nullable Integer quantity) {
    this.quantity = quantity;
    return this;
  }


  public Order shipDate(@Nullable OffsetDateTime shipDate) {
    this.shipDate = shipDate;
    return this;
  }


  public Order status(@Nullable StatusEnum status) {
    this.status = status;
    return this;
  }


  public Order complete(Boolean complete) {
    this.complete = complete;
    return this;
  }



}

