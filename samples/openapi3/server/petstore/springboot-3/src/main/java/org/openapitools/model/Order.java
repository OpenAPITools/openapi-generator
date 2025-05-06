package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import java.time.OffsetDateTime;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlRootElement;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlElementWrapper;
import io.swagger.v3.oas.annotations.media.Schema;

import jakarta.xml.bind.annotation.*;

import java.util.*;
import jakarta.annotation.Generated;

/**
 * An order for a pets from the pet store
 */

@Schema(name = "Order", description = "An order for a pets from the pet store")
@JacksonXmlRootElement(localName = "Order")
@XmlRootElement(name = "Order")
@XmlAccessorType(XmlAccessType.FIELD)
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.14.0-SNAPSHOT")
public class Order {

  private @Nullable Long id;

  private @Nullable Long petId;

  private @Nullable Integer quantity;

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

  private @Nullable StatusEnum status;

  private Boolean complete = false;

  public Order() {
    super();
  }

  /**
   * Constructor with all args parameters
   */
  public Order(@Nullable Long id, @Nullable Long petId, @Nullable Integer quantity, @Nullable OffsetDateTime shipDate, @Nullable StatusEnum status, Boolean complete) {
      this.id = id;
      this.petId = petId;
      this.quantity = quantity;
      this.shipDate = shipDate;
      this.status = status;
      this.complete = complete;
  }

  public Order id(Long id) {
    this.id = id;
    return this;
  }

  /**
   * Get id
   * @return id
   */
  
  @Schema(name = "id", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("id")
  @JacksonXmlProperty(localName = "id")
  @XmlElement(name = "id")
  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }

  public Order petId(Long petId) {
    this.petId = petId;
    return this;
  }

  /**
   * Get petId
   * @return petId
   */
  
  @Schema(name = "petId", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("petId")
  @JacksonXmlProperty(localName = "petId")
  @XmlElement(name = "petId")
  public Long getPetId() {
    return petId;
  }

  public void setPetId(Long petId) {
    this.petId = petId;
  }

  public Order quantity(Integer quantity) {
    this.quantity = quantity;
    return this;
  }

  /**
   * Get quantity
   * @return quantity
   */
  
  @Schema(name = "quantity", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("quantity")
  @JacksonXmlProperty(localName = "quantity")
  @XmlElement(name = "quantity")
  public Integer getQuantity() {
    return quantity;
  }

  public void setQuantity(Integer quantity) {
    this.quantity = quantity;
  }

  public Order shipDate(OffsetDateTime shipDate) {
    this.shipDate = shipDate;
    return this;
  }

  /**
   * Get shipDate
   * @return shipDate
   */
  @Valid 
  @Schema(name = "shipDate", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("shipDate")
  @JacksonXmlProperty(localName = "shipDate")
  @XmlElement(name = "shipDate")
  public OffsetDateTime getShipDate() {
    return shipDate;
  }

  public void setShipDate(OffsetDateTime shipDate) {
    this.shipDate = shipDate;
  }

  public Order status(StatusEnum status) {
    this.status = status;
    return this;
  }

  /**
   * Order Status
   * @return status
   */
  
  @Schema(name = "status", description = "Order Status", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("status")
  @JacksonXmlProperty(localName = "status")
  @XmlElement(name = "status")
  public StatusEnum getStatus() {
    return status;
  }

  public void setStatus(StatusEnum status) {
    this.status = status;
  }

  public Order complete(Boolean complete) {
    this.complete = complete;
    return this;
  }

  /**
   * Get complete
   * @return complete
   */
  
  @Schema(name = "complete", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("complete")
  @JacksonXmlProperty(localName = "complete")
  @XmlElement(name = "complete")
  public Boolean getComplete() {
    return complete;
  }

  public void setComplete(Boolean complete) {
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

