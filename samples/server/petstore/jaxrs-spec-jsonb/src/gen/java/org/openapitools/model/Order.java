package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.time.OffsetDateTime;
import java.io.Serializable;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import jakarta.json.bind.annotation.JsonbCreator;
import jakarta.json.bind.annotation.JsonbProperty;
import jakarta.json.bind.annotation.JsonbTypeDeserializer;
import jakarta.json.bind.annotation.JsonbTypeSerializer;
import jakarta.json.bind.serializer.DeserializationContext;
import jakarta.json.bind.serializer.JsonbDeserializer;
import jakarta.json.bind.serializer.JsonbSerializer;
import jakarta.json.bind.serializer.SerializationContext;
import jakarta.json.stream.JsonGenerator;
import jakarta.json.stream.JsonParser;



@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public class Order  implements Serializable {
  private Long id;
  private Long petId;
  private Integer quantity;
  private OffsetDateTime shipDate;
  @JsonbTypeSerializer(StatusEnum.Serializer.class)
  @JsonbTypeDeserializer(StatusEnum.Deserializer.class)
  public enum StatusEnum {

    PLACED(String.valueOf("placed")), APPROVED(String.valueOf("approved")), DELIVERED(String.valueOf("delivered"));


    private String value;

    StatusEnum (String v) {
        value = v;
    }

    public String value() {
        return value;
    }

    @Override
    public String toString() {
        return String.valueOf(value);
    }

    /**
     * Convert a String into String, as specified in the
     * <a href="https://download.oracle.com/otndocs/jcp/jaxrs-2_0-fr-eval-spec/index.html">See JAX RS 2.0 Specification, section 3.2, p. 12</a>
     */
    public static StatusEnum fromString(String s) {
        for (StatusEnum b : StatusEnum.values()) {
            // using Objects.toString() to be safe if value type non-object type
            // because types like 'int' etc. will be auto-boxed
            if (java.util.Objects.toString(b.value).equals(s)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected string value '" + s + "'");
    }

    public static StatusEnum fromValue(String value) {
        for (StatusEnum b : StatusEnum.values()) {
            if (b.value.equals(value)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected value '" + value + "'");
    }

    public static final class Serializer implements JsonbSerializer<StatusEnum> {
        @Override
        public void serialize(StatusEnum obj, JsonGenerator generator, SerializationContext ctx) {
            ctx.serialize(obj.value, generator);
        }
    }

    public static final class Deserializer implements JsonbDeserializer<StatusEnum> {
        @Override
        public StatusEnum deserialize(JsonParser parser, DeserializationContext ctx, java.lang.reflect.Type rtType) {
            if (parser.getValue().getValueType() == jakarta.json.JsonValue.ValueType.NULL) {
                return null;
            }
            return fromValue(ctx.deserialize(String.class, parser));
        }
    }
}

  private StatusEnum status;
  private Boolean complete = false;

  protected Order(OrderBuilder<?, ?> b) {
    this.id = b.id;
    this.petId = b.petId;
    this.quantity = b.quantity;
    this.shipDate = b.shipDate;
    this.status = b.status;
    this.complete = b.complete;
  }

  public Order() {
  }

  /**
   **/
  public Order id(Long id) {
    this.id = id;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonbProperty("id")
  public Long getId() {
    return id;
  }

  @JsonbProperty("id")
  public void setId(Long id) {
    this.id = id;
  }

  /**
   **/
  public Order petId(Long petId) {
    this.petId = petId;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonbProperty("petId")
  public Long getPetId() {
    return petId;
  }

  @JsonbProperty("petId")
  public void setPetId(Long petId) {
    this.petId = petId;
  }

  /**
   **/
  public Order quantity(Integer quantity) {
    this.quantity = quantity;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonbProperty("quantity")
  public Integer getQuantity() {
    return quantity;
  }

  @JsonbProperty("quantity")
  public void setQuantity(Integer quantity) {
    this.quantity = quantity;
  }

  /**
   **/
  public Order shipDate(OffsetDateTime shipDate) {
    this.shipDate = shipDate;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonbProperty("shipDate")
  public OffsetDateTime getShipDate() {
    return shipDate;
  }

  @JsonbProperty("shipDate")
  public void setShipDate(OffsetDateTime shipDate) {
    this.shipDate = shipDate;
  }

  /**
   * Order Status
   **/
  public Order status(StatusEnum status) {
    this.status = status;
    return this;
  }

  
  @ApiModelProperty(value = "Order Status")
  @JsonbProperty("status")
  public StatusEnum getStatus() {
    return status;
  }

  @JsonbProperty("status")
  public void setStatus(StatusEnum status) {
    this.status = status;
  }

  /**
   **/
  public Order complete(Boolean complete) {
    this.complete = complete;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonbProperty("complete")
  public Boolean getComplete() {
    return complete;
  }

  @JsonbProperty("complete")
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
    return o == null ? "null" : o.toString().replace("\n", "\n    ");
  }


  public static OrderBuilder<?, ?> builder() {
    return new OrderBuilderImpl();
  }

  private static final class OrderBuilderImpl extends OrderBuilder<Order, OrderBuilderImpl> {

    @Override
    protected OrderBuilderImpl self() {
      return this;
    }

    @Override
    public Order build() {
      return new Order(this);
    }
  }

  public static abstract class OrderBuilder<C extends Order, B extends OrderBuilder<C, B>>  {
    private Long id;
    private Long petId;
    private Integer quantity;
    private OffsetDateTime shipDate;
    private StatusEnum status;
    private Boolean complete = false;
    protected abstract B self();

    public abstract C build();

    public B id(Long id) {
      this.id = id;
      return self();
    }
    public B petId(Long petId) {
      this.petId = petId;
      return self();
    }
    public B quantity(Integer quantity) {
      this.quantity = quantity;
      return self();
    }
    public B shipDate(OffsetDateTime shipDate) {
      this.shipDate = shipDate;
      return self();
    }
    public B status(StatusEnum status) {
      this.status = status;
      return self();
    }
    public B complete(Boolean complete) {
      this.complete = complete;
      return self();
    }
  }
}
