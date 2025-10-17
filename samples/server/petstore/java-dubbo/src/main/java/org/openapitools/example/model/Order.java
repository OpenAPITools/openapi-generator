package org.openapitools.example.model;

import java.time.OffsetDateTime;
import java.util.Objects;
import java.io.Serializable;
import com.fasterxml.jackson.annotation.JsonProperty;
import javax.annotation.Generated;
import java.time.*;
import java.math.*;
@Generated(value = "org.openapitools.codegen.languages.JavaDubboServerCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")

/**
 * An order for a pets from the pet store
 */
public class Order implements Serializable {
  private static final long serialVersionUID = 1L;

  @JsonProperty("id")
  private Long id;

  @JsonProperty("petId")
  private Long petId;

  @JsonProperty("quantity")
  private Integer quantity;

  @JsonProperty("shipDate")
  private OffsetDateTime shipDate;

  /**
   * Order Status
   */
  @JsonProperty("status")
  private String status;

  @JsonProperty("complete")
  private Boolean complete = false;

  /**
   * 
   * @return id
   */
  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }

  /**
   * 
   * @return petId
   */
  public Long getPetId() {
    return petId;
  }

  public void setPetId(Long petId) {
    this.petId = petId;
  }

  /**
   * 
   * @return quantity
   */
  public Integer getQuantity() {
    return quantity;
  }

  public void setQuantity(Integer quantity) {
    this.quantity = quantity;
  }

  /**
   * 
   * @return shipDate
   */
  public OffsetDateTime getShipDate() {
    return shipDate;
  }

  public void setShipDate(OffsetDateTime shipDate) {
    this.shipDate = shipDate;
  }

  /**
   * Order Status
   * @return status
   */
  public String getStatus() {
    return status;
  }

  public void setStatus(String status) {
    this.status = status;
  }

  /**
   * 
   * @return complete
   */
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
}
