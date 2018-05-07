package org.openapitools.client.model;

import java.util.Date;

import io.swagger.annotations.*;
import com.google.gson.annotations.SerializedName;


/**
 * An order for a pets from the pet store
 **/
@ApiModel(description = "An order for a pets from the pet store")
public class Order  {
  
  @SerializedName("id")
  private Long id = null;
  @SerializedName("petId")
  private Long petId = null;
  @SerializedName("quantity")
  private Integer quantity = null;
  @SerializedName("shipDate")
  private Date shipDate = null;
  public enum StatusEnum {
     placed,  approved,  delivered, 
  };
  @SerializedName("status")
  private StatusEnum status = null;
  @SerializedName("complete")
  private Boolean complete = false;

  /**
   **/
  @ApiModelProperty(value = "")
  public Long getId() {
    return id;
  }
  public void setId(Long id) {
    this.id = id;
  }

  /**
   **/
  @ApiModelProperty(value = "")
  public Long getPetId() {
    return petId;
  }
  public void setPetId(Long petId) {
    this.petId = petId;
  }

  /**
   **/
  @ApiModelProperty(value = "")
  public Integer getQuantity() {
    return quantity;
  }
  public void setQuantity(Integer quantity) {
    this.quantity = quantity;
  }

  /**
   **/
  @ApiModelProperty(value = "")
  public Date getShipDate() {
    return shipDate;
  }
  public void setShipDate(Date shipDate) {
    this.shipDate = shipDate;
  }

  /**
   * Order Status
   **/
  @ApiModelProperty(value = "Order Status")
  public StatusEnum getStatus() {
    return status;
  }
  public void setStatus(StatusEnum status) {
    this.status = status;
  }

  /**
   **/
  @ApiModelProperty(value = "")
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
    return (this.id == null ? order.id == null : this.id.equals(order.id)) &&
        (this.petId == null ? order.petId == null : this.petId.equals(order.petId)) &&
        (this.quantity == null ? order.quantity == null : this.quantity.equals(order.quantity)) &&
        (this.shipDate == null ? order.shipDate == null : this.shipDate.equals(order.shipDate)) &&
        (this.status == null ? order.status == null : this.status.equals(order.status)) &&
        (this.complete == null ? order.complete == null : this.complete.equals(order.complete));
  }

  @Override
  public int hashCode() {
    int result = 17;
    result = 31 * result + (this.id == null ? 0: this.id.hashCode());
    result = 31 * result + (this.petId == null ? 0: this.petId.hashCode());
    result = 31 * result + (this.quantity == null ? 0: this.quantity.hashCode());
    result = 31 * result + (this.shipDate == null ? 0: this.shipDate.hashCode());
    result = 31 * result + (this.status == null ? 0: this.status.hashCode());
    result = 31 * result + (this.complete == null ? 0: this.complete.hashCode());
    return result;
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class Order {\n");
    
    sb.append("  id: ").append(id).append("\n");
    sb.append("  petId: ").append(petId).append("\n");
    sb.append("  quantity: ").append(quantity).append("\n");
    sb.append("  shipDate: ").append(shipDate).append("\n");
    sb.append("  status: ").append(status).append("\n");
    sb.append("  complete: ").append(complete).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
