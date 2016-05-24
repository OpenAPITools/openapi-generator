package io.swagger.client.model;

import java.util.Date;

import io.swagger.annotations.*;
import com.google.gson.annotations.SerializedName;


@ApiModel(description = "")
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
  private Boolean complete = null;

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
    return (id == null ? order.id == null : id.equals(order.id)) &&
        (petId == null ? order.petId == null : petId.equals(order.petId)) &&
        (quantity == null ? order.quantity == null : quantity.equals(order.quantity)) &&
        (shipDate == null ? order.shipDate == null : shipDate.equals(order.shipDate)) &&
        (status == null ? order.status == null : status.equals(order.status)) &&
        (complete == null ? order.complete == null : complete.equals(order.complete));
  }

  @Override 
  public int hashCode() {
    int result = 17;
    result = 31 * result + (id == null ? 0: id.hashCode());
    result = 31 * result + (petId == null ? 0: petId.hashCode());
    result = 31 * result + (quantity == null ? 0: quantity.hashCode());
    result = 31 * result + (shipDate == null ? 0: shipDate.hashCode());
    result = 31 * result + (status == null ? 0: status.hashCode());
    result = 31 * result + (complete == null ? 0: complete.hashCode());
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
