package io.swagger.client.model;

import io.swagger.client.StringUtil;
import java.util.Date;


import java.util.Objects;

import io.swagger.annotations.*;
import com.fasterxml.jackson.annotation.*;


@ApiModel(description = "")
@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaClientCodegen", date = "2015-12-10T16:26:30.730+08:00")
public class Order   {
  
  private Long id = null;
  private Long petId = null;
  private Integer quantity = null;
  private Date shipDate = null;


public enum StatusEnum {
  PLACED("placed"),
  APPROVED("approved"),
  DELIVERED("delivered");

  private String value;

  StatusEnum(String value) {
    this.value = value;
  }

  @Override
  @JsonValue
  public String toString() {
    return value;
  }
}

  private StatusEnum status = null;
  private Boolean complete = null;

  
  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("id")
  public Long getId() {
    return id;
  }
  public void setId(Long id) {
    this.id = id;
  }

  
  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("petId")
  public Long getPetId() {
    return petId;
  }
  public void setPetId(Long petId) {
    this.petId = petId;
  }

  
  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("quantity")
  public Integer getQuantity() {
    return quantity;
  }
  public void setQuantity(Integer quantity) {
    this.quantity = quantity;
  }

  
  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("shipDate")
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
  @JsonProperty("status")
  public StatusEnum getStatus() {
    return status;
  }
  public void setStatus(StatusEnum status) {
    this.status = status;
  }

  
  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("complete")
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
    return Objects.equals(id, order.id) &&
        Objects.equals(petId, order.petId) &&
        Objects.equals(quantity, order.quantity) &&
        Objects.equals(shipDate, order.shipDate) &&
        Objects.equals(status, order.status) &&
        Objects.equals(complete, order.complete);
  }

  @Override
  public int hashCode() {
    return Objects.hash(id, petId, quantity, shipDate, status, complete);
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class Order {\n");
    
    sb.append("    id: ").append(StringUtil.toIndentedString(id)).append("\n");
    sb.append("    petId: ").append(StringUtil.toIndentedString(petId)).append("\n");
    sb.append("    quantity: ").append(StringUtil.toIndentedString(quantity)).append("\n");
    sb.append("    shipDate: ").append(StringUtil.toIndentedString(shipDate)).append("\n");
    sb.append("    status: ").append(StringUtil.toIndentedString(status)).append("\n");
    sb.append("    complete: ").append(StringUtil.toIndentedString(complete)).append("\n");
    sb.append("}");
    return sb.toString();
  }
}
