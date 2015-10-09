package io.swagger.client.model;

import io.swagger.client.StringUtil;
import java.util.Date;

import com.google.gson.annotations.SerializedName;



import io.swagger.annotations.*;



@ApiModel(description = "")
public class Order   {
  
  @SerializedName("id")
  private Long id = null;
  
  @SerializedName("petId")
  private Long petId = null;
  
  @SerializedName("quantity")
  private Integer quantity = null;
  
  @SerializedName("shipDate")
  private Date shipDate = null;
  

public enum StatusEnum {
  @SerializedName("placed")
  PLACED("placed"),

  @SerializedName("approved")
  APPROVED("approved"),

  @SerializedName("delivered")
  DELIVERED("delivered");

  private String value;

  StatusEnum(String value) {
    this.value = value;
  }

  @Override
  public String toString() {
    return value;
  }
}

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
