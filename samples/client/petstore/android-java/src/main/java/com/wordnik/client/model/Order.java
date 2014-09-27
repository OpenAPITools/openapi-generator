package com.wordnik.client.model;

import java.util.Date;
import java.util.Map;
import java.util.*;

import com.wordnik.swagger.annotations.*;


@ApiModel(description = "")
public class Order  { 
  private Long id = null;
  
  //public enum idEnum {  }; 
  
  private Long petId = null;
  
  //public enum petIdEnum {  }; 
  
  private Integer quantity = null;
  
  //public enum quantityEnum {  }; 
  
  private Date shipDate = null;
  private String status = null;
  private Boolean complete = null;
  private Map<String, String> keyValuePairs = new HashMap<String, String>() ;
  
  
  /**
   **/
  @ApiModelProperty(required = true, value = "")
  public Long getId() {
    return id;
  }
  public void setId(Long id) {
    this.id = id;
  }

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  public Long getPetId() {
    return petId;
  }
  public void setPetId(Long petId) {
    this.petId = petId;
  }

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  public Integer getQuantity() {
    return quantity;
  }
  public void setQuantity(Integer quantity) {
    this.quantity = quantity;
  }

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  public Date getShipDate() {
    return shipDate;
  }
  public void setShipDate(Date shipDate) {
    this.shipDate = shipDate;
  }

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  public String getStatus() {
    return status;
  }
  public void setStatus(String status) {
    this.status = status;
  }

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  public Boolean getComplete() {
    return complete;
  }
  public void setComplete(Boolean complete) {
    this.complete = complete;
  }

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  public Map<String, String> getKeyValuePairs() {
    return keyValuePairs;
  }
  public void setKeyValuePairs(Map<String, String> keyValuePairs) {
    this.keyValuePairs = keyValuePairs;
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
    sb.append("  keyValuePairs: ").append(keyValuePairs).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
