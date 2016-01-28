package io.swagger.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.Date;

import javax.xml.bind.annotation.XmlRootElement;



@XmlRootElement(name="Order")
public class Order  {
  
  
  private Long id = null;
  
  private Long petId = null;
  
  private Integer quantity = null;
  
  private Date shipDate = null;
  
  public enum StatusEnum {
     placed,  approved,  delivered, 
  };
  private StatusEnum status = null;
  
  private Boolean complete = null;

  
  /**
   **/
  @JsonProperty("id")
  public Long getId() {
    return id;
  }
  public void setId(Long id) {
    this.id = id;
  }

  
  /**
   **/
  @JsonProperty("petId")
  public Long getPetId() {
    return petId;
  }
  public void setPetId(Long petId) {
    this.petId = petId;
  }

  
  /**
   **/
  @JsonProperty("quantity")
  public Integer getQuantity() {
    return quantity;
  }
  public void setQuantity(Integer quantity) {
    this.quantity = quantity;
  }

  
  /**
   **/
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
  @JsonProperty("status")
  public StatusEnum getStatus() {
    return status;
  }
  public void setStatus(StatusEnum status) {
    this.status = status;
  }

  
  /**
   **/
  @JsonProperty("complete")
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
