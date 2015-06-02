package io.swagger.client.model;

import java.util.Date;

import com.wordnik.swagger.annotations.*;
import com.google.gson.annotations.SerializedName;


@ApiModel(description = "")
public class Order  {
    

    /**
     **/
    @ApiModelProperty(value = "")  
    @SerializedName("id")
    private Long id = null;

    /**
     **/
    @ApiModelProperty(value = "")  
    @SerializedName("petId")
    private Long petId = null;

    /**
     **/
    @ApiModelProperty(value = "")  
    @SerializedName("quantity")
    private Integer quantity = null;

    /**
     **/
    @ApiModelProperty(value = "")  
    @SerializedName("shipDate")
    private Date shipDate = null;
    public enum StatusEnum {
         placed,  approved,  delivered, 
    };

    /**
     * Order Status
     **/
    @ApiModelProperty(value = "Order Status")  
    @SerializedName("status")    
    private StatusEnum status = null;

    /**
     **/
    @ApiModelProperty(value = "")  
    @SerializedName("complete")
    private Boolean complete = null;

        
    public Long getId() {
        return id;
    }
    public void setId(Long id) {
        this.id = id;
    }
        
    public Long getPetId() {
        return petId;
    }
    public void setPetId(Long petId) {
        this.petId = petId;
    }
        
    public Integer getQuantity() {
        return quantity;
    }
    public void setQuantity(Integer quantity) {
        this.quantity = quantity;
    }
        
    public Date getShipDate() {
        return shipDate;
    }
    public void setShipDate(Date shipDate) {
        this.shipDate = shipDate;
    }
        
    public StatusEnum getStatus() {
        return status;
    }
    public void setStatus(StatusEnum status) {
        this.status = status;
    }
        
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
