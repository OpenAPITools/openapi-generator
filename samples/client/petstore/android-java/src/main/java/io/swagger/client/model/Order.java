package io.swagger.client.model;

import com.google.gson.annotations.SerializedName;

import java.util.Date;


@ApiModel(description = "")
public class Order {

    @SerializedName("id")
    private Long id = null;
    @SerializedName("petId")
    private Long petId = null;
    @SerializedName("quantity")
    private Integer quantity = null;
    @SerializedName("shipDate")
    private Date shipDate = null;
    @SerializedName("status")
    private StatusEnum status = null;

    ;
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
    public String toString() {
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


    public enum StatusEnum {
        placed, approved, delivered,
    }
}
