package org.openapitools.model;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import java.util.Date;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.ApiModelProperty;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;


public class Order  {
  
  @ApiModelProperty(value = "")
  private Long id;

  @ApiModelProperty(value = "")
  private Long petId;

  @ApiModelProperty(value = "")
  private Integer quantity;

  @ApiModelProperty(value = "")
  @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'hh:mm:ss.SSSX")
  private Date shipDate;

@XmlType(name="StatusEnum")
@XmlEnum(String.class)
public enum StatusEnum {

    @XmlEnumValue("placed") @JsonProperty("placed") PLACED(String.valueOf("placed")), 
    @XmlEnumValue("approved") @JsonProperty("approved") APPROVED(String.valueOf("approved")), 
    @XmlEnumValue("delivered") @JsonProperty("delivered") DELIVERED(String.valueOf("delivered"));

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

    public static StatusEnum fromValue(String value) {
        for (StatusEnum b : StatusEnum.values()) {
            if (b.value.equals(value)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected value '" + value + "'");
    }
}

 /**
  * Order Status
  */
  @ApiModelProperty(value = "Order Status")
  private StatusEnum status;

  @ApiModelProperty(value = "")
  private Boolean complete = false;
 /**
  * Get id
  * @return id
  */
  @JsonProperty("id")
  public Long getId() {
    return id;
  }

  /**
   * Sets the <code>id</code> property.
   */
  public void setId(Long id) {
    this.id = id;
  }

  /**
   * Sets the <code>id</code> property.
   */
  public Order id(Long id) {
    this.id = id;
    return this;
  }

 /**
  * Get petId
  * @return petId
  */
  @JsonProperty("petId")
  public Long getPetId() {
    return petId;
  }

  /**
   * Sets the <code>petId</code> property.
   */
  public void setPetId(Long petId) {
    this.petId = petId;
  }

  /**
   * Sets the <code>petId</code> property.
   */
  public Order petId(Long petId) {
    this.petId = petId;
    return this;
  }

 /**
  * Get quantity
  * @return quantity
  */
  @JsonProperty("quantity")
  public Integer getQuantity() {
    return quantity;
  }

  /**
   * Sets the <code>quantity</code> property.
   */
  public void setQuantity(Integer quantity) {
    this.quantity = quantity;
  }

  /**
   * Sets the <code>quantity</code> property.
   */
  public Order quantity(Integer quantity) {
    this.quantity = quantity;
    return this;
  }

 /**
  * Get shipDate
  * @return shipDate
  */
  @JsonProperty("shipDate")
  public Date getShipDate() {
    return shipDate;
  }

  /**
   * Sets the <code>shipDate</code> property.
   */
  public void setShipDate(Date shipDate) {
    this.shipDate = shipDate;
  }

  /**
   * Sets the <code>shipDate</code> property.
   */
  public Order shipDate(Date shipDate) {
    this.shipDate = shipDate;
    return this;
  }

 /**
  * Order Status
  * @return status
  */
  @JsonProperty("status")
  public String getStatus() {
    return status == null ? null : status.value();
  }

  /**
   * Sets the <code>status</code> property.
   */
  public void setStatus(StatusEnum status) {
    this.status = status;
  }

  /**
   * Sets the <code>status</code> property.
   */
  public Order status(StatusEnum status) {
    this.status = status;
    return this;
  }

 /**
  * Get complete
  * @return complete
  */
  @JsonProperty("complete")
  public Boolean getComplete() {
    return complete;
  }

  /**
   * Sets the <code>complete</code> property.
   */
  public void setComplete(Boolean complete) {
    this.complete = complete;
  }

  /**
   * Sets the <code>complete</code> property.
   */
  public Order complete(Boolean complete) {
    this.complete = complete;
    return this;
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
  private static String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

