package io.swagger.model;




import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;

@XmlAccessorType(XmlAccessType.FIELD)
 @XmlType(name = "", propOrder =
	{ "id", "petId", "quantity", "shipDate", "status", "complete"
})

@XmlRootElement(name="Order")
public class Order  {
  

  private Long id = null;

  private Long petId = null;

  private Integer quantity = null;

  private javax.xml.datatype.XMLGregorianCalendar shipDate = null;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlType;

@XmlType(name="Order")
@XmlEnum
public enum Order {
    {values=[placed, approved, delivered], enumVars=[{name=PLACED, value=placed}, {name=APPROVED, value=approved}, {name=DELIVERED, value=delivered}]}, 
    
    public String value() {
        return name();
    }

    public static Order fromValue(String v) {
        return valueOf(v);
    }
}

  private StatusEnum status = null;

  private Boolean complete = null;

  
  /**
   **/
  
  public Long getId() {
    return id;
  }
  public void setId(Long id) {
    this.id = id;
  }
  
  /**
   **/
  
  public Long getPetId() {
    return petId;
  }
  public void setPetId(Long petId) {
    this.petId = petId;
  }
  
  /**
   **/
  
  public Integer getQuantity() {
    return quantity;
  }
  public void setQuantity(Integer quantity) {
    this.quantity = quantity;
  }
  
  /**
   **/
  
  public javax.xml.datatype.XMLGregorianCalendar getShipDate() {
    return shipDate;
  }
  public void setShipDate(javax.xml.datatype.XMLGregorianCalendar shipDate) {
    this.shipDate = shipDate;
  }
  
  /**
   * Order Status
   **/
  
  public StatusEnum getStatus() {
    return status;
  }
  public void setStatus(StatusEnum status) {
    this.status = status;
  }
  
  /**
   **/
  
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

