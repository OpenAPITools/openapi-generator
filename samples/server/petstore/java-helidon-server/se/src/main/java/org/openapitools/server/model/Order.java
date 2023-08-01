package org.openapitools.server.model;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import java.time.OffsetDateTime;



public class Order   {

    private Long id;
    private Long petId;
    private Integer quantity;
    private OffsetDateTime shipDate;

    /**
    * Order Status
    */
    public enum StatusEnum {
        PLACED("placed"),
        APPROVED("approved"),
        DELIVERED("delivered");

        private String value;

        StatusEnum(String value) {
            this.value = value;
        }

        @JsonValue
        public String getValue() {
            return value;
        }

        @Override
        public String toString() {
            return String.valueOf(value);
        }


        @JsonCreator
        public static StatusEnum fromValue(String text) {
            for (StatusEnum b : StatusEnum.values()) {
                if (String.valueOf(b.value).equals(text)) {
                    return b;
                }
            }
            throw new IllegalArgumentException("Unexpected value '" + text + "'");
        }
    }


    private StatusEnum status;
    private Boolean complete = false;

    /**
     * Default constructor.
     */
    public Order() {
    // JSON-B / Jackson
    }

    /**
     * Create Order.
     *
     * @param id id
     * @param petId petId
     * @param quantity quantity
     * @param shipDate shipDate
     * @param status Order Status
     * @param complete complete
     */
    public Order(
        Long id, 
        Long petId, 
        Integer quantity, 
        OffsetDateTime shipDate, 
        StatusEnum status, 
        Boolean complete
    ) {
        this.id = id;
        this.petId = petId;
        this.quantity = quantity;
        this.shipDate = shipDate;
        this.status = status;
        this.complete = complete;
    }



    /**
     * Get id
     * @return id
     */
    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    /**
     * Get petId
     * @return petId
     */
    public Long getPetId() {
        return petId;
    }

    public void setPetId(Long petId) {
        this.petId = petId;
    }

    /**
     * Get quantity
     * @return quantity
     */
    public Integer getQuantity() {
        return quantity;
    }

    public void setQuantity(Integer quantity) {
        this.quantity = quantity;
    }

    /**
     * Get shipDate
     * @return shipDate
     */
    public OffsetDateTime getShipDate() {
        return shipDate;
    }

    public void setShipDate(OffsetDateTime shipDate) {
        this.shipDate = shipDate;
    }

    /**
     * Order Status
     * @return status
     */
    public StatusEnum getStatus() {
        return status;
    }

    public void setStatus(StatusEnum status) {
        this.status = status;
    }

    /**
     * Get complete
     * @return complete
     */
    public Boolean getComplete() {
        return complete;
    }

    public void setComplete(Boolean complete) {
        this.complete = complete;
    }

    /**
      * Create a string representation of this pojo.
    **/
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

