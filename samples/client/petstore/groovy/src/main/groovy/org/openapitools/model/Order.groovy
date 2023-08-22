package org.openapitools.model;

import groovy.transform.Canonical
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

@Canonical
class Order {
    
    Long id
    
    Long petId
    
    Integer quantity
    
    Date shipDate

    enum StatusEnum {
    
        PLACED("placed"),
        
        APPROVED("approved"),
        
        DELIVERED("delivered")
    
        private final String value
    
        StatusEnum(String value) {
            this.value = value
        }
    
        String getValue() {
            value
        }
    
        @Override
        String toString() {
            String.valueOf(value)
        }
    }

    /* Order Status */
    StatusEnum status
    
    Boolean complete = false
}
