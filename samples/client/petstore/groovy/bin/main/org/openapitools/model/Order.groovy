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
    /* Order Status */
    String status
    
    Boolean complete = false
}
