package org.openapitools.model;

import groovy.transform.Canonical
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.ArrayList;
import java.util.Arrays;
import org.openapitools.model.Category;
import org.openapitools.model.Tag;

@Canonical
class Pet {
    
    Long id
    
    Category category
    
    String name
    
    List<String> photoUrls = new ArrayList<>()
    
    List<Tag> tags

    enum StatusEnum {
    
        AVAILABLE("available"),
        
        PENDING("pending"),
        
        SOLD("sold")
    
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

    /* pet status in the store */
    StatusEnum status
}
