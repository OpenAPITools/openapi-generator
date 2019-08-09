package org.openapitools.model;

import groovy.transform.Canonical
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

@Canonical
class User {
    
    Long id
    
    String username
    
    String firstName
    
    String lastName
    
    String email
    
    String password
    
    String phone
    /* User Status */
    Integer userStatus
}
