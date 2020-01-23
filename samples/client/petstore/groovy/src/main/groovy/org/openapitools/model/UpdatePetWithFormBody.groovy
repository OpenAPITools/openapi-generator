package org.openapitools.model;

import groovy.transform.Canonical
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

@Canonical
class UpdatePetWithFormBody {
    /* Updated name of the pet */
    String name
    /* Updated status of the pet */
    String status
}
