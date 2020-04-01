package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import javax.validation.constraints.DecimalMax
import javax.validation.constraints.DecimalMin
import javax.validation.constraints.Max
import javax.validation.constraints.Min
import javax.validation.constraints.NotNull
import javax.validation.constraints.Pattern
import javax.validation.constraints.Size
import io.swagger.annotations.ApiModelProperty

/**
 * A User who is purchasing from the pet store
 * @param id 
 * @param username 
 * @param firstName 
 * @param lastName 
 * @param email 
 * @param password 
 * @param phone 
 * @param userStatus User Status
 */
data class User (

        @ApiModelProperty(example = "null", value = "")
        @JsonProperty("id") var id: kotlin.Long? = null,

        @ApiModelProperty(example = "null", value = "")
        @JsonProperty("username") var username: kotlin.String? = null,

        @ApiModelProperty(example = "null", value = "")
        @JsonProperty("firstName") var firstName: kotlin.String? = null,

        @ApiModelProperty(example = "null", value = "")
        @JsonProperty("lastName") var lastName: kotlin.String? = null,

        @ApiModelProperty(example = "null", value = "")
        @JsonProperty("email") var email: kotlin.String? = null,

        @ApiModelProperty(example = "null", value = "")
        @JsonProperty("password") var password: kotlin.String? = null,

        @ApiModelProperty(example = "null", value = "")
        @JsonProperty("phone") var phone: kotlin.String? = null,

        @ApiModelProperty(example = "null", value = "User Status")
        @JsonProperty("userStatus") var userStatus: kotlin.Int? = null
) {

}

