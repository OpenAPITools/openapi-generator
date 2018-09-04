package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import javax.validation.Valid
import javax.validation.constraints.*
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

        @ApiModelProperty(value = "")
        @JsonProperty("id") val id: kotlin.Long? = null,

        @ApiModelProperty(value = "")
        @JsonProperty("username") val username: kotlin.String? = null,

        @ApiModelProperty(value = "")
        @JsonProperty("firstName") val firstName: kotlin.String? = null,

        @ApiModelProperty(value = "")
        @JsonProperty("lastName") val lastName: kotlin.String? = null,

        @ApiModelProperty(value = "")
        @JsonProperty("email") val email: kotlin.String? = null,

        @ApiModelProperty(value = "")
        @JsonProperty("password") val password: kotlin.String? = null,

        @ApiModelProperty(value = "")
        @JsonProperty("phone") val phone: kotlin.String? = null,

        @ApiModelProperty(value = "User Status")
        @JsonProperty("userStatus") val userStatus: kotlin.Int? = null
) {

}

