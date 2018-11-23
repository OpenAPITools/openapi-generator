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
        @JsonProperty("id") val id: Long? = null,

        @ApiModelProperty(value = "")
        @JsonProperty("username") val username: String? = null,

        @ApiModelProperty(value = "")
        @JsonProperty("firstName") val firstName: String? = null,

        @ApiModelProperty(value = "")
        @JsonProperty("lastName") val lastName: String? = null,

        @ApiModelProperty(value = "")
        @JsonProperty("email") val email: String? = null,

        @ApiModelProperty(value = "")
        @JsonProperty("password") val password: String? = null,

        @ApiModelProperty(value = "")
        @JsonProperty("phone") val phone: String? = null,

        @ApiModelProperty(value = "User Status")
        @JsonProperty("userStatus") val userStatus: Int? = null
) {

}

