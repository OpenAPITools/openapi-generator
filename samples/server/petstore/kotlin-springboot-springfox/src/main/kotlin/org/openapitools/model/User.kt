package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import javax.validation.constraints.*
import javax.validation.Valid
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
data class User(

    @ApiModelProperty(example = "null", value = "")
    @field:JsonProperty("id") val id: kotlin.Long? = null,

    @ApiModelProperty(example = "null", value = "")
    @field:JsonProperty("username") val username: kotlin.String? = null,

    @ApiModelProperty(example = "null", value = "")
    @field:JsonProperty("firstName") val firstName: kotlin.String? = null,

    @ApiModelProperty(example = "null", value = "")
    @field:JsonProperty("lastName") val lastName: kotlin.String? = null,

    @ApiModelProperty(example = "null", value = "")
    @field:JsonProperty("email") val email: kotlin.String? = null,

    @ApiModelProperty(example = "null", value = "")
    @field:JsonProperty("password") val password: kotlin.String? = null,

    @ApiModelProperty(example = "null", value = "")
    @field:JsonProperty("phone") val phone: kotlin.String? = null,

    @ApiModelProperty(example = "null", value = "User Status")
    @field:JsonProperty("userStatus") val userStatus: kotlin.Int? = null
) {

}

