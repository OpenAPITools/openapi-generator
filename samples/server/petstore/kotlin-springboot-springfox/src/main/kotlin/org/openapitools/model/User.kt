package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import javax.validation.constraints.DecimalMax
import javax.validation.constraints.DecimalMin
import javax.validation.constraints.Email
import javax.validation.constraints.Max
import javax.validation.constraints.Min
import javax.validation.constraints.NotNull
import javax.validation.constraints.Pattern
import javax.validation.constraints.Size
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
    @get:JsonProperty("id") val id: kotlin.Long? = null,

    @ApiModelProperty(example = "null", value = "")
    @get:JsonProperty("username") val username: kotlin.String? = null,

    @ApiModelProperty(example = "null", value = "")
    @get:JsonProperty("firstName") val firstName: kotlin.String? = null,

    @ApiModelProperty(example = "null", value = "")
    @get:JsonProperty("lastName") val lastName: kotlin.String? = null,

    @ApiModelProperty(example = "null", value = "")
    @get:JsonProperty("email") val email: kotlin.String? = null,

    @ApiModelProperty(example = "null", value = "")
    @get:JsonProperty("password") val password: kotlin.String? = null,

    @ApiModelProperty(example = "null", value = "")
    @get:JsonProperty("phone") val phone: kotlin.String? = null,

    @ApiModelProperty(example = "null", value = "User Status")
    @get:JsonProperty("userStatus") val userStatus: kotlin.Int? = null
) {

}

