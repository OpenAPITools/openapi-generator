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

    @ApiModelProperty(value = "")
    @param:JsonProperty("id")
    @get:JsonProperty("id") val id: kotlin.Long? = null,

    @ApiModelProperty(value = "")
    @param:JsonProperty("username")
    @get:JsonProperty("username") val username: kotlin.String? = null,

    @ApiModelProperty(value = "")
    @param:JsonProperty("firstName")
    @get:JsonProperty("firstName") val firstName: kotlin.String? = null,

    @ApiModelProperty(value = "")
    @param:JsonProperty("lastName")
    @get:JsonProperty("lastName") val lastName: kotlin.String? = null,

    @ApiModelProperty(value = "")
    @param:JsonProperty("email")
    @get:JsonProperty("email") val email: kotlin.String? = null,

    @ApiModelProperty(value = "")
    @param:JsonProperty("password")
    @get:JsonProperty("password") val password: kotlin.String? = null,

    @ApiModelProperty(value = "")
    @param:JsonProperty("phone")
    @get:JsonProperty("phone") val phone: kotlin.String? = null,

    @ApiModelProperty(value = "User Status")
    @param:JsonProperty("userStatus")
    @get:JsonProperty("userStatus") val userStatus: kotlin.Int? = null
) : java.io.Serializable {

    companion object {
        private const val serialVersionUID: kotlin.Long = 1
    }
}

