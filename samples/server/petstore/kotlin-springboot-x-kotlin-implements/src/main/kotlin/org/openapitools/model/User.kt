package org.openapitools.model

import java.util.Locale
import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import java.io.Serializable
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
 * 
 * @param id 
 * @param username 
 * @param firstName 
 * @param lastName 
 * @param email 
 * @param password 
 * @param phone 
 * @param userStatus 
 */
data class User(

    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("id", required = false)
    val id: kotlin.Long? = null,

    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("username", required = false)
    val username: kotlin.String? = null,

    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("firstName", required = false)
    val firstName: kotlin.String? = null,

    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("lastName", required = false)
    val lastName: kotlin.String? = null,

    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("email", required = false)
    val email: kotlin.String? = null,

    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("password", required = false)
    val password: kotlin.String? = null,

    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("phone", required = false)
    val phone: kotlin.String? = null,

    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("userStatus", required = false)
    val userStatus: kotlin.Int? = null
) : Serializable {

    companion object {
        private const val serialVersionUID: kotlin.Long = 1
    }
}

