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
import io.swagger.v3.oas.annotations.media.Schema

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

    @Schema(example = "null", required = false, description = "")
    @get:JsonProperty("id", required = false)
    var id: kotlin.Long? = null,

    @Schema(example = "null", required = false, description = "")
    @get:JsonProperty("username", required = false)
    var username: kotlin.String? = null,

    @Schema(example = "null", required = false, description = "")
    @get:JsonProperty("firstName", required = false)
    var firstName: kotlin.String? = null,

    @Schema(example = "null", required = false, description = "")
    @get:JsonProperty("lastName", required = false)
    var lastName: kotlin.String? = null,

    @Schema(example = "null", required = false, description = "")
    @get:JsonProperty("email", required = false)
    var email: kotlin.String? = null,

    @Schema(example = "null", required = false, description = "")
    @get:JsonProperty("password", required = false)
    var password: kotlin.String? = null,

    @Schema(example = "null", required = false, description = "")
    @get:JsonProperty("phone", required = false)
    var phone: kotlin.String? = null,

    @Schema(example = "null", required = false, description = "User Status")
    @get:JsonProperty("userStatus", required = false)
    var userStatus: kotlin.Int? = null
) : Serializable {

    companion object {
        private const val serialVersionUID: kotlin.Long = 1
    }
}

