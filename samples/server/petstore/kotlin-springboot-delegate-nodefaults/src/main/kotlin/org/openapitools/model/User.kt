package org.openapitools.model

import java.util.Locale
import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import jakarta.validation.constraints.DecimalMax
import jakarta.validation.constraints.DecimalMin
import jakarta.validation.constraints.Email
import jakarta.validation.constraints.Max
import jakarta.validation.constraints.Min
import jakarta.validation.constraints.NotNull
import jakarta.validation.constraints.Pattern
import jakarta.validation.constraints.Size
import jakarta.validation.Valid
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
    val id: kotlin.Long? = null,

    @Schema(example = "null", required = false, description = "")
    @get:JsonProperty("username", required = false)
    val username: kotlin.String? = null,

    @Schema(example = "null", required = false, description = "")
    @get:JsonProperty("firstName", required = false)
    val firstName: kotlin.String? = null,

    @Schema(example = "null", required = false, description = "")
    @get:JsonProperty("lastName", required = false)
    val lastName: kotlin.String? = null,

    @Schema(example = "null", required = false, description = "")
    @get:JsonProperty("email", required = false)
    val email: kotlin.String? = null,

    @Schema(example = "null", required = false, description = "")
    @get:JsonProperty("password", required = false)
    val password: kotlin.String? = null,

    @Schema(example = "null", required = false, description = "")
    @get:JsonProperty("phone", required = false)
    val phone: kotlin.String? = null,

    @Schema(example = "null", required = false, description = "User Status")
    @get:JsonProperty("userStatus", required = false)
    val userStatus: kotlin.Int? = null
) {

}

