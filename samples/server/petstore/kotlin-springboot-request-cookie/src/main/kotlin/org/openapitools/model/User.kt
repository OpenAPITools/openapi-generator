package org.openapitools.model

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
 * 
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

    @Schema(description = "")
    @param:JsonProperty("id")
    @get:JsonProperty("id") val id: kotlin.Long? = null,

    @Schema(description = "")
    @param:JsonProperty("username")
    @get:JsonProperty("username") val username: kotlin.String? = null,

    @Schema(description = "")
    @param:JsonProperty("firstName")
    @get:JsonProperty("firstName") val firstName: kotlin.String? = null,

    @Schema(description = "")
    @param:JsonProperty("lastName")
    @get:JsonProperty("lastName") val lastName: kotlin.String? = null,

    @Schema(description = "")
    @param:JsonProperty("email")
    @get:JsonProperty("email") val email: kotlin.String? = null,

    @Schema(description = "")
    @param:JsonProperty("password")
    @get:JsonProperty("password") val password: kotlin.String? = null,

    @Schema(description = "")
    @param:JsonProperty("phone")
    @get:JsonProperty("phone") val phone: kotlin.String? = null,

    @Schema(description = "User Status")
    @param:JsonProperty("userStatus")
    @get:JsonProperty("userStatus") val userStatus: kotlin.Int? = null
) {

}

