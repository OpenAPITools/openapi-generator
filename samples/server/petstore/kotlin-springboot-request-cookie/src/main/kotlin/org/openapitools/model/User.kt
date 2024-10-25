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

    @Schema(example = "null", description = "")
    @get:JsonProperty("id") val id: Long? = null,

    @Schema(example = "null", description = "")
    @get:JsonProperty("username") val username: String? = null,

    @Schema(example = "null", description = "")
    @get:JsonProperty("firstName") val firstName: String? = null,

    @Schema(example = "null", description = "")
    @get:JsonProperty("lastName") val lastName: String? = null,

    @Schema(example = "null", description = "")
    @get:JsonProperty("email") val email: String? = null,

    @Schema(example = "null", description = "")
    @get:JsonProperty("password") val password: String? = null,

    @Schema(example = "null", description = "")
    @get:JsonProperty("phone") val phone: String? = null,

    @Schema(example = "null", description = "User Status")
    @get:JsonProperty("userStatus") val userStatus: Int? = null
    ) {

}

