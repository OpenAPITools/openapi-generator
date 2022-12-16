package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import javax.validation.constraints.*
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

    @Schema(example = "null", description = "")
    @field:JsonProperty("id") val id: kotlin.Long? = null,

    @Schema(example = "null", description = "")
    @field:JsonProperty("username") val username: kotlin.String? = null,

    @Schema(example = "null", description = "")
    @field:JsonProperty("firstName") val firstName: kotlin.String? = null,

    @Schema(example = "null", description = "")
    @field:JsonProperty("lastName") val lastName: kotlin.String? = null,

    @Schema(example = "null", description = "")
    @field:JsonProperty("email") val email: kotlin.String? = null,

    @Schema(example = "null", description = "")
    @field:JsonProperty("password") val password: kotlin.String? = null,

    @Schema(example = "null", description = "")
    @field:JsonProperty("phone") val phone: kotlin.String? = null,

    @Schema(example = "null", description = "User Status")
    @field:JsonProperty("userStatus") val userStatus: kotlin.Int? = null
) {

}

