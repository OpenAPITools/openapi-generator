package org.openapitools.model

import java.util.Locale
import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import java.io.Serializable
import jakarta.validation.constraints.DecimalMax
import jakarta.validation.constraints.DecimalMin
import jakarta.validation.constraints.Email
import jakarta.validation.constraints.Max
import jakarta.validation.constraints.Min
import jakarta.validation.constraints.NotNull
import jakarta.validation.constraints.Pattern
import jakarta.validation.constraints.Size
import jakarta.validation.Valid

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

    @get:JsonProperty("id", required = false)
    val id: kotlin.Long? = null,

    @get:JsonProperty("username", required = false)
    val username: kotlin.String? = null,

    @get:JsonProperty("firstName", required = false)
    val firstName: kotlin.String? = null,

    @get:JsonProperty("lastName", required = false)
    val lastName: kotlin.String? = null,

    @get:JsonProperty("email", required = false)
    val email: kotlin.String? = null,

    @get:JsonProperty("password", required = false)
    val password: kotlin.String? = null,

    @get:JsonProperty("phone", required = false)
    val phone: kotlin.String? = null,

    @get:JsonProperty("userStatus", required = false)
    val userStatus: kotlin.Int? = null
) : Serializable {

    companion object {
        private const val serialVersionUID: kotlin.Long = 1
    }
}

