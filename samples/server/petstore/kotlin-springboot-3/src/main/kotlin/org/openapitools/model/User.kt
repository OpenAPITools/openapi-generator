package org.openapitools.model

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

    @get:JsonProperty("id") val id: Long? = null,

    @get:JsonProperty("username") val username: String? = null,

    @get:JsonProperty("firstName") val firstName: String? = null,

    @get:JsonProperty("lastName") val lastName: String? = null,

    @get:JsonProperty("email") val email: String? = null,

    @get:JsonProperty("password") val password: String? = null,

    @get:JsonProperty("phone") val phone: String? = null,

    @get:JsonProperty("userStatus") val userStatus: Int? = null
    ) : Serializable{

    companion object {
        private const val serialVersionUID: kotlin.Long = 1
    }
}

