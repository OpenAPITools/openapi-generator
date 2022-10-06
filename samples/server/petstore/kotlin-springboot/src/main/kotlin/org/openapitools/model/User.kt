package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import javax.validation.constraints.*
import javax.validation.Valid

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

    @field:JsonProperty("id") val id: kotlin.Long? = null,

    @field:JsonProperty("username") val username: kotlin.String? = null,

    @field:JsonProperty("firstName") val firstName: kotlin.String? = null,

    @field:JsonProperty("lastName") val lastName: kotlin.String? = null,

    @field:JsonProperty("email") val email: kotlin.String? = null,

    @field:JsonProperty("password") val password: kotlin.String? = null,

    @field:JsonProperty("phone") val phone: kotlin.String? = null,

    @field:JsonProperty("userStatus") val userStatus: kotlin.Int? = null
) {

}

