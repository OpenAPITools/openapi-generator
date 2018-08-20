package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import javax.validation.Valid
import javax.validation.constraints.*

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
data class User (
    @JsonProperty("id") val id: kotlin.Long? = null,
    @JsonProperty("username") val username: kotlin.String? = null,
    @JsonProperty("firstName") val firstName: kotlin.String? = null,
    @JsonProperty("lastName") val lastName: kotlin.String? = null,
    @JsonProperty("email") val email: kotlin.String? = null,
    @JsonProperty("password") val password: kotlin.String? = null,
    @JsonProperty("phone") val phone: kotlin.String? = null,
    /* User Status */
    @JsonProperty("userStatus") val userStatus: kotlin.Int? = null
) {

}

