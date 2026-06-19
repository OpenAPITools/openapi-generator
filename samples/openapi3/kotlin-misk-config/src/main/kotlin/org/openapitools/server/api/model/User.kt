package org.openapitools.server.api.model

import com.squareup.moshi.JsonClass

@JsonClass(generateAdapter = true)
data class User(
    val id: kotlin.Long? = null,
    val username: kotlin.String? = null,
    val firstName: kotlin.String? = null,
    val lastName: kotlin.String? = null,
    val email: kotlin.String? = null,
    val password: kotlin.String? = null,
    val phone: kotlin.String? = null,
    /** User Status */
    val userStatus: kotlin.Int? = null
)
