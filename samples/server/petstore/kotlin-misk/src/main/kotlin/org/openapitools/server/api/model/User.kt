package org.openapitools.server.api.model


data class User(
    val id: kotlin.Long?,
    val username: kotlin.String?,
    val firstName: kotlin.String?,
    val lastName: kotlin.String?,
    val email: kotlin.String?,
    val password: kotlin.String?,
    val phone: kotlin.String?,
    /** User Status */
    val userStatus: kotlin.Int?
)
