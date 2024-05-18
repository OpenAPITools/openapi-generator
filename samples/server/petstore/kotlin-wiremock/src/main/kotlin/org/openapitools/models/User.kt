@file:Suppress(
    "RemoveRedundantQualifierName",
    "unused",
)

package org.openapitools.models

import com.fasterxml.jackson.annotation.JsonProperty

data class User(
    @field:JsonProperty("id")
    val id: kotlin.Long? = null,

    @field:JsonProperty("username")
    val username: kotlin.String? = null,

    @field:JsonProperty("firstName")
    val firstName: kotlin.String? = null,

    @field:JsonProperty("lastName")
    val lastName: kotlin.String? = null,

    @field:JsonProperty("email")
    val email: kotlin.String? = null,

    @field:JsonProperty("password")
    val password: kotlin.String? = null,

    @field:JsonProperty("phone")
    val phone: kotlin.String? = null,

    @field:JsonProperty("userStatus")
    val userStatus: kotlin.Int? = null,

)
