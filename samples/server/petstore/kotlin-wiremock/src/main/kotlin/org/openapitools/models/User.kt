@file:Suppress(
    "RemoveRedundantQualifierName",
    "unused",
)

package org.openapitools.models

import com.fasterxml.jackson.annotation.JsonProperty

data class User(
    @field:JsonProperty("id")
    val id: Long? = null,

    @field:JsonProperty("username")
    val username: String? = null,

    @field:JsonProperty("firstName")
    val firstName: String? = null,

    @field:JsonProperty("lastName")
    val lastName: String? = null,

    @field:JsonProperty("email")
    val email: String? = null,

    @field:JsonProperty("password")
    val password: String? = null,

    @field:JsonProperty("phone")
    val phone: String? = null,

    @field:JsonProperty("userStatus")
    val userStatus: Int? = null,

)
