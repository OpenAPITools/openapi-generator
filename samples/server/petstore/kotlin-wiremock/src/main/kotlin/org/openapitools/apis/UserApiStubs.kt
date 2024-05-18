@file:Suppress(
    "RemoveRedundantQualifierName",
    "UnusedImport",
    "unused",
)

package org.openapitools.apis

import com.fasterxml.jackson.databind.ObjectMapper
import com.github.tomakehurst.wiremock.client.MappingBuilder
import com.github.tomakehurst.wiremock.client.ResponseDefinitionBuilder
import com.github.tomakehurst.wiremock.client.WireMock.*
import com.github.tomakehurst.wiremock.matching.StringValuePattern
import org.openapitools.models.*

open class UserApiStubs(private val objectMapper: ObjectMapper) {

    fun createUser(configurer: MappingBuilder.() -> MappingBuilder = { this }): CreateUserStubBuilder =
        CreateUserStubBuilder(objectMapper, post(urlPathTemplate("/user"))
            .configurer()
        )

    fun createUsersWithArrayInput(configurer: MappingBuilder.() -> MappingBuilder = { this }): CreateUsersWithArrayInputStubBuilder =
        CreateUsersWithArrayInputStubBuilder(objectMapper, post(urlPathTemplate("/user/createWithArray"))
            .configurer()
        )

    fun createUsersWithListInput(configurer: MappingBuilder.() -> MappingBuilder = { this }): CreateUsersWithListInputStubBuilder =
        CreateUsersWithListInputStubBuilder(objectMapper, post(urlPathTemplate("/user/createWithList"))
            .configurer()
        )

    fun deleteUser(username: StringValuePattern, configurer: MappingBuilder.() -> MappingBuilder = { this }): DeleteUserStubBuilder =
        DeleteUserStubBuilder(objectMapper, delete(urlPathTemplate("/user/{username}"))
            .withPathParam("username", username)
            .configurer()
        )

    fun getUserByName(username: StringValuePattern, configurer: MappingBuilder.() -> MappingBuilder = { this }): GetUserByNameStubBuilder =
        GetUserByNameStubBuilder(objectMapper, get(urlPathTemplate("/user/{username}"))
            .withPathParam("username", username)
            .configurer()
        )

    fun loginUser(username: StringValuePattern? = null, password: StringValuePattern? = null, configurer: MappingBuilder.() -> MappingBuilder = { this }): LoginUserStubBuilder =
        LoginUserStubBuilder(objectMapper, get(urlPathTemplate("/user/login"))
            .apply { username?.let { withQueryParam("username", it) } }
            .apply { password?.let { withQueryParam("password", it) } }
            .configurer()
        )

    fun logoutUser(configurer: MappingBuilder.() -> MappingBuilder = { this }): LogoutUserStubBuilder =
        LogoutUserStubBuilder(objectMapper, get(urlPathTemplate("/user/logout"))
            .configurer()
        )

    fun updateUser(username: StringValuePattern, configurer: MappingBuilder.() -> MappingBuilder = { this }): UpdateUserStubBuilder =
        UpdateUserStubBuilder(objectMapper, put(urlPathTemplate("/user/{username}"))
            .withPathParam("username", username)
            .configurer()
        )
}
