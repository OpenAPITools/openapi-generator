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

/**
 * WireMock stub request builder.
 */
open class UserApiStubs(private val objectMapper: ObjectMapper) {

    /**
     * Construct a stub for the operation createUser.
     *
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [CreateUserStubBuilder] to configure the response, and the final [MappingBuilder].
     */
    fun createUser(configurer: MappingBuilder.() -> MappingBuilder = { this }): CreateUserStubBuilder =
        CreateUserStubBuilder(objectMapper, post(urlPathTemplate("/user"))
            .configurer()
        )

    /**
     * Construct a stub for the operation createUsersWithArrayInput.
     *
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [CreateUsersWithArrayInputStubBuilder] to configure the response, and the final [MappingBuilder].
     */
    fun createUsersWithArrayInput(configurer: MappingBuilder.() -> MappingBuilder = { this }): CreateUsersWithArrayInputStubBuilder =
        CreateUsersWithArrayInputStubBuilder(objectMapper, post(urlPathTemplate("/user/createWithArray"))
            .configurer()
        )

    /**
     * Construct a stub for the operation createUsersWithListInput.
     *
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [CreateUsersWithListInputStubBuilder] to configure the response, and the final [MappingBuilder].
     */
    fun createUsersWithListInput(configurer: MappingBuilder.() -> MappingBuilder = { this }): CreateUsersWithListInputStubBuilder =
        CreateUsersWithListInputStubBuilder(objectMapper, post(urlPathTemplate("/user/createWithList"))
            .configurer()
        )

    /**
     * Construct a stub for the operation deleteUser.
     *
     * @param username path parameter username pattern.
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [DeleteUserStubBuilder] to configure the response, and the final [MappingBuilder].
     */
    fun deleteUser(username: StringValuePattern, configurer: MappingBuilder.() -> MappingBuilder = { this }): DeleteUserStubBuilder =
        DeleteUserStubBuilder(objectMapper, delete(urlPathTemplate("/user/{username}"))
            .withPathParam("username", username)
            .configurer()
        )

    /**
     * Construct a stub for the operation getUserByName.
     *
     * @param username path parameter username pattern.
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [GetUserByNameStubBuilder] to configure the response, and the final [MappingBuilder].
     */
    fun getUserByName(username: StringValuePattern, configurer: MappingBuilder.() -> MappingBuilder = { this }): GetUserByNameStubBuilder =
        GetUserByNameStubBuilder(objectMapper, get(urlPathTemplate("/user/{username}"))
            .withPathParam("username", username)
            .configurer()
        )

    /**
     * Construct a stub for the operation loginUser.
     *
     * @param username query parameter username pattern.
     * @param password query parameter password pattern.
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [LoginUserStubBuilder] to configure the response, and the final [MappingBuilder].
     */
    fun loginUser(username: StringValuePattern? = null, password: StringValuePattern? = null, configurer: MappingBuilder.() -> MappingBuilder = { this }): LoginUserStubBuilder =
        LoginUserStubBuilder(objectMapper, get(urlPathTemplate("/user/login"))
            .apply { username?.let { withQueryParam("username", it) } }
            .apply { password?.let { withQueryParam("password", it) } }
            .configurer()
        )

    /**
     * Construct a stub for the operation logoutUser.
     *
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [LogoutUserStubBuilder] to configure the response, and the final [MappingBuilder].
     */
    fun logoutUser(configurer: MappingBuilder.() -> MappingBuilder = { this }): LogoutUserStubBuilder =
        LogoutUserStubBuilder(objectMapper, get(urlPathTemplate("/user/logout"))
            .configurer()
        )

    /**
     * Construct a stub for the operation updateUser.
     *
     * @param username path parameter username pattern.
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [UpdateUserStubBuilder] to configure the response, and the final [MappingBuilder].
     */
    fun updateUser(username: StringValuePattern, configurer: MappingBuilder.() -> MappingBuilder = { this }): UpdateUserStubBuilder =
        UpdateUserStubBuilder(objectMapper, put(urlPathTemplate("/user/{username}"))
            .withPathParam("username", username)
            .configurer()
        )
}
