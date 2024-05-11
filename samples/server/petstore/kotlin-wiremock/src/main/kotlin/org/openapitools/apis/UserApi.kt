package org.openapitools.apis

import com.fasterxml.jackson.databind.ObjectMapper
import com.github.tomakehurst.wiremock.client.MappingBuilder
import com.github.tomakehurst.wiremock.client.ResponseDefinitionBuilder
import com.github.tomakehurst.wiremock.client.WireMock.*
import com.github.tomakehurst.wiremock.matching.StringValuePattern
import org.openapitools.models.*

open class UserApiStubs(protected val objectMapper: ObjectMapper) {

    fun createUser(configurer: MappingBuilder.() -> MappingBuilder = { this }): CreateUserStubBuilder =
        CreateUserStubBuilder(post("/user")
            .configurer()
        )

    inner class CreateUserStubBuilder(private val stub: MappingBuilder) {

        fun respondWith(
            code: Int,
            body: Any? = null,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this }
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .configurer()
                .withStatus(code)
                .apply {
                    body?.let { withBody(objectMapper.writeValueAsString(it)) }
                }
        )
    }

    fun createUsersWithArrayInput(configurer: MappingBuilder.() -> MappingBuilder = { this }): CreateUsersWithArrayInputStubBuilder =
        CreateUsersWithArrayInputStubBuilder(post("/user/createWithArray")
            .configurer()
        )

    inner class CreateUsersWithArrayInputStubBuilder(private val stub: MappingBuilder) {

        fun respondWith(
            code: Int,
            body: Any? = null,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this }
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .configurer()
                .withStatus(code)
                .apply {
                    body?.let { withBody(objectMapper.writeValueAsString(it)) }
                }
        )
    }

    fun createUsersWithListInput(configurer: MappingBuilder.() -> MappingBuilder = { this }): CreateUsersWithListInputStubBuilder =
        CreateUsersWithListInputStubBuilder(post("/user/createWithList")
            .configurer()
        )

    inner class CreateUsersWithListInputStubBuilder(private val stub: MappingBuilder) {

        fun respondWith(
            code: Int,
            body: Any? = null,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this }
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .configurer()
                .withStatus(code)
                .apply {
                    body?.let { withBody(objectMapper.writeValueAsString(it)) }
                }
        )
    }

    fun deleteUser(username: StringValuePattern, configurer: MappingBuilder.() -> MappingBuilder = { this }): DeleteUserStubBuilder =
        DeleteUserStubBuilder(delete("/user/{username}")
            .configurer()
            .withPathParam("username", username)
        )

    inner class DeleteUserStubBuilder(private val stub: MappingBuilder) {

        fun respondWith400(
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .configurer()
                .withStatus(400)
            )

        fun respondWith404(
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .configurer()
                .withStatus(404)
            )

        fun respondWith(
            code: Int,
            body: Any? = null,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this }
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .configurer()
                .withStatus(code)
                .apply {
                    body?.let { withBody(objectMapper.writeValueAsString(it)) }
                }
        )
    }

    fun getUserByName(username: StringValuePattern, configurer: MappingBuilder.() -> MappingBuilder = { this }): GetUserByNameStubBuilder =
        GetUserByNameStubBuilder(get("/user/{username}")
            .configurer()
            .withPathParam("username", username)
        )

    inner class GetUserByNameStubBuilder(private val stub: MappingBuilder) {

        fun respondWith200(
            body: User,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .configurer()
                .withStatus(200)
                .withHeader("Content-Type", "application/json")
                .withBody(objectMapper.writeValueAsString(body))
            )

        fun respondWith400(
            body: User,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .configurer()
                .withStatus(400)
                .withHeader("Content-Type", "application/json")
                .withBody(objectMapper.writeValueAsString(body))
            )

        fun respondWith404(
            body: User,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .configurer()
                .withStatus(404)
                .withHeader("Content-Type", "application/json")
                .withBody(objectMapper.writeValueAsString(body))
            )

        fun respondWith(
            code: Int,
            body: Any? = null,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this }
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .configurer()
                .withStatus(code)
                .apply {
                    body?.let { withBody(objectMapper.writeValueAsString(it)) }
                }
        )
    }

    fun loginUser(username: StringValuePattern? = null, password: StringValuePattern? = null, configurer: MappingBuilder.() -> MappingBuilder = { this }): LoginUserStubBuilder =
        LoginUserStubBuilder(get("/user/login")
            .configurer()
            .apply {
                username?.let { withQueryParam("username", it) }
            }
            .apply {
                password?.let { withQueryParam("password", it) }
            }
        )

    inner class LoginUserStubBuilder(private val stub: MappingBuilder) {

        fun respondWith200(
            body: kotlin.String,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .configurer()
                .withStatus(200)
                .withHeader("Content-Type", "application/json")
                .withBody(objectMapper.writeValueAsString(body))
            )

        fun respondWith400(
            body: kotlin.String,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .configurer()
                .withStatus(400)
                .withHeader("Content-Type", "application/json")
                .withBody(objectMapper.writeValueAsString(body))
            )

        fun respondWith(
            code: Int,
            body: Any? = null,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this }
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .configurer()
                .withStatus(code)
                .apply {
                    body?.let { withBody(objectMapper.writeValueAsString(it)) }
                }
        )
    }

    fun logoutUser(configurer: MappingBuilder.() -> MappingBuilder = { this }): LogoutUserStubBuilder =
        LogoutUserStubBuilder(get("/user/logout")
            .configurer()
        )

    inner class LogoutUserStubBuilder(private val stub: MappingBuilder) {

        fun respondWith(
            code: Int,
            body: Any? = null,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this }
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .configurer()
                .withStatus(code)
                .apply {
                    body?.let { withBody(objectMapper.writeValueAsString(it)) }
                }
        )
    }

    fun updateUser(username: StringValuePattern, configurer: MappingBuilder.() -> MappingBuilder = { this }): UpdateUserStubBuilder =
        UpdateUserStubBuilder(put("/user/{username}")
            .configurer()
            .withPathParam("username", username)
        )

    inner class UpdateUserStubBuilder(private val stub: MappingBuilder) {

        fun respondWith400(
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .configurer()
                .withStatus(400)
            )

        fun respondWith404(
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .configurer()
                .withStatus(404)
            )

        fun respondWith(
            code: Int,
            body: Any? = null,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this }
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .configurer()
                .withStatus(code)
                .apply {
                    body?.let { withBody(objectMapper.writeValueAsString(it)) }
                }
        )
    }
}
