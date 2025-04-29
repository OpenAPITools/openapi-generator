package org.openapitools.server.api.api

import jakarta.inject.Inject
import jakarta.inject.Singleton
import jakarta.validation.Valid
import jakarta.validation.constraints.DecimalMax
import jakarta.validation.constraints.DecimalMin
import jakarta.validation.constraints.Email
import jakarta.validation.constraints.Max
import jakarta.validation.constraints.Min
import jakarta.validation.constraints.NotNull
import jakarta.validation.constraints.Pattern
import jakarta.validation.constraints.Size
import misk.web.HttpCall
import misk.web.PathParam
import misk.web.QueryParam
import misk.web.RequestBody
import misk.web.RequestHeader
import org.openapitools.server.api.model.User

/**
 * @TODO("Fill out implementation")
 */
@Singleton
class UserApiImpl @Inject constructor(
): UserApi {

    override fun createUser(@Valid @RequestBody user: User) {
        TODO()
    }

    override fun createUsersWithArrayInput(@Valid @RequestBody user: kotlin.Array<User>) {
        TODO()
    }

    override fun createUsersWithListInput(@Valid @RequestBody user: kotlin.Array<User>) {
        TODO()
    }

    override fun deleteUser(@PathParam("username") username: kotlin.String) {
        TODO()
    }

    override fun getUserByName(@PathParam("username") username: kotlin.String): User {
        TODO()
    }

    override fun loginUser( @QueryParam(value = "username") username: kotlin.String,  @QueryParam(value = "password") password: kotlin.String): kotlin.String {
        TODO()
    }

    override fun logoutUser() {
        TODO()
    }

    override fun updateUser(@PathParam("username") username: kotlin.String, @Valid @RequestBody user: User) {
        TODO()
    }
}
