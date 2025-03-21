package org.openapitools.server.api.api

import org.openapitools.server.api.model.User

import jakarta.inject.Inject
import jakarta.inject.Singleton
import okhttp3.Headers

/**
 * @TODO("Fill out implementation")
 */
@Singleton
class UserApiImpl @Inject constructor(
): UserApi {

    override fun createUser(user: User) {
        TODO()
    }

    override fun createUsersWithArrayInput(user: kotlin.Array<User>) {
        TODO()
    }

    override fun createUsersWithListInput(user: kotlin.Array<User>) {
        TODO()
    }

    override fun deleteUser(username: kotlin.String) {
        TODO()
    }

    override fun getUserByName(username: kotlin.String): User {
        TODO()
    }

    override fun loginUser(username: kotlin.String, password: kotlin.String): kotlin.String {
        TODO()
    }

    override fun logoutUser() {
        TODO()
    }

    override fun updateUser(username: kotlin.String, user: User) {
        TODO()
    }
}
