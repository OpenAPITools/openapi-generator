package org.openapitools.server.apis

import org.openapitools.server.models.User
import io.javalin.http.Context

class UserApiServiceImpl : UserApiService {

    override fun createUser(user: User, ctx: Context): Unit {
        TODO("Implement me")
    }

    override fun createUsersWithArrayInput(user: kotlin.collections.List<User>, ctx: Context): Unit {
        TODO("Implement me")
    }

    override fun createUsersWithListInput(user: kotlin.collections.List<User>, ctx: Context): Unit {
        TODO("Implement me")
    }

    override fun deleteUser(username: kotlin.String, ctx: Context): Unit {
        TODO("Implement me")
    }

    override fun getUserByName(username: kotlin.String, ctx: Context): User {
        TODO("Implement me")
    }

    override fun loginUser(username: kotlin.String, password: kotlin.String, ctx: Context): kotlin.String {
        TODO("Implement me")
    }

    override fun logoutUser(ctx: Context): Unit {
        TODO("Implement me")
    }

    override fun updateUser(username: kotlin.String, user: User, ctx: Context): Unit {
        TODO("Implement me")
    }
}
