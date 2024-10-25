package org.openapitools.server.apis

import java.time.OffsetDateTime
import org.openapitools.server.models.User
import io.javalin.http.Context

class UserApiServiceImpl : UserApiService {

    override fun createUser(user: User, ctx: Context): Unit {
        TODO("Implement me")
    }

    override fun createUsersWithArrayInput(user: List<User>, ctx: Context): Unit {
        TODO("Implement me")
    }

    override fun createUsersWithListInput(user: List<User>, ctx: Context): Unit {
        TODO("Implement me")
    }

    override fun deleteUser(username: String, ctx: Context): Unit {
        TODO("Implement me")
    }

    override fun getUserByName(username: String, ctx: Context): User {
        TODO("Implement me")
    }

    override fun loginUser(username: String, password: String, ctx: Context): String {
        TODO("Implement me")
    }

    override fun logoutUser(ctx: Context): Unit {
        TODO("Implement me")
    }

    override fun updateUser(username: String, user: User, ctx: Context): Unit {
        TODO("Implement me")
    }
}
