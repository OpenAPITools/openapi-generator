package org.openapitools.api

import org.openapitools.model.User
import org.springframework.stereotype.Service
@Service
class UserApiServiceImpl : UserApiService {

    override fun createUser(user: User): Unit {
        TODO("Implement me")
    }

    override fun createUsersWithArrayInput(user: kotlin.collections.List<User>): Unit {
        TODO("Implement me")
    }

    override fun createUsersWithListInput(user: kotlin.collections.List<User>): Unit {
        TODO("Implement me")
    }

    override fun deleteUser(username: kotlin.String): Unit {
        TODO("Implement me")
    }

    override fun getUserByName(username: kotlin.String): User {
        TODO("Implement me")
    }

    override fun loginUser(username: kotlin.String, password: kotlin.String): kotlin.String {
        TODO("Implement me")
    }

    override fun logoutUser(): Unit {
        TODO("Implement me")
    }

    override fun updateUser(username: kotlin.String, user: User): Unit {
        TODO("Implement me")
    }
}
