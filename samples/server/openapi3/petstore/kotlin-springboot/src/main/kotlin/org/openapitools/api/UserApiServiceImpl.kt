package org.openapitools.api

import org.openapitools.model.User
import org.springframework.stereotype.Service

@Service
class UserApiServiceImpl : UserApiService {

    override fun createUser(user: User): Unit {
        TODO("Implement me")
    }

    override fun createUsersWithArrayInput(user: List<User>): Unit {
        TODO("Implement me")
    }

    override fun createUsersWithListInput(user: List<User>): Unit {
        TODO("Implement me")
    }

    override fun deleteUser(username: String): Unit {
        TODO("Implement me")
    }

    override fun getUserByName(username: String): User {
        TODO("Implement me")
    }

    override fun loginUser(username: String,password: String): String {
        TODO("Implement me")
    }

    override fun logoutUser(): Unit {
        TODO("Implement me")
    }

    override fun updateUser(username: String,user: User): Unit {
        TODO("Implement me")
    }
}
