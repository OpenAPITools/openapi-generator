package org.openapitools.api

import org.openapitools.model.User
import kotlinx.coroutines.flow.Flow;
import org.springframework.stereotype.Service
@Service
class UserApiServiceImpl : UserApiService {

    override suspend fun createUser(user: User): Unit {
        TODO("Implement me")
    }

    override suspend fun createUsersWithArrayInput(user: Flow<User>): Unit {
        TODO("Implement me")
    }

    override suspend fun createUsersWithListInput(user: Flow<User>): Unit {
        TODO("Implement me")
    }

    override suspend fun deleteUser(username: String): Unit {
        TODO("Implement me")
    }

    override suspend fun getUserByName(username: String): User {
        TODO("Implement me")
    }

    override suspend fun loginUser(username: String, password: String): String {
        TODO("Implement me")
    }

    override suspend fun logoutUser(): Unit {
        TODO("Implement me")
    }

    override suspend fun updateUser(username: String, user: User): Unit {
        TODO("Implement me")
    }
}
