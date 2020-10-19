package org.openapitools.api

import org.openapitools.model.User
import kotlinx.coroutines.flow.Flow;
import org.springframework.stereotype.Service
@Service
class UserApiServiceImpl : UserApiService {

    override suspend fun createUser(body: User): Unit {
        TODO("Implement me")
    }

    override suspend fun createUsersWithArrayInput(body: Flow<User>): Unit {
        TODO("Implement me")
    }

    override suspend fun createUsersWithListInput(body: Flow<User>): Unit {
        TODO("Implement me")
    }

    override suspend fun deleteUser(username: kotlin.String): Unit {
        TODO("Implement me")
    }

    override suspend fun getUserByName(username: kotlin.String): User {
        TODO("Implement me")
    }

    override suspend fun loginUser(username: kotlin.String, password: kotlin.String): kotlin.String {
        TODO("Implement me")
    }

    override suspend fun logoutUser(): Unit {
        TODO("Implement me")
    }

    override suspend fun updateUser(username: kotlin.String, body: User): Unit {
        TODO("Implement me")
    }
}
