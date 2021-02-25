package org.openapitools.api

import org.openapitools.model.User
import kotlinx.coroutines.flow.Flow;

interface UserApiService {

    suspend fun createUser(body: User): Unit

    suspend fun createUsersWithArrayInput(body: Flow<User>): Unit

    suspend fun createUsersWithListInput(body: Flow<User>): Unit

    suspend fun deleteUser(username: kotlin.String): Unit

    suspend fun getUserByName(username: kotlin.String): User

    suspend fun loginUser(username: kotlin.String, password: kotlin.String): kotlin.String

    suspend fun logoutUser(): Unit

    suspend fun updateUser(username: kotlin.String, body: User): Unit
}
