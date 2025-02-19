package org.openapitools.api

import org.openapitools.model.User
import kotlinx.coroutines.flow.Flow;
interface UserApiService {

	suspend fun createUser(user: User): Unit

	suspend fun createUsersWithArrayInput(user: Flow<User>): Unit

	suspend fun createUsersWithListInput(user: Flow<User>): Unit

	suspend fun deleteUser(username: kotlin.String): Unit

	suspend fun getUserByName(username: kotlin.String): User

	suspend fun loginUser(username: kotlin.String, password: kotlin.String): kotlin.String

	suspend fun logoutUser(): Unit

	suspend fun updateUser(username: kotlin.String, user: User): Unit
}
