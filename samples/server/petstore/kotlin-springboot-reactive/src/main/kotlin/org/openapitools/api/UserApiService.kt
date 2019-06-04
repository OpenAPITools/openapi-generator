package org.openapitools.api

import org.openapitools.model.User
import kotlinx.coroutines.flow.Flow;
interface UserApiService {

	suspend fun createUser(body: User): Unit

	suspend fun createUsersWithArrayInput(body: Flow<User>): Unit

	suspend fun createUsersWithListInput(body: Flow<User>): Unit

	suspend fun deleteUser(username: String): Unit

	suspend fun getUserByName(username: String): User

	suspend fun loginUser(username: String, password: String): String

	suspend fun logoutUser(): Unit

	suspend fun updateUser(username: String, body: User): Unit
}
