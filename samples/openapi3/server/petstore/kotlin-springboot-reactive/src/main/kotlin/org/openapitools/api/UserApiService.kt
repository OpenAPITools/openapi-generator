package org.openapitools.api

import org.openapitools.model.User
import kotlinx.coroutines.flow.Flow;
interface UserApiService {

	suspend fun createUser(user: User): Unit

	suspend fun createUsersWithArrayInput(user: Flow<User>): Unit

	suspend fun createUsersWithListInput(user: Flow<User>): Unit

	suspend fun deleteUser(username: String): Unit

	suspend fun getUserByName(username: String): User

	suspend fun loginUser(username: String, password: String): String

	suspend fun logoutUser(): Unit

	suspend fun updateUser(username: String, user: User): Unit
}
