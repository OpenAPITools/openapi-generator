package org.openapitools.api

import org.openapitools.model.User
interface UserApiService {

	fun createUser(body: User): Unit

	fun createUsersWithArrayInput(body: kotlin.collections.List<User>): Unit

	fun createUsersWithListInput(body: kotlin.collections.List<User>): Unit

	fun deleteUser(username: kotlin.String): Unit

	fun getUserByName(username: kotlin.String): User

	fun loginUser(username: kotlin.String, password: kotlin.String): kotlin.String

	fun logoutUser(): Unit

	fun updateUser(username: kotlin.String, body: User): Unit
}
