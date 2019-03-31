package org.openapitools.api

import org.openapitools.model.User

interface UserApiService {

    fun createUser(body: User): Unit

    fun createUsersWithArrayInput(body: List<User>): Unit

    fun createUsersWithListInput(body: List<User>): Unit

    fun deleteUser(username: String): Unit

    fun getUserByName(username: String): User

    fun loginUser(username: String, password: String): String

    fun logoutUser(): Unit

    fun updateUser(username: String, body: User): Unit
}
