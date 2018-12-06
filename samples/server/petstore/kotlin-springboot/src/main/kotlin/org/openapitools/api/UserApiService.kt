package org.openapitools.api

import org.openapitools.model.User

interface UserApiService {

    fun createUser(user: User): Unit

    fun createUsersWithArrayInput(user: List<User>): Unit

    fun createUsersWithListInput(user: List<User>): Unit

    fun deleteUser(username: String): Unit

    fun getUserByName(username: String): User

    fun loginUser(username: String,password: String): String

    fun logoutUser(): Unit

    fun updateUser(username: String,user: User): Unit
}
