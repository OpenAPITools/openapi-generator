package org.openapitools.api

import org.openapitools.model.User

interface UserApiService {

    fun createUser(user: User): Unit

    fun createUsersWithArrayInput(user: kotlin.Array<User>): Unit

    fun createUsersWithListInput(user: kotlin.Array<User>): Unit

    fun deleteUser(username: kotlin.String): Unit

    fun getUserByName(username: kotlin.String): User

    fun loginUser(username: kotlin.String,password: kotlin.String): kotlin.String

    fun logoutUser(): Unit

    fun updateUser(username: kotlin.String,user: User): Unit
}
