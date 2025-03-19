package org.openapitools.server.api.api

import org.openapitools.server.api.model.User
import okhttp3.Headers

interface UserApi {

    fun createUser(user: User) 

    fun createUsersWithArrayInput(user: kotlin.Array<User>) 

    fun createUsersWithListInput(user: kotlin.Array<User>) 

    fun deleteUser(username: kotlin.String) 

    fun getUserByName(username: kotlin.String) : User

    fun loginUser(username: kotlin.String, password: kotlin.String) : kotlin.String

    fun logoutUser() 

    fun updateUser(username: kotlin.String, user: User) 
}
