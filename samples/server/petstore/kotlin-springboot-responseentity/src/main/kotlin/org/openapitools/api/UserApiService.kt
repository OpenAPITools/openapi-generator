package org.openapitools.api

import org.openapitools.model.User
import org.springframework.http.ResponseEntity


interface UserApiService {

    fun createUser(body: User): ResponseEntity<Unit>

    fun createUsersWithArrayInput(body: kotlin.collections.List<User>): ResponseEntity<Unit>

    fun createUsersWithListInput(body: kotlin.collections.List<User>): ResponseEntity<Unit>

    fun deleteUser(username: kotlin.String): ResponseEntity<Unit>

    fun getUserByName(username: kotlin.String): ResponseEntity<User>

    fun loginUser(username: kotlin.String, password: kotlin.String): ResponseEntity<kotlin.String>

    fun logoutUser(): ResponseEntity<Unit>

    fun updateUser(username: kotlin.String, body: User): ResponseEntity<Unit>
}
