package org.openapitools.api

import org.openapitools.model.User
import org.springframework.stereotype.Service
import org.springframework.http.ResponseEntity


@Service
class UserApiServiceImpl : UserApiService {

    override fun createUser(body: User): ResponseEntity<Unit> {
        TODO("Implement me")
    }

    override fun createUsersWithArrayInput(body: kotlin.collections.List<User>): ResponseEntity<Unit> {
        TODO("Implement me")
    }

    override fun createUsersWithListInput(body: kotlin.collections.List<User>): ResponseEntity<Unit> {
        TODO("Implement me")
    }

    override fun deleteUser(username: kotlin.String): ResponseEntity<Unit> {
        TODO("Implement me")
    }

    override fun getUserByName(username: kotlin.String): ResponseEntity<User> {
        TODO("Implement me")
    }

    override fun loginUser(username: kotlin.String, password: kotlin.String): ResponseEntity<kotlin.String> {
        TODO("Implement me")
    }

    override fun logoutUser(): ResponseEntity<Unit> {
        TODO("Implement me")
    }

    override fun updateUser(username: kotlin.String, body: User): ResponseEntity<Unit> {
        TODO("Implement me")
    }
}
