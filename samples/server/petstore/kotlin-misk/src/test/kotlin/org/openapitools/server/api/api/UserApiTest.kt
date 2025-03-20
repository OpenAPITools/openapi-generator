package org.openapitools.server.api.api

import org.openapitools.server.api.model.User
import jakarta.inject.Inject
import misk.testing.MiskTest

import org.junit.jupiter.api.Test

@MiskTest(startService = true)
internal class UserApiTest {

    @Inject private lateinit var UserApi : UserApi

    /**
     * To test UserApiController.createUser
     */
    @Test
    fun `should handle createUser`() {
        val user: User = TODO()
        val response = UserApi.createUser(user)
    }

    /**
     * To test UserApiController.createUsersWithArrayInput
     */
    @Test
    fun `should handle createUsersWithArrayInput`() {
        val user: kotlin.Array<User> = TODO()
        val response = UserApi.createUsersWithArrayInput(user)
    }

    /**
     * To test UserApiController.createUsersWithListInput
     */
    @Test
    fun `should handle createUsersWithListInput`() {
        val user: kotlin.Array<User> = TODO()
        val response = UserApi.createUsersWithListInput(user)
    }

    /**
     * To test UserApiController.deleteUser
     */
    @Test
    fun `should handle deleteUser`() {
        val username: kotlin.String = TODO()
        val response = UserApi.deleteUser(username)
    }

    /**
     * To test UserApiController.getUserByName
     */
    @Test
    fun `should handle getUserByName`() {
        val username: kotlin.String = TODO()
        val response: User = UserApi.getUserByName(username)
    }

    /**
     * To test UserApiController.loginUser
     */
    @Test
    fun `should handle loginUser`() {
        val username: kotlin.String = TODO()
        val password: kotlin.String = TODO()
        val response: kotlin.String = UserApi.loginUser(username, password)
    }

    /**
     * To test UserApiController.logoutUser
     */
    @Test
    fun `should handle logoutUser`() {
        val response = UserApi.logoutUser()
    }

    /**
     * To test UserApiController.updateUser
     */
    @Test
    fun `should handle updateUser`() {
        val username: kotlin.String = TODO()
        val user: User = TODO()
        val response = UserApi.updateUser(username, user)
    }

}
