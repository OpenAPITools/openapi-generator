package org.openapitools.server.api.api

import org.openapitools.server.api.model.User
import jakarta.inject.Inject
import misk.testing.MiskTest
import misk.testing.MiskTestModule
import misk.web.WebTestClient


import org.junit.jupiter.api.Test

@MiskTest(startService = true)
internal class UserApiTest {

    @MiskTestModule val module = GambitTestingModule()

    @Inject private lateinit var webTestClient: WebTestClient

    /**
     * To test UserApiController.createUser
     */
    @Test
    fun `should handle createUser`() {
        val user: User = TODO()
        val response:  = webTestClient.createUser(user)

        TODO()
    }
    /**
     * To test UserApiController.createUsersWithArrayInput
     */
    @Test
    fun `should handle createUsersWithArrayInput`() {
        val user: kotlin.Array<User> = TODO()
        val response:  = webTestClient.createUsersWithArrayInput(user)

        TODO()
    }
    /**
     * To test UserApiController.createUsersWithListInput
     */
    @Test
    fun `should handle createUsersWithListInput`() {
        val user: kotlin.Array<User> = TODO()
        val response:  = webTestClient.createUsersWithListInput(user)

        TODO()
    }
    /**
     * To test UserApiController.deleteUser
     */
    @Test
    fun `should handle deleteUser`() {
        val username: kotlin.String = TODO()
        val response:  = webTestClient.deleteUser(username)

        TODO()
    }
    /**
     * To test UserApiController.getUserByName
     */
    @Test
    fun `should handle getUserByName`() {
        val username: kotlin.String = TODO()
        val response: : User = webTestClient.getUserByName(username)

        TODO()
    }
    /**
     * To test UserApiController.loginUser
     */
    @Test
    fun `should handle loginUser`() {
        val username: kotlin.String = TODO()
        val password: kotlin.String = TODO()
        val response: : kotlin.String = webTestClient.loginUser(username, password)

        TODO()
    }
    /**
     * To test UserApiController.logoutUser
     */
    @Test
    fun `should handle logoutUser`() {
        val response:  = webTestClient.logoutUser()

        TODO()
    }
    /**
     * To test UserApiController.updateUser
     */
    @Test
    fun `should handle updateUser`() {
        val username: kotlin.String = TODO()
        val user: User = TODO()
        val response:  = webTestClient.updateUser(username, user)

        TODO()
    }
}
