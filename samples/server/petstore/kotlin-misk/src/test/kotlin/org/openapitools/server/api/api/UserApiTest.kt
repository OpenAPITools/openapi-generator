package org.openapitools.server.api.api

import jakarta.inject.Inject
import misk.testing.MiskTest
import org.junit.jupiter.api.Test

import misk.web.HttpCall
import misk.web.PathParam
import misk.web.QueryParam
import misk.web.RequestBody
import misk.web.RequestHeader

import org.openapitools.server.api.model.User

@MiskTest(startService = true)
internal class UserApiTest {

    @Inject private lateinit var userApi: UserApi

    /**
     * To test UserApiController.createUser
     */
    @Test
    fun `should handle createUser`() {
        val user = TODO()
        val response = userApi.createUser(user)
    }

    /**
     * To test UserApiController.createUsersWithArrayInput
     */
    @Test
    fun `should handle createUsersWithArrayInput`() {
        val user = TODO()
        val response = userApi.createUsersWithArrayInput(user)
    }

    /**
     * To test UserApiController.createUsersWithListInput
     */
    @Test
    fun `should handle createUsersWithListInput`() {
        val user = TODO()
        val response = userApi.createUsersWithListInput(user)
    }

    /**
     * To test UserApiController.deleteUser
     */
    @Test
    fun `should handle deleteUser`() {
        val username = TODO()
        val response = userApi.deleteUser(username)
    }

    /**
     * To test UserApiController.getUserByName
     */
    @Test
    fun `should handle getUserByName`() {
        val username = TODO()
        val response: User = userApi.getUserByName(username)
    }

    /**
     * To test UserApiController.loginUser
     */
    @Test
    fun `should handle loginUser`() {
        val username = TODO()
        val password = TODO()
        val response: kotlin.String = userApi.loginUser(username, password)
    }

    /**
     * To test UserApiController.logoutUser
     */
    @Test
    fun `should handle logoutUser`() {
        val response = userApi.logoutUser()
    }

    /**
     * To test UserApiController.updateUser
     */
    @Test
    fun `should handle updateUser`() {
        val username = TODO()
        val user = TODO()
        val response = userApi.updateUser(username, user)
    }

}
