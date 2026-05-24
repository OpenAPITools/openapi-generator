package org.openapitools.server

import io.ktor.client.call.*
import io.ktor.client.request.*
import io.ktor.http.*
import io.ktor.server.plugins.di.*
import io.ktor.server.routing.*
import org.openapitools.server.apis.UserApiDelegate
import org.openapitools.server.models.User
import kotlin.test.Test
import kotlin.test.assertEquals

class UserApiTest {

    @Test
    fun testCreateUserShouldCreateUser() = petstoreTestApplication {
        var receivedUser: User? = null
        class UserApiImpl: UserApiDelegate {
            override suspend fun createUser(user: User, call: RoutingCall) {
                receivedUser = user
            }
        }
        application.dependencies.provide<UserApiDelegate> { UserApiImpl() }
        val user = User(id = 1, username = "testuser")
        val response = client.post("/user") {
            contentType(ContentType.Application.Json)
            setBody(user)
        }
        assertEquals(HttpStatusCode.OK, response.status)
        assertEquals(user, receivedUser)
    }

    @Test
    fun testCreateUsersWithArrayInputShouldCreateUsers() = petstoreTestApplication {
        var receivedUsers: List<User>? = null
        class UserApiImpl: UserApiDelegate {
            override suspend fun createUsersWithArrayInput(user: List<User>, call: RoutingCall) {
                receivedUsers = user
            }
        }
        application.dependencies.provide<UserApiDelegate> { UserApiImpl() }
        val users = listOf(User(id = 1, username = "testuser"))
        val response = client.post("/user/createWithArray") {
            contentType(ContentType.Application.Json)
            setBody(users)
        }
        assertEquals(HttpStatusCode.OK, response.status)
        assertEquals(users, receivedUsers)
    }

    @Test
    fun testCreateUsersWithListInputShouldCreateUsers() = petstoreTestApplication {
        var receivedUsers: List<User>? = null
        class UserApiImpl: UserApiDelegate {
            override suspend fun createUsersWithListInput(user: List<User>, call: RoutingCall) {
                receivedUsers = user
            }
        }
        application.dependencies.provide<UserApiDelegate> { UserApiImpl() }
        val users = listOf(User(id = 1, username = "testuser"))
        val response = client.post("/user/createWithList") {
            contentType(ContentType.Application.Json)
            setBody(users)
        }
        assertEquals(HttpStatusCode.OK, response.status)
        assertEquals(users, receivedUsers)
    }

    @Test
    fun testLoginUserShouldReturnToken() = petstoreTestApplication {
        class UserApiImpl: UserApiDelegate {
            override suspend fun loginUser(username: String, password: String, call: RoutingCall): String {
                return "test-token"
            }
        }
        application.dependencies.provide<UserApiDelegate> { UserApiImpl() }
        val response = client.get("/user/login?username=testuser&password=password")
        assertEquals(HttpStatusCode.OK, response.status)
        assertEquals("test-token", response.body<String>())
    }

    @Test
    fun testLogoutUserShouldSucceed() = petstoreTestApplication {
        var logoutCalled = false
        class UserApiImpl: UserApiDelegate {
            override suspend fun logoutUser(call: RoutingCall) {
                logoutCalled = true
            }
        }
        application.dependencies.provide<UserApiDelegate> { UserApiImpl() }
        val response = client.get("/user/logout")
        assertEquals(HttpStatusCode.OK, response.status)
        assertEquals(true, logoutCalled)
    }

    @Test
    fun testGetUserByNameShouldReturnUser() = petstoreTestApplication {
        val user = User(id = 1, username = "testuser")
        class UserApiImpl: UserApiDelegate {
            override suspend fun getUserByName(username: String, call: RoutingCall): User {
                return user
            }
        }
        application.dependencies.provide<UserApiDelegate> { UserApiImpl() }
        val response = client.get("/user/testuser")
        assertEquals(HttpStatusCode.OK, response.status)
        assertEquals(user, response.body<User>())
    }

    @Test
    fun testUpdateUserShouldUpdateUser() = petstoreTestApplication {
        var receivedUsername: String? = null
        var receivedUser: User? = null
        class UserApiImpl: UserApiDelegate {
            override suspend fun updateUser(username: String, user: User, call: RoutingCall) {
                receivedUsername = username
                receivedUser = user
            }
        }
        application.dependencies.provide<UserApiDelegate> { UserApiImpl() }
        val user = User(id = 1, username = "testuser")
        val response = client.put("/user/testuser") {
            contentType(ContentType.Application.Json)
            setBody(user)
        }
        assertEquals(HttpStatusCode.OK, response.status)
        assertEquals("testuser", receivedUsername)
        assertEquals(user, receivedUser)
    }

    @Test
    fun testDeleteUserShouldDeleteUser() = petstoreTestApplication {
        var receivedUsername: String? = null
        class UserApiImpl: UserApiDelegate {
            override suspend fun deleteUser(username: String, call: RoutingCall) {
                receivedUsername = username
            }
        }
        application.dependencies.provide<UserApiDelegate> { UserApiImpl() }
        val response = client.delete("/user/testuser")
        assertEquals(HttpStatusCode.OK, response.status)
        assertEquals("testuser", receivedUsername)
    }

    @Test
    fun testLoginUserShouldRejectMissingParameters() = petstoreTestApplication {
        val response = client.get("/user/login")
        assertEquals(HttpStatusCode.NotImplemented, response.status)
    }

    @Test
    fun testLoginUserShouldRejectInvalidUsername() = petstoreTestApplication {
        val response = client.get("/user/login?username=a&password=p")
        assertEquals(HttpStatusCode.BadRequest, response.status)
    }
}
