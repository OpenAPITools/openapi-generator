package org.openapitools.server.api.api

import org.openapitools.server.api.model.User


import jakarta.inject.Inject
import jakarta.inject.Singleton

// TODO("Only import what we need")
import misk.web.Delete
import misk.web.Description
import misk.web.Get
import misk.web.HttpCall
import misk.web.PathParam
import misk.web.Post
import misk.web.Put
import misk.web.QueryParam
import misk.web.RequestBody
import misk.web.RequestContentType
import misk.web.RequestHeaders
import misk.web.ResponseContentType
import misk.web.actions.WebAction
import misk.web.interceptors.LogRequestResponse
import misk.web.mediatype.MediaTypes
import okhttp3.Headers

    @Singleton
    class UserApiController @Inject constructor(
        private val userApi: UserApi
    ) : UserApi, WebAction {

        @Post("/user")
        @Description("Create user")
        @RequestContentType(MediaTypes.APPLICATION_JSON)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        @Suppress("unused")
        override fun createUser(@RequestBody(required = false) user: User) =
            userApi.createUser(user)

        @Post("/user/createWithArray")
        @Description("Creates list of users with given input array")
        @RequestContentType(MediaTypes.APPLICATION_JSON)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        @Suppress("unused")
        override fun createUsersWithArrayInput(@RequestBody(required = false) user: kotlin.Array<User>) =
            userApi.createUsersWithArrayInput(user)

        @Post("/user/createWithList")
        @Description("Creates list of users with given input array")
        @RequestContentType(MediaTypes.APPLICATION_JSON)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        @Suppress("unused")
        override fun createUsersWithListInput(@RequestBody(required = false) user: kotlin.Array<User>) =
            userApi.createUsersWithListInput(user)

        @Delete("/user/{username}")
        @Description("Delete user")
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        @Suppress("unused")
        override fun deleteUser(@PathParam("username") username: kotlin.String) =
            userApi.deleteUser(username)

        @Get("/user/{username}")
        @Description("Get user by user name")
        @ResponseContentType(MediaTypes.APPLICATION_XML, MediaTypes.APPLICATION_JSON)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        @Suppress("unused")
        override fun getUserByName(@PathParam("username") username: kotlin.String) =
            userApi.getUserByName(username)

        @Get("/user/login")
        @Description("Logs user into the system")
        @ResponseContentType(MediaTypes.APPLICATION_XML, MediaTypes.APPLICATION_JSON)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        @Suppress("unused")
        override fun loginUser(@QueryParam username: kotlin.String,@QueryParam password: kotlin.String) =
            userApi.loginUser(username, password)

        @Get("/user/logout")
        @Description("Logs out current logged in user session")
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        @Suppress("unused")
        override fun logoutUser() =
            userApi.logoutUser()

        @Put("/user/{username}")
        @Description("Updated user")
        @RequestContentType(MediaTypes.APPLICATION_JSON)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        @Suppress("unused")
        override fun updateUser(@PathParam("username") username: kotlin.String,@RequestBody(required = false) user: User) =
            userApi.updateUser(username, user)
    }
