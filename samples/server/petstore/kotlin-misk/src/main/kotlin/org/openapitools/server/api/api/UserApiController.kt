package org.openapitools.server.api.api

import org.openapitools.server.api.model.User

import javax.validation.Valid
import javax.validation.constraints.DecimalMax
import javax.validation.constraints.DecimalMin
import javax.validation.constraints.Email
import javax.validation.constraints.Max
import javax.validation.constraints.Min
import javax.validation.constraints.NotNull
import javax.validation.constraints.Pattern
import javax.validation.constraints.Size

import jakarta.inject.Inject
import jakarta.inject.Singleton

import misk.web.Delete
import misk.web.Description
import misk.web.Get
import misk.web.HttpCall
import misk.web.Patch
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
        //private val userApi: UserApi
    ) : WebAction {

        @Post("/user")
        @Description("Create user")
        @RequestContentType(MediaTypes.APPLICATION_JSON)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        fun createUser(@Valid @RequestBody user: User) {
            TODO()
        }

        @Post("/user/createWithArray")
        @Description("Creates list of users with given input array")
        @RequestContentType(MediaTypes.APPLICATION_JSON)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        fun createUsersWithArrayInput(@Valid @RequestBody user: kotlin.Array<User>) {
            TODO()
        }

        @Post("/user/createWithList")
        @Description("Creates list of users with given input array")
        @RequestContentType(MediaTypes.APPLICATION_JSON)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        fun createUsersWithListInput(@Valid @RequestBody user: kotlin.Array<User>) {
            TODO()
        }

        @Delete("/user/{username}")
        @Description("Delete user")
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        fun deleteUser(@PathParam("username") username: kotlin.String) {
            TODO()
        }

        @Get("/user/{username}")
        @Description("Get user by user name")
        @ResponseContentType(MediaTypes.APPLICATION_XML, MediaTypes.APPLICATION_JSON)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        fun getUserByName(@PathParam("username") username: kotlin.String): User {
            TODO()
        }

        @Get("/user/login")
        @Description("Logs user into the system")
        @ResponseContentType(MediaTypes.APPLICATION_XML, MediaTypes.APPLICATION_JSON)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        fun loginUser( @QueryParam(value = "username") username: kotlin.String, @QueryParam(value = "password") password: kotlin.String): kotlin.String {
            TODO()
        }

        @Get("/user/logout")
        @Description("Logs out current logged in user session")
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        fun logoutUser() {
            TODO()
        }

        @Put("/user/{username}")
        @Description("Updated user")
        @RequestContentType(MediaTypes.APPLICATION_JSON)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        fun updateUser(@PathParam("username") username: kotlin.String,@Valid @RequestBody user: User) {
            TODO()
        }
    }
