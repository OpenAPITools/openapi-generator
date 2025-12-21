package org.openapitools.api

import org.openapitools.model.User
import org.springframework.http.HttpStatus
import org.springframework.http.MediaType

import org.springframework.web.bind.annotation.*
import org.springframework.validation.annotation.Validated
import org.springframework.web.context.request.NativeWebRequest
import org.springframework.beans.factory.annotation.Autowired
import org.openapitools.api.UserApiController.Companion.BASE_PATH

import javax.validation.Valid
import javax.validation.constraints.DecimalMax
import javax.validation.constraints.DecimalMin
import javax.validation.constraints.Email
import javax.validation.constraints.Max
import javax.validation.constraints.Min
import javax.validation.constraints.NotNull
import javax.validation.constraints.Pattern
import javax.validation.constraints.Size

import kotlin.collections.List
import kotlin.collections.Map

@RestController
@Validated
@RequestMapping("\${openapi.openAPIPetstore.base-path:\${api.base-path:$BASE_PATH}}")
class UserApiController(@Autowired(required = true) val service: UserApiService) {

    @ResponseStatus(HttpStatus.OK)
    @RequestMapping(
        method = [RequestMethod.POST],
        value = [PATH_CREATE_USER /* "/user" */]
    )
    fun createUser(
        @Valid @RequestBody body: User
    ): Unit {
        return service.createUser(body)
    }

    @ResponseStatus(HttpStatus.OK)
    @RequestMapping(
        method = [RequestMethod.POST],
        value = [PATH_CREATE_USERS_WITH_ARRAY_INPUT /* "/user/createWithArray" */]
    )
    fun createUsersWithArrayInput(
        @Valid @RequestBody body: kotlin.collections.List<User>
    ): Unit {
        return service.createUsersWithArrayInput(body)
    }

    @ResponseStatus(HttpStatus.OK)
    @RequestMapping(
        method = [RequestMethod.POST],
        value = [PATH_CREATE_USERS_WITH_LIST_INPUT /* "/user/createWithList" */]
    )
    fun createUsersWithListInput(
        @Valid @RequestBody body: kotlin.collections.List<User>
    ): Unit {
        return service.createUsersWithListInput(body)
    }

    @ResponseStatus(HttpStatus.BAD_REQUEST)
    @RequestMapping(
        method = [RequestMethod.DELETE],
        value = [PATH_DELETE_USER /* "/user/{username}" */]
    )
    fun deleteUser(
        @PathVariable("username") username: kotlin.String
    ): Unit {
        return service.deleteUser(username)
    }

    @ResponseStatus(HttpStatus.OK)
    @RequestMapping(
        method = [RequestMethod.GET],
        value = [PATH_GET_USER_BY_NAME /* "/user/{username}" */],
        produces = ["application/xml", "application/json"]
    )
    fun getUserByName(
        @PathVariable("username") username: kotlin.String
    ): User {
        return service.getUserByName(username)
    }

    @ResponseStatus(HttpStatus.OK)
    @RequestMapping(
        method = [RequestMethod.GET],
        value = [PATH_LOGIN_USER /* "/user/login" */],
        produces = ["application/xml", "application/json"]
    )
    fun loginUser(
        @NotNull @Valid @RequestParam(value = "username", required = true) username: kotlin.String,
        @NotNull @Valid @RequestParam(value = "password", required = true) password: kotlin.String
    ): kotlin.String {
        return service.loginUser(username, password)
    }

    @ResponseStatus(HttpStatus.OK)
    @RequestMapping(
        method = [RequestMethod.GET],
        value = [PATH_LOGOUT_USER /* "/user/logout" */]
    )
    fun logoutUser(): Unit {
        return service.logoutUser()
    }

    @ResponseStatus(HttpStatus.BAD_REQUEST)
    @RequestMapping(
        method = [RequestMethod.PUT],
        value = [PATH_UPDATE_USER /* "/user/{username}" */]
    )
    fun updateUser(
        @PathVariable("username") username: kotlin.String,
        @Valid @RequestBody body: User
    ): Unit {
        return service.updateUser(username, body)
    }

    companion object {
        //for your own safety never directly reuse these path definitions in tests
        const val BASE_PATH: String = "/v2"
        const val PATH_CREATE_USER: String = "/user"
        const val PATH_CREATE_USERS_WITH_ARRAY_INPUT: String = "/user/createWithArray"
        const val PATH_CREATE_USERS_WITH_LIST_INPUT: String = "/user/createWithList"
        const val PATH_DELETE_USER: String = "/user/{username}"
        const val PATH_GET_USER_BY_NAME: String = "/user/{username}"
        const val PATH_LOGIN_USER: String = "/user/login"
        const val PATH_LOGOUT_USER: String = "/user/logout"
        const val PATH_UPDATE_USER: String = "/user/{username}"
    }
}
