package org.openapitools.api

import org.openapitools.model.User
import org.springframework.http.HttpStatus
import org.springframework.http.MediaType
import org.springframework.http.ResponseEntity

import org.springframework.web.bind.annotation.*
import org.springframework.validation.annotation.Validated
import org.springframework.web.context.request.NativeWebRequest
import org.springframework.beans.factory.annotation.Autowired

import jakarta.validation.Valid
import jakarta.validation.constraints.DecimalMax
import jakarta.validation.constraints.DecimalMin
import jakarta.validation.constraints.Email
import jakarta.validation.constraints.Max
import jakarta.validation.constraints.Min
import jakarta.validation.constraints.NotNull
import jakarta.validation.constraints.Pattern
import jakarta.validation.constraints.Size

import kotlin.collections.List
import kotlin.collections.Map

@RestController
@Validated
class UserApiController(@Autowired(required = true) val service: UserApiService) {


    @RequestMapping(
        method = [RequestMethod.POST],
        // "/user"
        value = [PATH_CREATE_USER],
        consumes = ["application/json"]
    )
    fun createUser(
        @Valid @RequestBody user: User
    ): ResponseEntity<Unit> {
        return ResponseEntity(service.createUser(user), HttpStatus.valueOf(200))
    }


    @RequestMapping(
        method = [RequestMethod.POST],
        // "/user/createWithArray"
        value = [PATH_CREATE_USERS_WITH_ARRAY_INPUT],
        consumes = ["application/json"]
    )
    fun createUsersWithArrayInput(
        @Valid @RequestBody user: kotlin.collections.List<User>
    ): ResponseEntity<Unit> {
        return ResponseEntity(service.createUsersWithArrayInput(user), HttpStatus.valueOf(200))
    }


    @RequestMapping(
        method = [RequestMethod.POST],
        // "/user/createWithList"
        value = [PATH_CREATE_USERS_WITH_LIST_INPUT],
        consumes = ["application/json"]
    )
    fun createUsersWithListInput(
        @Valid @RequestBody user: kotlin.collections.List<User>
    ): ResponseEntity<Unit> {
        return ResponseEntity(service.createUsersWithListInput(user), HttpStatus.valueOf(200))
    }


    @RequestMapping(
        method = [RequestMethod.DELETE],
        // "/user/{username}"
        value = [PATH_DELETE_USER]
    )
    fun deleteUser(
        @PathVariable("username") username: kotlin.String
    ): ResponseEntity<Unit> {
        return ResponseEntity(service.deleteUser(username), HttpStatus.valueOf(400))
    }


    @RequestMapping(
        method = [RequestMethod.GET],
        // "/user/{username}"
        value = [PATH_GET_USER_BY_NAME],
        produces = ["application/xml", "application/json"]
    )
    fun getUserByName(
        @PathVariable("username") username: kotlin.String
    ): ResponseEntity<User> {
        return ResponseEntity(service.getUserByName(username), HttpStatus.valueOf(200))
    }


    @RequestMapping(
        method = [RequestMethod.GET],
        // "/user/login"
        value = [PATH_LOGIN_USER],
        produces = ["application/xml", "application/json"]
    )
    fun loginUser(
        @NotNull @Pattern(regexp="^[a-zA-Z0-9]+[a-zA-Z0-9\\.\\-_]*[a-zA-Z0-9]+$") @Valid @RequestParam(value = "username", required = true) username: kotlin.String,
        @NotNull @Valid @RequestParam(value = "password", required = true) password: kotlin.String
    ): ResponseEntity<kotlin.String> {
        return ResponseEntity(service.loginUser(username, password), HttpStatus.valueOf(200))
    }


    @RequestMapping(
        method = [RequestMethod.GET],
        // "/user/logout"
        value = [PATH_LOGOUT_USER]
    )
    fun logoutUser(): ResponseEntity<Unit> {
        return ResponseEntity(service.logoutUser(), HttpStatus.valueOf(200))
    }


    @RequestMapping(
        method = [RequestMethod.PUT],
        // "/user/{username}"
        value = [PATH_UPDATE_USER],
        consumes = ["application/json"]
    )
    fun updateUser(
        @PathVariable("username") username: kotlin.String,
        @Valid @RequestBody user: User
    ): ResponseEntity<Unit> {
        return ResponseEntity(service.updateUser(username, user), HttpStatus.valueOf(400))
    }

    companion object {
        //for your own safety never directly reuse these path definitions in tests
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
