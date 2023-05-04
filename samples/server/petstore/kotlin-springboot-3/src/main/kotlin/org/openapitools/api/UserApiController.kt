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
@RequestMapping("\${api.base-path:/v2}")
class UserApiController(@Autowired(required = true) val service: UserApiService) {


    @RequestMapping(
        method = [RequestMethod.POST],
        value = ["/user"],
        consumes = ["application/json"]
    )
    fun createUser( @Valid @RequestBody user: User): ResponseEntity<Unit> {
        return ResponseEntity(service.createUser(user), HttpStatus.valueOf(200))
    }


    @RequestMapping(
        method = [RequestMethod.POST],
        value = ["/user/createWithArray"],
        consumes = ["application/json"]
    )
    fun createUsersWithArrayInput( @Valid @RequestBody user: kotlin.collections.List<User>): ResponseEntity<Unit> {
        return ResponseEntity(service.createUsersWithArrayInput(user), HttpStatus.valueOf(200))
    }


    @RequestMapping(
        method = [RequestMethod.POST],
        value = ["/user/createWithList"],
        consumes = ["application/json"]
    )
    fun createUsersWithListInput( @Valid @RequestBody user: kotlin.collections.List<User>): ResponseEntity<Unit> {
        return ResponseEntity(service.createUsersWithListInput(user), HttpStatus.valueOf(200))
    }


    @RequestMapping(
        method = [RequestMethod.DELETE],
        value = ["/user/{username}"]
    )
    fun deleteUser( @PathVariable("username") username: kotlin.String): ResponseEntity<Unit> {
        return ResponseEntity(service.deleteUser(username), HttpStatus.valueOf(400))
    }


    @RequestMapping(
        method = [RequestMethod.GET],
        value = ["/user/{username}"],
        produces = ["application/xml", "application/json"]
    )
    fun getUserByName( @PathVariable("username") username: kotlin.String): ResponseEntity<User> {
        return ResponseEntity(service.getUserByName(username), HttpStatus.valueOf(200))
    }


    @RequestMapping(
        method = [RequestMethod.GET],
        value = ["/user/login"],
        produces = ["application/xml", "application/json"]
    )
    fun loginUser(@NotNull @Pattern(regexp="^[a-zA-Z0-9]+[a-zA-Z0-9\\.\\-_]*[a-zA-Z0-9]+$")  @Valid @RequestParam(value = "username", required = true) username: kotlin.String,@NotNull  @Valid @RequestParam(value = "password", required = true) password: kotlin.String): ResponseEntity<kotlin.String> {
        return ResponseEntity(service.loginUser(username, password), HttpStatus.valueOf(200))
    }


    @RequestMapping(
        method = [RequestMethod.GET],
        value = ["/user/logout"]
    )
    fun logoutUser(): ResponseEntity<Unit> {
        return ResponseEntity(service.logoutUser(), HttpStatus.valueOf(200))
    }


    @RequestMapping(
        method = [RequestMethod.PUT],
        value = ["/user/{username}"],
        consumes = ["application/json"]
    )
    fun updateUser( @PathVariable("username") username: kotlin.String, @Valid @RequestBody user: User): ResponseEntity<Unit> {
        return ResponseEntity(service.updateUser(username, user), HttpStatus.valueOf(400))
    }
}
