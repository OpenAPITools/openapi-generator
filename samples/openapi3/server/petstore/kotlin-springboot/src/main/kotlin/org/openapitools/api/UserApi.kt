package org.openapitools.api

import org.openapitools.model.User
import org.springframework.http.HttpStatus
import org.springframework.http.MediaType
import org.springframework.http.ResponseEntity

import org.springframework.web.bind.annotation.RequestBody
import org.springframework.web.bind.annotation.RequestPart
import org.springframework.web.bind.annotation.RequestParam
import org.springframework.web.bind.annotation.PathVariable
import org.springframework.web.bind.annotation.RequestHeader
import org.springframework.web.bind.annotation.RequestMethod
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.web.bind.annotation.RestController
import org.springframework.validation.annotation.Validated
import org.springframework.web.context.request.NativeWebRequest
import org.springframework.beans.factory.annotation.Autowired

import javax.validation.Valid
import javax.validation.constraints.DecimalMax
import javax.validation.constraints.DecimalMin
import javax.validation.constraints.Max
import javax.validation.constraints.Min
import javax.validation.constraints.NotNull
import javax.validation.constraints.Pattern
import javax.validation.constraints.Size

import kotlin.collections.List
import kotlin.collections.Map

@RestController
@Validated
@RequestMapping("\${api.base-path:/v2}")
class UserApiController() {


    @RequestMapping(
        value = ["/user"],
        consumes = ["application/json"],
        method = [RequestMethod.POST])
    fun createUser( @Valid @RequestBody user: User
): ResponseEntity<Unit> {
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)
    }


    @RequestMapping(
        value = ["/user/createWithArray"],
        consumes = ["application/json"],
        method = [RequestMethod.POST])
    fun createUsersWithArrayInput( @Valid @RequestBody user: kotlin.collections.List<User>
): ResponseEntity<Unit> {
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)
    }


    @RequestMapping(
        value = ["/user/createWithList"],
        consumes = ["application/json"],
        method = [RequestMethod.POST])
    fun createUsersWithListInput( @Valid @RequestBody user: kotlin.collections.List<User>
): ResponseEntity<Unit> {
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)
    }


    @RequestMapping(
        value = ["/user/{username}"],
        method = [RequestMethod.DELETE])
    fun deleteUser( @PathVariable("username") username: kotlin.String
): ResponseEntity<Unit> {
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)
    }


    @RequestMapping(
        value = ["/user/{username}"],
        produces = ["application/xml", "application/json"], 
        method = [RequestMethod.GET])
    fun getUserByName( @PathVariable("username") username: kotlin.String
): ResponseEntity<User> {
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)
    }


    @RequestMapping(
        value = ["/user/login"],
        produces = ["application/xml", "application/json"], 
        method = [RequestMethod.GET])
    fun loginUser(@NotNull @Pattern(regexp="^[a-zA-Z0-9]+[a-zA-Z0-9\\.\\-_]*[a-zA-Z0-9]+$")  @RequestParam(value = "username", required = true) username: kotlin.String
,@NotNull  @RequestParam(value = "password", required = true) password: kotlin.String
): ResponseEntity<kotlin.String> {
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)
    }


    @RequestMapping(
        value = ["/user/logout"],
        method = [RequestMethod.GET])
    fun logoutUser(): ResponseEntity<Unit> {
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)
    }


    @RequestMapping(
        value = ["/user/{username}"],
        consumes = ["application/json"],
        method = [RequestMethod.PUT])
    fun updateUser( @PathVariable("username") username: kotlin.String
, @Valid @RequestBody user: User
): ResponseEntity<Unit> {
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)
    }
}
