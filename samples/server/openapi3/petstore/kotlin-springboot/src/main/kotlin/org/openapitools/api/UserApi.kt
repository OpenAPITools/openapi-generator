package org.openapitools.api

import org.openapitools.model.User
import io.swagger.annotations.*
import org.springframework.http.HttpStatus
import org.springframework.http.MediaType
import org.springframework.http.ResponseEntity
import org.springframework.stereotype.Controller
import org.springframework.web.bind.annotation.RequestBody
import org.springframework.web.bind.annotation.RequestPart
import org.springframework.web.bind.annotation.RequestParam
import org.springframework.web.bind.annotation.PathVariable
import org.springframework.web.bind.annotation.RequestHeader
import org.springframework.web.bind.annotation.RequestMethod
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.validation.annotation.Validated
import org.springframework.web.context.request.NativeWebRequest
import org.springframework.web.multipart.MultipartFile
import org.springframework.beans.factory.annotation.Autowired

import javax.validation.Valid
import javax.validation.constraints.*

import kotlin.collections.List
import kotlin.collections.Map

@Controller
@Validated
@Api(value = "User", description = "The User API")
@RequestMapping("\${api.base-path:/v2}")
class UserApiController(@Autowired(required = true) val service: UserApiService) {

    @ApiOperation(
            value = "Create user",
            nickname = "createUser",
            notes = "This can only be done by the logged in user.")
    @ApiResponses(
            value = [ApiResponse(code = 200, message = "successful operation")])
    @RequestMapping(
            value = ["/user"],
            consumes = ["application/json"],
            method = [RequestMethod.POST])
    fun createUser(@ApiParam(value = "Created user object" ,required=true ) @Valid @RequestBody user: User): ResponseEntity<Unit> {
        return ResponseEntity(service.createUser(user), HttpStatus.OK)
    }

    @ApiOperation(
            value = "Creates list of users with given input array",
            nickname = "createUsersWithArrayInput",
            notes = "")
    @ApiResponses(
            value = [ApiResponse(code = 200, message = "successful operation")])
    @RequestMapping(
            value = ["/user/createWithArray"],
            consumes = ["application/json"],
            method = [RequestMethod.POST])
    fun createUsersWithArrayInput(@ApiParam(value = "List of user object" ,required=true ) @Valid @RequestBody user: kotlin.Array<User>): ResponseEntity<Unit> {
        return ResponseEntity(service.createUsersWithArrayInput(user), HttpStatus.OK)
    }

    @ApiOperation(
            value = "Creates list of users with given input array",
            nickname = "createUsersWithListInput",
            notes = "")
    @ApiResponses(
            value = [ApiResponse(code = 200, message = "successful operation")])
    @RequestMapping(
            value = ["/user/createWithList"],
            consumes = ["application/json"],
            method = [RequestMethod.POST])
    fun createUsersWithListInput(@ApiParam(value = "List of user object" ,required=true ) @Valid @RequestBody user: kotlin.Array<User>): ResponseEntity<Unit> {
        return ResponseEntity(service.createUsersWithListInput(user), HttpStatus.OK)
    }

    @ApiOperation(
            value = "Delete user",
            nickname = "deleteUser",
            notes = "This can only be done by the logged in user.")
    @ApiResponses(
            value = [ApiResponse(code = 400, message = "Invalid username supplied"),ApiResponse(code = 404, message = "User not found")])
    @RequestMapping(
            value = ["/user/{username}"],
            method = [RequestMethod.DELETE])
    fun deleteUser(@ApiParam(value = "The name that needs to be deleted", required=true) @PathVariable("username") username: kotlin.String): ResponseEntity<Unit> {
        return ResponseEntity(service.deleteUser(username), HttpStatus.OK)
    }

    @ApiOperation(
            value = "Get user by user name",
            nickname = "getUserByName",
            notes = "",
            response = User::class)
    @ApiResponses(
            value = [ApiResponse(code = 200, message = "successful operation", response = User::class),ApiResponse(code = 400, message = "Invalid username supplied"),ApiResponse(code = 404, message = "User not found")])
    @RequestMapping(
            value = ["/user/{username}"],
            produces = ["application/xml", "application/json"], 
            method = [RequestMethod.GET])
    fun getUserByName(@ApiParam(value = "The name that needs to be fetched. Use user1 for testing.", required=true) @PathVariable("username") username: kotlin.String): ResponseEntity<User> {
        return ResponseEntity(service.getUserByName(username), HttpStatus.OK)
    }

    @ApiOperation(
            value = "Logs user into the system",
            nickname = "loginUser",
            notes = "",
            response = kotlin.String::class)
    @ApiResponses(
            value = [ApiResponse(code = 200, message = "successful operation", response = kotlin.String::class),ApiResponse(code = 400, message = "Invalid username/password supplied")])
    @RequestMapping(
            value = ["/user/login"],
            produces = ["application/xml", "application/json"], 
            method = [RequestMethod.GET])
    fun loginUser(@NotNull @ApiParam(value = "The user name for login", required = true) @Valid @RequestParam(value = "username", required = true) username: kotlin.String,@NotNull @ApiParam(value = "The password for login in clear text", required = true) @Valid @RequestParam(value = "password", required = true) password: kotlin.String): ResponseEntity<kotlin.String> {
        return ResponseEntity(service.loginUser(username, password), HttpStatus.OK)
    }

    @ApiOperation(
            value = "Logs out current logged in user session",
            nickname = "logoutUser",
            notes = "")
    @ApiResponses(
            value = [ApiResponse(code = 200, message = "successful operation")])
    @RequestMapping(
            value = ["/user/logout"],
            method = [RequestMethod.GET])
    fun logoutUser(): ResponseEntity<Unit> {
        return ResponseEntity(service.logoutUser(), HttpStatus.OK)
    }

    @ApiOperation(
            value = "Updated user",
            nickname = "updateUser",
            notes = "This can only be done by the logged in user.")
    @ApiResponses(
            value = [ApiResponse(code = 400, message = "Invalid user supplied"),ApiResponse(code = 404, message = "User not found")])
    @RequestMapping(
            value = ["/user/{username}"],
            consumes = ["application/json"],
            method = [RequestMethod.PUT])
    fun updateUser(@ApiParam(value = "name that need to be deleted", required=true) @PathVariable("username") username: kotlin.String,@ApiParam(value = "Updated user object" ,required=true ) @Valid @RequestBody user: User): ResponseEntity<Unit> {
        return ResponseEntity(service.updateUser(username, user), HttpStatus.OK)
    }
}
