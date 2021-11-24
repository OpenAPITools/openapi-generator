package org.openapitools.api

import org.openapitools.model.User
import io.swagger.annotations.Api
import io.swagger.annotations.ApiOperation
import io.swagger.annotations.ApiParam
import io.swagger.annotations.ApiResponse
import io.swagger.annotations.ApiResponses
import io.swagger.annotations.Authorization
import io.swagger.annotations.AuthorizationScope
import org.springframework.http.HttpStatus
import org.springframework.http.MediaType
import org.springframework.http.ResponseEntity

import org.springframework.web.bind.annotation.*
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
@Api(value = "user", description = "The user API")
@RequestMapping("\${api.base-path:/v2}")
class UserApiController(@Autowired(required = true) val service: UserApiService) {

    @ApiOperation(
        value = "Create user",
        nickname = "createUser",
        notes = "This can only be done by the logged in user.",
        authorizations = [Authorization(value = "api_key")])
    @ApiResponses(
        value = [ApiResponse(code = 200, message = "successful operation")])
    @PostMapping(
        value = ["/user"],
        consumes = ["application/json"]
    )
    fun createUser(@ApiParam(value = "Created user object" ,required=true ) @Valid @RequestBody user: User
): ResponseEntity<Unit> {
        return ResponseEntity(service.createUser(user), HttpStatus.valueOf(200))
    }

    @ApiOperation(
        value = "Creates list of users with given input array",
        nickname = "createUsersWithArrayInput",
        notes = "",
        authorizations = [Authorization(value = "api_key")])
    @ApiResponses(
        value = [ApiResponse(code = 200, message = "successful operation")])
    @PostMapping(
        value = ["/user/createWithArray"],
        consumes = ["application/json"]
    )
    fun createUsersWithArrayInput(@ApiParam(value = "List of user object" ,required=true ) @Valid @RequestBody user: kotlin.collections.List<User>
): ResponseEntity<Unit> {
        return ResponseEntity(service.createUsersWithArrayInput(user), HttpStatus.valueOf(200))
    }

    @ApiOperation(
        value = "Creates list of users with given input array",
        nickname = "createUsersWithListInput",
        notes = "",
        authorizations = [Authorization(value = "api_key")])
    @ApiResponses(
        value = [ApiResponse(code = 200, message = "successful operation")])
    @PostMapping(
        value = ["/user/createWithList"],
        consumes = ["application/json"]
    )
    fun createUsersWithListInput(@ApiParam(value = "List of user object" ,required=true ) @Valid @RequestBody user: kotlin.collections.List<User>
): ResponseEntity<Unit> {
        return ResponseEntity(service.createUsersWithListInput(user), HttpStatus.valueOf(200))
    }

    @ApiOperation(
        value = "Delete user",
        nickname = "deleteUser",
        notes = "This can only be done by the logged in user.",
        authorizations = [Authorization(value = "api_key")])
    @ApiResponses(
        value = [ApiResponse(code = 400, message = "Invalid username supplied"),ApiResponse(code = 404, message = "User not found")])
    @DeleteMapping(
        value = ["/user/{username}"]
    )
    fun deleteUser(@ApiParam(value = "The name that needs to be deleted", required=true) @PathVariable("username") username: kotlin.String
): ResponseEntity<Unit> {
        return ResponseEntity(service.deleteUser(username), HttpStatus.valueOf(400))
    }

    @ApiOperation(
        value = "Get user by user name",
        nickname = "getUserByName",
        notes = "",
        response = User::class)
    @ApiResponses(
        value = [ApiResponse(code = 200, message = "successful operation", response = User::class),ApiResponse(code = 400, message = "Invalid username supplied"),ApiResponse(code = 404, message = "User not found")])
    @GetMapping(
        value = ["/user/{username}"],
        produces = ["application/xml", "application/json"]
    )
    fun getUserByName(@ApiParam(value = "The name that needs to be fetched. Use user1 for testing.", required=true) @PathVariable("username") username: kotlin.String
): ResponseEntity<User> {
        return ResponseEntity(service.getUserByName(username), HttpStatus.valueOf(200))
    }

    @ApiOperation(
        value = "Logs user into the system",
        nickname = "loginUser",
        notes = "",
        response = kotlin.String::class)
    @ApiResponses(
        value = [ApiResponse(code = 200, message = "successful operation", response = kotlin.String::class),ApiResponse(code = 400, message = "Invalid username/password supplied")])
    @GetMapping(
        value = ["/user/login"],
        produces = ["application/xml", "application/json"]
    )
    fun loginUser(@NotNull @Pattern(regexp="^[a-zA-Z0-9]+[a-zA-Z0-9\\.\\-_]*[a-zA-Z0-9]+$") @ApiParam(value = "The user name for login", required = true) @Valid @RequestParam(value = "username", required = true) username: kotlin.String
,@NotNull @ApiParam(value = "The password for login in clear text", required = true) @Valid @RequestParam(value = "password", required = true) password: kotlin.String
): ResponseEntity<kotlin.String> {
        return ResponseEntity(service.loginUser(username, password), HttpStatus.valueOf(200))
    }

    @ApiOperation(
        value = "Logs out current logged in user session",
        nickname = "logoutUser",
        notes = "",
        authorizations = [Authorization(value = "api_key")])
    @ApiResponses(
        value = [ApiResponse(code = 200, message = "successful operation")])
    @GetMapping(
        value = ["/user/logout"]
    )
    fun logoutUser(): ResponseEntity<Unit> {
        return ResponseEntity(service.logoutUser(), HttpStatus.valueOf(200))
    }

    @ApiOperation(
        value = "Updated user",
        nickname = "updateUser",
        notes = "This can only be done by the logged in user.",
        authorizations = [Authorization(value = "api_key")])
    @ApiResponses(
        value = [ApiResponse(code = 400, message = "Invalid user supplied"),ApiResponse(code = 404, message = "User not found")])
    @PutMapping(
        value = ["/user/{username}"],
        consumes = ["application/json"]
    )
    fun updateUser(@ApiParam(value = "name that need to be deleted", required=true) @PathVariable("username") username: kotlin.String
,@ApiParam(value = "Updated user object" ,required=true ) @Valid @RequestBody user: User
): ResponseEntity<Unit> {
        return ResponseEntity(service.updateUser(username, user), HttpStatus.valueOf(400))
    }
}
