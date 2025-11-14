package org.openapitools.api

import org.openapitools.model.User
import org.springframework.http.HttpStatus
import org.springframework.http.MediaType
import org.springframework.http.ResponseEntity
import org.springframework.web.context.request.NativeWebRequest
import kotlinx.coroutines.flow.Flow

import java.util.Optional

/**
 * A delegate to be called by the {@link UserApiController}}.
 * Implement this interface with a {@link org.springframework.stereotype.Service} annotated class.
 */
@javax.annotation.Generated(value = ["org.openapitools.codegen.languages.KotlinSpringServerCodegen"], comments = "Generator version: 7.18.0-SNAPSHOT")
interface UserApiDelegate {

    fun getRequest(): Optional<NativeWebRequest> = Optional.empty()

    /**
     * @see UserApi#createUser
     */
    suspend fun createUser(user: User,
        exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>


    /**
     * @see UserApi#createUsersWithArrayInput
     */
    suspend fun createUsersWithArrayInput(user: Flow<User>,
        exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>


    /**
     * @see UserApi#createUsersWithListInput
     */
    suspend fun createUsersWithListInput(user: Flow<User>,
        exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>


    /**
     * @see UserApi#deleteUser
     */
    suspend fun deleteUser(username: kotlin.String,
        exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>


    /**
     * @see UserApi#getUserByName
     */
    suspend fun getUserByName(username: kotlin.String,
        exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<User>


    /**
     * @see UserApi#loginUser
     */
    suspend fun loginUser(username: kotlin.String,
        password: kotlin.String,
        exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<kotlin.String>


    /**
     * @see UserApi#logoutUser
     */
    suspend fun logoutUser(exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>


    /**
     * @see UserApi#updateUser
     */
    suspend fun updateUser(username: kotlin.String,
        user: User,
        exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>

}
