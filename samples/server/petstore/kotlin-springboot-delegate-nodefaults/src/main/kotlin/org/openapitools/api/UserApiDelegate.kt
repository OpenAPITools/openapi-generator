package org.openapitools.api

import java.time.OffsetDateTime
import org.openapitools.model.User
import org.springframework.http.HttpStatus
import org.springframework.http.MediaType
import org.springframework.http.ResponseEntity
import org.springframework.web.context.request.NativeWebRequest
import org.springframework.core.io.Resource

import java.util.Optional

/**
 * A delegate to be called by the {@link UserApiController}}.
 * Implement this interface with a {@link org.springframework.stereotype.Service} annotated class.
 */
@jakarta.annotation.Generated(value = ["org.openapitools.codegen.languages.KotlinSpringServerCodegen"], comments = "Generator version: 7.10.0-SNAPSHOT")
interface UserApiDelegate {

    fun getRequest(): Optional<NativeWebRequest> = Optional.empty()

    /**
     * @see UserApi#createUser
     */
    fun createUser(user: User): ResponseEntity<Unit>


    /**
     * @see UserApi#createUsersWithArrayInput
     */
    fun createUsersWithArrayInput(user: List<User>): ResponseEntity<Unit>


    /**
     * @see UserApi#createUsersWithListInput
     */
    fun createUsersWithListInput(user: List<User>): ResponseEntity<Unit>


    /**
     * @see UserApi#deleteUser
     */
    fun deleteUser(username: String): ResponseEntity<Unit>


    /**
     * @see UserApi#getUserByName
     */
    fun getUserByName(username: String): ResponseEntity<User>


    /**
     * @see UserApi#loginUser
     */
    fun loginUser(username: String,
        password: String): ResponseEntity<String>


    /**
     * @see UserApi#logoutUser
     */
    fun logoutUser(): ResponseEntity<Unit>


    /**
     * @see UserApi#updateUser
     */
    fun updateUser(username: String,
        user: User): ResponseEntity<Unit>

}
