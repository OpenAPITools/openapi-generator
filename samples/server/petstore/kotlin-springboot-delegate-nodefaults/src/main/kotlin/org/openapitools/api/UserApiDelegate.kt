package org.openapitools.api

import org.openapitools.model.User
import org.springframework.http.HttpStatus
import org.springframework.http.MediaType
import org.springframework.http.ResponseEntity
import org.springframework.web.context.request.NativeWebRequest

import java.util.Optional

/**
 * A delegate to be called by the {@link UserApiController}}.
 * Implement this interface with a {@link org.springframework.stereotype.Service} annotated class.
 */
@jakarta.annotation.Generated(value = ["org.openapitools.codegen.languages.KotlinSpringServerCodegen"], comments = "Generator version: 7.14.0-SNAPSHOT")
interface UserApiDelegate {

    fun getRequest(): Optional<NativeWebRequest> = Optional.empty()

    /**
     * @see UserApi#createUser
     */
    fun createUser(user: User): ResponseEntity<Unit>


    /**
     * @see UserApi#createUsersWithArrayInput
     */
    fun createUsersWithArrayInput(user: kotlin.collections.List<User>): ResponseEntity<Unit>


    /**
     * @see UserApi#createUsersWithListInput
     */
    fun createUsersWithListInput(user: kotlin.collections.List<User>): ResponseEntity<Unit>


    /**
     * @see UserApi#deleteUser
     */
    fun deleteUser(username: kotlin.String): ResponseEntity<Unit>


    /**
     * @see UserApi#getUserByName
     */
    fun getUserByName(username: kotlin.String): ResponseEntity<User>


    /**
     * @see UserApi#loginUser
     */
    fun loginUser(username: kotlin.String,
        password: kotlin.String): ResponseEntity<kotlin.String>


    /**
     * @see UserApi#logoutUser
     */
    fun logoutUser(): ResponseEntity<Unit>


    /**
     * @see UserApi#updateUser
     */
    fun updateUser(username: kotlin.String,
        user: User): ResponseEntity<Unit>

}
