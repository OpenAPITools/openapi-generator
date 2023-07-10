package org.openapitools.api

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
@javax.annotation.Generated(value = ["org.openapitools.codegen.languages.KotlinSpringServerCodegen"])
interface UserApiDelegate {

    fun getRequest(): Optional<NativeWebRequest> = Optional.empty()

    /**
     * @see UserApi#createUser
     */
    fun createUser(user: User): ResponseEntity<Unit> {
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)

    }


    /**
     * @see UserApi#createUsersWithArrayInput
     */
    fun createUsersWithArrayInput(user: kotlin.collections.List<User>): ResponseEntity<Unit> {
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)

    }


    /**
     * @see UserApi#createUsersWithListInput
     */
    fun createUsersWithListInput(user: kotlin.collections.List<User>): ResponseEntity<Unit> {
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)

    }


    /**
     * @see UserApi#deleteUser
     */
    fun deleteUser(username: kotlin.String): ResponseEntity<Unit> {
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)

    }


    /**
     * @see UserApi#getUserByName
     */
    fun getUserByName(username: kotlin.String): ResponseEntity<User> {
        getRequest().ifPresent { request ->
            for (mediaType in MediaType.parseMediaTypes(request.getHeader("Accept"))) {
                if (mediaType.isCompatibleWith(MediaType.valueOf("application/json"))) {
                    ApiUtil.setExampleResponse(request, "application/json", "{  \"firstName\" : \"firstName\",  \"lastName\" : \"lastName\",  \"password\" : \"password\",  \"userStatus\" : 6,  \"phone\" : \"phone\",  \"id\" : 0,  \"email\" : \"email\",  \"username\" : \"username\"}")
                    break
                }
                if (mediaType.isCompatibleWith(MediaType.valueOf("application/xml"))) {
                    ApiUtil.setExampleResponse(request, "application/xml", "<User>  <id>123456789</id>  <username>aeiou</username>  <firstName>aeiou</firstName>  <lastName>aeiou</lastName>  <email>aeiou</email>  <password>aeiou</password>  <phone>aeiou</phone>  <userStatus>123</userStatus></User>")
                    break
                }
            }
        }
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)

    }


    /**
     * @see UserApi#loginUser
     */
    fun loginUser(username: kotlin.String,
        password: kotlin.String): ResponseEntity<kotlin.String> {
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)

    }


    /**
     * @see UserApi#logoutUser
     */
    fun logoutUser(): ResponseEntity<Unit> {
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)

    }


    /**
     * @see UserApi#updateUser
     */
    fun updateUser(username: kotlin.String,
        user: User): ResponseEntity<Unit> {
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)

    }

}
