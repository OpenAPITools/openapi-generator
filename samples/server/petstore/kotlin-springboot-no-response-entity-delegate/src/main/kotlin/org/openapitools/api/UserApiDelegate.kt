package org.openapitools.api

import org.openapitools.model.User
import org.springframework.http.HttpStatus
import org.springframework.http.MediaType
import org.springframework.web.context.request.NativeWebRequest

import java.util.Optional

/**
 * A delegate to be called by the {@link UserApiController}}.
 * Implement this interface with a {@link org.springframework.stereotype.Service} annotated class.
 */
@javax.annotation.Generated(value = ["org.openapitools.codegen.languages.KotlinSpringServerCodegen"], comments = "Generator version: 7.19.0-SNAPSHOT")
interface UserApiDelegate {

    fun getRequest(): Optional<NativeWebRequest> = Optional.empty()

    /**
     * @see UserApi#createUser
     */
    fun createUser(body: User): Unit {
        return TODO("Not yet implemented")

    }


    /**
     * @see UserApi#createUsersWithArrayInput
     */
    fun createUsersWithArrayInput(body: kotlin.collections.List<User>): Unit {
        return TODO("Not yet implemented")

    }


    /**
     * @see UserApi#createUsersWithListInput
     */
    fun createUsersWithListInput(body: kotlin.collections.List<User>): Unit {
        return TODO("Not yet implemented")

    }


    /**
     * @see UserApi#deleteUser
     */
    fun deleteUser(username: kotlin.String): Unit {
        return TODO("Not yet implemented")

    }


    /**
     * @see UserApi#getUserByName
     */
    fun getUserByName(username: kotlin.String): User {
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
        return TODO("Not yet implemented")

    }


    /**
     * @see UserApi#loginUser
     */
    fun loginUser(username: kotlin.String,
        password: kotlin.String): kotlin.String {
        return TODO("Not yet implemented")

    }


    /**
     * @see UserApi#logoutUser
     */
    fun logoutUser(): Unit {
        return TODO("Not yet implemented")

    }


    /**
     * @see UserApi#updateUser
     */
    fun updateUser(username: kotlin.String,
        body: User): Unit {
        return TODO("Not yet implemented")

    }

}
