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
@javax.annotation.Generated(value = ["org.openapitools.codegen.languages.KotlinSpringServerCodegen"], comments = "Generator version: 7.22.0-SNAPSHOT")
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
                    ApiUtil.setExampleResponse(request, "application/json", "{\n  \"firstName\" : \"firstName\",\n  \"lastName\" : \"lastName\",\n  \"password\" : \"password\",\n  \"userStatus\" : 6,\n  \"phone\" : \"phone\",\n  \"id\" : 0,\n  \"email\" : \"email\",\n  \"username\" : \"username\"\n}")
                    break
                }
                if (mediaType.isCompatibleWith(MediaType.valueOf("application/xml"))) {
                    ApiUtil.setExampleResponse(request, "application/xml", "<User>\n  <id>123456789</id>\n  <username>aeiou</username>\n  <firstName>aeiou</firstName>\n  <lastName>aeiou</lastName>\n  <email>aeiou</email>\n  <password>aeiou</password>\n  <phone>aeiou</phone>\n  <userStatus>123</userStatus>\n</User>")
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
