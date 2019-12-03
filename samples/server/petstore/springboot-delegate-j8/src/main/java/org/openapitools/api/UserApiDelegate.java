package org.openapitools.api;

import java.util.List;
import org.openapitools.model.User;
import io.swagger.annotations.*;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.context.request.NativeWebRequest;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * A delegate to be called by the {@link UserApiController}}.
 * Implement this interface with a {@link org.springframework.stereotype.Service} annotated class.
 */

public interface UserApiDelegate {

    default Optional<NativeWebRequest> getRequest() {
        return Optional.empty();
    }

    /**
     * POST /user : Create user
     * This can only be done by the logged in user.
     *
     * @param body Created user object (required)
     * @return the response
     * @see UserApi#createUser
     */
    default ResponseEntity<Void> createUser(User body) {
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * POST /user/createWithArray : Creates list of users with given input array
     *
     * @param body List of user object (required)
     * @return the response
     * @see UserApi#createUsersWithArrayInput
     */
    default ResponseEntity<Void> createUsersWithArrayInput(List<User> body) {
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * POST /user/createWithList : Creates list of users with given input array
     *
     * @param body List of user object (required)
     * @return the response
     * @see UserApi#createUsersWithListInput
     */
    default ResponseEntity<Void> createUsersWithListInput(List<User> body) {
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * DELETE /user/{username} : Delete user
     * This can only be done by the logged in user.
     *
     * @param username The name that needs to be deleted (required)
     * @return the response
     * @see UserApi#deleteUser
     */
    default ResponseEntity<Void> deleteUser(String username) {
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * GET /user/{username} : Get user by user name
     *
     * @param username The name that needs to be fetched. Use user1 for testing. (required)
     * @return the response
     * @see UserApi#getUserByName
     */
    default ResponseEntity<User> getUserByName(String username) {
        getRequest().ifPresent(request -> {
            for (MediaType mediaType: MediaType.parseMediaTypes(request.getHeader("Accept"))) {
                if (mediaType.isCompatibleWith(MediaType.valueOf("application/json"))) {
                    String exampleString = "{ \"firstName\" : \"firstName\", \"lastName\" : \"lastName\", \"password\" : \"password\", \"userStatus\" : 6, \"phone\" : \"phone\", \"id\" : 0, \"email\" : \"email\", \"username\" : \"username\" }";
                    ApiUtil.setExampleResponse(request, "application/json", exampleString);
                    break;
                }
                if (mediaType.isCompatibleWith(MediaType.valueOf("application/xml"))) {
                    String exampleString = "<User> <id>123456789</id> <username>aeiou</username> <firstName>aeiou</firstName> <lastName>aeiou</lastName> <email>aeiou</email> <password>aeiou</password> <phone>aeiou</phone> <userStatus>123</userStatus> </User>";
                    ApiUtil.setExampleResponse(request, "application/xml", exampleString);
                    break;
                }
            }
        });
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * GET /user/login : Logs user into the system
     *
     * @param username The user name for login (required)
     * @param password The password for login in clear text (required)
     * @return the response
     * @see UserApi#loginUser
     */
    default ResponseEntity<String> loginUser(String username,
        String password) {
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * GET /user/logout : Logs out current logged in user session
     *
     * @return the response
     * @see UserApi#logoutUser
     */
    default ResponseEntity<Void> logoutUser() {
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * PUT /user/{username} : Updated user
     * This can only be done by the logged in user.
     *
     * @param username name that need to be deleted (required)
     * @param body Updated user object (required)
     * @return the response
     * @see UserApi#updateUser
     */
    default ResponseEntity<Void> updateUser(String username,
        User body) {
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

}
