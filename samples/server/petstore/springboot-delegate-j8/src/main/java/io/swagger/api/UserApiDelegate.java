package io.swagger.api;

import java.util.List;
import io.swagger.model.User;

import io.swagger.annotations.*;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

/**
 * A delegate to be called by the {@link UserApiController}}.
 * Should be implemented as a controller but without the {@link org.springframework.stereotype.Controller} annotation.
 * Instead, use spring to autowire this class into the {@link UserApiController}.
 */

public interface UserApiDelegate {

    /**
     * @see UserApi#createUser
     */
    default ResponseEntity<Void> createUser(User body) {
    // do some magic!
    return new ResponseEntity<Void>(HttpStatus.OK);
    }

    /**
     * @see UserApi#createUsersWithArrayInput
     */
    default ResponseEntity<Void> createUsersWithArrayInput(List<User> body) {
    // do some magic!
    return new ResponseEntity<Void>(HttpStatus.OK);
    }

    /**
     * @see UserApi#createUsersWithListInput
     */
    default ResponseEntity<Void> createUsersWithListInput(List<User> body) {
    // do some magic!
    return new ResponseEntity<Void>(HttpStatus.OK);
    }

    /**
     * @see UserApi#deleteUser
     */
    default ResponseEntity<Void> deleteUser(String username) {
    // do some magic!
    return new ResponseEntity<Void>(HttpStatus.OK);
    }

    /**
     * @see UserApi#getUserByName
     */
    default ResponseEntity<User> getUserByName(String username) {
    // do some magic!
    return new ResponseEntity<User>(HttpStatus.OK);
    }

    /**
     * @see UserApi#loginUser
     */
    default ResponseEntity<String> loginUser(String username,
        String password) {
    // do some magic!
    return new ResponseEntity<String>(HttpStatus.OK);
    }

    /**
     * @see UserApi#logoutUser
     */
    default ResponseEntity<Void> logoutUser() {
    // do some magic!
    return new ResponseEntity<Void>(HttpStatus.OK);
    }

    /**
     * @see UserApi#updateUser
     */
    default ResponseEntity<Void> updateUser(String username,
        User body) {
    // do some magic!
    return new ResponseEntity<Void>(HttpStatus.OK);
    }

}
