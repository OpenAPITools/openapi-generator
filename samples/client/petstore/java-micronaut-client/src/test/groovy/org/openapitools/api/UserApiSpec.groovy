package org.openapitools.api

import org.openapitools.model.User
import io.micronaut.test.extensions.spock.annotation.MicronautTest
import spock.lang.Specification
import jakarta.inject.Inject
import reactor.core.publisher.Mono
import java.util.ArrayList
import java.util.HashMap
import java.util.List
import java.util.Map


/**
 * API tests for UserApi
 */
@MicronautTest
class UserApiSpec extends Specification {

    @Inject
    UserApi api

    
    /**
     * Create user
     *
     * This can only be done by the logged in user.
     */
    void "createUser() test"() {
        given:
        User _body = null
        // api.createUser(_body).block()
        // Mono<Void> asyncResponse = api.createUser(_body)

        expect:
        true
        // TODO: test validations
    }

    
    /**
     * Creates list of users with given input array
     */
    void "createUsersWithArrayInput() test"() {
        given:
        List<User> _body = null
        // api.createUsersWithArrayInput(_body).block()
        // Mono<Void> asyncResponse = api.createUsersWithArrayInput(_body)

        expect:
        true
        // TODO: test validations
    }

    
    /**
     * Creates list of users with given input array
     */
    void "createUsersWithListInput() test"() {
        given:
        List<User> _body = null
        // api.createUsersWithListInput(_body).block()
        // Mono<Void> asyncResponse = api.createUsersWithListInput(_body)

        expect:
        true
        // TODO: test validations
    }

    
    /**
     * Delete user
     *
     * This can only be done by the logged in user.
     */
    void "deleteUser() test"() {
        given:
        String username = null
        // api.deleteUser(username).block()
        // Mono<Void> asyncResponse = api.deleteUser(username)

        expect:
        true
        // TODO: test validations
    }

    
    /**
     * Get user by user name
     */
    void "getUserByName() test"() {
        given:
        String username = null
        // User response = api.getUserByName(username).block()
        // Mono<User> asyncResponse = api.getUserByName(username)

        expect:
        true
        // TODO: test validations
    }

    
    /**
     * Logs user into the system
     */
    void "loginUser() test"() {
        given:
        String username = null
        String password = null
        // String response = api.loginUser(username, password).block()
        // Mono<String> asyncResponse = api.loginUser(username, password)

        expect:
        true
        // TODO: test validations
    }

    
    /**
     * Logs out current logged in user session
     */
    void "logoutUser() test"() {
        given:
        // api.logoutUser().block()
        // Mono<Void> asyncResponse = api.logoutUser()

        expect:
        true
        // TODO: test validations
    }

    
    /**
     * Updated user
     *
     * This can only be done by the logged in user.
     */
    void "updateUser() test"() {
        given:
        String username = null
        User _body = null
        // api.updateUser(username, _body).block()
        // Mono<Void> asyncResponse = api.updateUser(username, _body)

        expect:
        true
        // TODO: test validations
    }

    
}
