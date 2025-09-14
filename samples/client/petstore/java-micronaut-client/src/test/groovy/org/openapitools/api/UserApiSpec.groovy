package org.openapitools.api

import java.time.OffsetDateTime
import org.openapitools.model.User
import io.micronaut.test.extensions.spock.annotation.MicronautTest
import spock.lang.Specification
import jakarta.inject.Inject
import spock.lang.Ignore
import java.util.Arrays
import java.util.ArrayList
import java.util.HashMap
import java.util.List
import java.util.Map
import java.util.HashSet


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
    @Ignore("Not Implemented")
    void 'createUser() test'() {
        given:
        User _body = new User()

        when:
        api.createUser(_body).block()

        then:
        true
        // TODO: test validations
    }

    
    /**
     * Creates list of users with given input array
     */
    @Ignore("Not Implemented")
    void 'createUsersWithArrayInput() test'() {
        given:
        List<@Valid User> _body = []

        when:
        api.createUsersWithArrayInput(_body).block()

        then:
        true
        // TODO: test validations
    }

    
    /**
     * Creates list of users with given input array
     */
    @Ignore("Not Implemented")
    void 'createUsersWithListInput() test'() {
        given:
        List<@Valid User> _body = []

        when:
        api.createUsersWithListInput(_body).block()

        then:
        true
        // TODO: test validations
    }

    
    /**
     * Delete user
     *
     * This can only be done by the logged in user.
     */
    @Ignore("Not Implemented")
    void 'deleteUser() test'() {
        given:
        String username = 'example'

        when:
        api.deleteUser(username).block()

        then:
        true
        // TODO: test validations
    }

    
    /**
     * Get user by user name
     */
    @Ignore("Not Implemented")
    void 'getUserByName() test'() {
        given:
        String username = 'example'

        when:
        User body = api.getUserByName(username).block()

        then:
        true
        // TODO: test validations
    }

    
    /**
     * Logs user into the system
     */
    @Ignore("Not Implemented")
    void 'loginUser() test'() {
        given:
        String username = 'example'
        String password = 'example'

        when:
        String body = api.loginUser(username, password).block()

        then:
        true
        // TODO: test validations
    }

    
    /**
     * Logs out current logged in user session
     */
    @Ignore("Not Implemented")
    void 'logoutUser() test'() {
        given:

        when:
        api.logoutUser().block()

        then:
        true
        // TODO: test validations
    }

    
    /**
     * Updated user
     *
     * This can only be done by the logged in user.
     */
    @Ignore("Not Implemented")
    void 'updateUser() test'() {
        given:
        String username = 'example'
        User _body = new User()

        when:
        api.updateUser(username, _body).block()

        then:
        true
        // TODO: test validations
    }

    
}
