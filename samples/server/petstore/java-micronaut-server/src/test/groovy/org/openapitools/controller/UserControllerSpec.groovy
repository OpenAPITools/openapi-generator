package org.openapitools.controller

import java.time.LocalDateTime
import org.openapitools.model.User
import io.micronaut.test.extensions.spock.annotation.MicronautTest
import io.micronaut.http.client.HttpClient
import io.micronaut.http.client.annotation.Client
import io.micronaut.runtime.server.EmbeddedServer
import io.micronaut.http.HttpStatus
import io.micronaut.http.HttpRequest
import io.micronaut.http.MutableHttpRequest;
import io.micronaut.http.HttpResponse
import io.micronaut.http.MediaType
import io.micronaut.http.uri.UriTemplate
import io.micronaut.http.cookie.Cookie
import io.micronaut.http.client.multipart.MultipartBody
import io.micronaut.core.type.Argument
import jakarta.inject.Inject
import spock.lang.Specification
import spock.lang.Ignore
import reactor.core.publisher.Mono
import java.io.File
import java.io.FileReader


/**
 * Controller tests for UserController
 */
@MicronautTest
class UserControllerSpec extends Specification {

    @Inject
    EmbeddedServer server

    @Inject
    @Client('${context-path}')
    HttpClient client

    @Inject
    UserController controller

    /**
     * This test is used to validate the implementation of createUser() method
     *
     * The method should: Create user
     *
     * This can only be done by the logged in user.
     *
     * TODO fill in the parameters and test return value.
     */
    @Ignore("Not Implemented")
    def 'createUser() method test'() {
        given:
        User user = new User()

        when:
        controller.createUser(user).block()

        then:
        true
    }

    /**
     * This test is used to check that the api available to client through
     * '/user' to the features of createUser() works as desired.
     *
     * TODO fill in the request parameters and test response.
     */
    @Ignore("Not Implemented")
    def 'createUser() test with client through path /user'() {
        given:
        User body = new User()
        var uri = UriTemplate.of('/user').expand([:])
        MutableHttpRequest request = HttpRequest.POST(uri, body)
            .contentType('application/json')
            .accept('application/json')

        when:
        HttpResponse response = client.toBlocking().exchange(request); // To retrieve body you must specify required type (e.g. Map.class) as second argument 

        then:
        response.status() == HttpStatus.OK
    }

    /**
     * This test is used to validate the implementation of createUsersWithArrayInput() method
     *
     * The method should: Creates list of users with given input array
     *
     * TODO fill in the parameters and test return value.
     */
    @Ignore("Not Implemented")
    def 'createUsersWithArrayInput() method test'() {
        given:
        List<User> user = []

        when:
        controller.createUsersWithArrayInput(user).block()

        then:
        true
    }

    /**
     * This test is used to check that the api available to client through
     * '/user/createWithArray' to the features of createUsersWithArrayInput() works as desired.
     *
     * TODO fill in the request parameters and test response.
     */
    @Ignore("Not Implemented")
    def 'createUsersWithArrayInput() test with client through path /user/createWithArray'() {
        given:
        List<User> body = []
        var uri = UriTemplate.of('/user/createWithArray').expand([:])
        MutableHttpRequest request = HttpRequest.POST(uri, body)
            .contentType('application/json')
            .accept('application/json')

        when:
        HttpResponse response = client.toBlocking().exchange(request); // To retrieve body you must specify required type (e.g. Map.class) as second argument 

        then:
        response.status() == HttpStatus.OK
    }

    /**
     * This test is used to validate the implementation of createUsersWithListInput() method
     *
     * The method should: Creates list of users with given input array
     *
     * TODO fill in the parameters and test return value.
     */
    @Ignore("Not Implemented")
    def 'createUsersWithListInput() method test'() {
        given:
        List<User> user = []

        when:
        controller.createUsersWithListInput(user).block()

        then:
        true
    }

    /**
     * This test is used to check that the api available to client through
     * '/user/createWithList' to the features of createUsersWithListInput() works as desired.
     *
     * TODO fill in the request parameters and test response.
     */
    @Ignore("Not Implemented")
    def 'createUsersWithListInput() test with client through path /user/createWithList'() {
        given:
        List<User> body = []
        var uri = UriTemplate.of('/user/createWithList').expand([:])
        MutableHttpRequest request = HttpRequest.POST(uri, body)
            .contentType('application/json')
            .accept('application/json')

        when:
        HttpResponse response = client.toBlocking().exchange(request); // To retrieve body you must specify required type (e.g. Map.class) as second argument 

        then:
        response.status() == HttpStatus.OK
    }

    /**
     * This test is used to validate the implementation of deleteUser() method
     *
     * The method should: Delete user
     *
     * This can only be done by the logged in user.
     *
     * TODO fill in the parameters and test return value.
     */
    @Ignore("Not Implemented")
    def 'deleteUser() method test'() {
        given:
        String username = 'example'

        when:
        controller.deleteUser(username).block()

        then:
        true
    }

    /**
     * This test is used to check that the api available to client through
     * '/user/{username}' to the features of deleteUser() works as desired.
     *
     * TODO fill in the request parameters and test response.
     */
    @Ignore("Not Implemented")
    def 'deleteUser() test with client through path /user/{username}'() {
        given:
        var uri = UriTemplate.of('/user/{username}').expand([
            // Fill in the path variables
            'username': 'example'
        ])
        MutableHttpRequest request = HttpRequest.DELETE(uri)
            .accept('application/json')

        when:
        HttpResponse response = client.toBlocking().exchange(request); // To retrieve body you must specify required type (e.g. Map.class) as second argument 

        then:
        response.status() == HttpStatus.OK
    }

    /**
     * This test is used to validate the implementation of getUserByName() method
     *
     * The method should: Get user by user name
     *
     * TODO fill in the parameters and test return value.
     */
    @Ignore("Not Implemented")
    def 'getUserByName() method test'() {
        given:
        String username = 'example'

        when:
        User result = controller.getUserByName(username).block()

        then:
        true
    }

    /**
     * This test is used to check that the api available to client through
     * '/user/{username}' to the features of getUserByName() works as desired.
     *
     * TODO fill in the request parameters and test response.
     */
    @Ignore("Not Implemented")
    def 'getUserByName() test with client through path /user/{username}'() {
        given:
        var uri = UriTemplate.of('/user/{username}').expand([
            // Fill in the path variables
            'username': 'example'
        ])
        MutableHttpRequest request = HttpRequest.GET(uri)
            .accept('application/json')

        when:
        HttpResponse response = client.toBlocking().exchange(request, User.class);

        then:
        response.status() == HttpStatus.OK
    }

    /**
     * This test is used to validate the implementation of loginUser() method
     *
     * The method should: Logs user into the system
     *
     * TODO fill in the parameters and test return value.
     */
    @Ignore("Not Implemented")
    def 'loginUser() method test'() {
        given:
        String username = 'example'
        String password = 'example'

        when:
        String result = controller.loginUser(username, password).block()

        then:
        true
    }

    /**
     * This test is used to check that the api available to client through
     * '/user/login' to the features of loginUser() works as desired.
     *
     * TODO fill in the request parameters and test response.
     */
    @Ignore("Not Implemented")
    def 'loginUser() test with client through path /user/login'() {
        given:
        var uri = UriTemplate.of('/user/login').expand([:])
        MutableHttpRequest request = HttpRequest.GET(uri)
            .accept('application/json')
        request.getParameters()
            .add('username', 'example')
            .add('password', 'example')

        when:
        HttpResponse response = client.toBlocking().exchange(request, String.class);

        then:
        response.status() == HttpStatus.OK
    }

    /**
     * This test is used to validate the implementation of logoutUser() method
     *
     * The method should: Logs out current logged in user session
     *
     * TODO fill in the parameters and test return value.
     */
    @Ignore("Not Implemented")
    def 'logoutUser() method test'() {
        given:

        when:
        controller.logoutUser().block()

        then:
        true
    }

    /**
     * This test is used to check that the api available to client through
     * '/user/logout' to the features of logoutUser() works as desired.
     *
     * TODO fill in the request parameters and test response.
     */
    @Ignore("Not Implemented")
    def 'logoutUser() test with client through path /user/logout'() {
        given:
        var uri = UriTemplate.of('/user/logout').expand([:])
        MutableHttpRequest request = HttpRequest.GET(uri)
            .accept('application/json')

        when:
        HttpResponse response = client.toBlocking().exchange(request); // To retrieve body you must specify required type (e.g. Map.class) as second argument 

        then:
        response.status() == HttpStatus.OK
    }

    /**
     * This test is used to validate the implementation of updateUser() method
     *
     * The method should: Updated user
     *
     * This can only be done by the logged in user.
     *
     * TODO fill in the parameters and test return value.
     */
    @Ignore("Not Implemented")
    def 'updateUser() method test'() {
        given:
        String username = 'example'
        User user = new User()

        when:
        controller.updateUser(username, user).block()

        then:
        true
    }

    /**
     * This test is used to check that the api available to client through
     * '/user/{username}' to the features of updateUser() works as desired.
     *
     * TODO fill in the request parameters and test response.
     */
    @Ignore("Not Implemented")
    def 'updateUser() test with client through path /user/{username}'() {
        given:
        User body = new User()
        var uri = UriTemplate.of('/user/{username}').expand([
            // Fill in the path variables
            'username': 'example'
        ])
        MutableHttpRequest request = HttpRequest.PUT(uri, body)
            .contentType('application/json')
            .accept('application/json')

        when:
        HttpResponse response = client.toBlocking().exchange(request); // To retrieve body you must specify required type (e.g. Map.class) as second argument 

        then:
        response.status() == HttpStatus.OK
    }

}
