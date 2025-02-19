package org.openapitools.controller

import io.micronaut.http.multipart.CompletedFileUpload
import org.openapitools.model.ModelApiResponse
import org.openapitools.model.Pet
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
 * Controller tests for PetController
 */
@MicronautTest
class PetControllerSpec extends Specification {

    @Inject
    EmbeddedServer server

    @Inject
    @Client('${context-path}')
    HttpClient client

    @Inject
    PetController controller

    /**
     * This test is used to validate the implementation of addPet() method
     *
     * The method should: Add a new pet to the store
     *
     * TODO fill in the parameters and test return value.
     */
    @Ignore("Not Implemented")
    def 'addPet() method test'() {
        given:
        Pet pet = new Pet('doggie', ['example'])

        when:
        Pet result = controller.addPet(pet).block()

        then:
        true
    }

    /**
     * This test is used to check that the api available to client through
     * '/pet' to the features of addPet() works as desired.
     *
     * TODO fill in the request parameters and test response.
     */
    @Ignore("Not Implemented")
    def 'addPet() test with client through path /pet'() {
        given:
        Pet body = new Pet('doggie', ['example'])
        var uri = UriTemplate.of('/pet').expand([:])
        MutableHttpRequest request = HttpRequest.POST(uri, body)
            .contentType('application/json')
            .accept('application/json')

        when:
        HttpResponse response = client.toBlocking().exchange(request, Pet.class);

        then:
        response.status() == HttpStatus.OK
    }

    /**
     * This test is used to validate the implementation of deletePet() method
     *
     * The method should: Deletes a pet
     *
     * TODO fill in the parameters and test return value.
     */
    @Ignore("Not Implemented")
    def 'deletePet() method test'() {
        given:
        Long petId = 56L
        String apiKey = 'example'

        when:
        controller.deletePet(petId, apiKey).block()

        then:
        true
    }

    /**
     * This test is used to check that the api available to client through
     * '/pet/{petId}' to the features of deletePet() works as desired.
     *
     * TODO fill in the request parameters and test response.
     */
    @Ignore("Not Implemented")
    def 'deletePet() test with client through path /pet/{petId}'() {
        given:
        var uri = UriTemplate.of('/pet/{petId}').expand([
            // Fill in the path variables
            'petId': 56L
        ])
        MutableHttpRequest request = HttpRequest.DELETE(uri)
            .accept('application/json')
            .header('api_key', 'example')

        when:
        HttpResponse response = client.toBlocking().exchange(request); // To retrieve body you must specify required type (e.g. Map.class) as second argument 

        then:
        response.status() == HttpStatus.OK
    }

    /**
     * This test is used to validate the implementation of findPetsByStatus() method
     *
     * The method should: Finds Pets by status
     *
     * Multiple status values can be provided with comma separated strings
     *
     * TODO fill in the parameters and test return value.
     */
    @Ignore("Not Implemented")
    def 'findPetsByStatus() method test'() {
        given:
        List<String> status = ['available']

        when:
        List<Pet> result = controller.findPetsByStatus(status).block()

        then:
        true
    }

    /**
     * This test is used to check that the api available to client through
     * '/pet/findByStatus' to the features of findPetsByStatus() works as desired.
     *
     * TODO fill in the request parameters and test response.
     */
    @Ignore("Not Implemented")
    def 'findPetsByStatus() test with client through path /pet/findByStatus'() {
        given:
        var uri = UriTemplate.of('/pet/findByStatus').expand([:])
        MutableHttpRequest request = HttpRequest.GET(uri)
            .accept('application/json')
        request.getParameters()
            .add('status', ['available'].toString()) // The query parameter format should be csv

        when:
        HttpResponse response = client.toBlocking().exchange(request, Argument.of(List.class, Pet.class));

        then:
        response.status() == HttpStatus.OK
    }

    /**
     * This test is used to validate the implementation of findPetsByTags() method
     *
     * The method should: Finds Pets by tags
     *
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     *
     * TODO fill in the parameters and test return value.
     */
    @Ignore("Not Implemented")
    def 'findPetsByTags() method test'() {
        given:
        List<String> tags = ['example']

        when:
        List<Pet> result = controller.findPetsByTags(tags).block()

        then:
        true
    }

    /**
     * This test is used to check that the api available to client through
     * '/pet/findByTags' to the features of findPetsByTags() works as desired.
     *
     * TODO fill in the request parameters and test response.
     */
    @Ignore("Not Implemented")
    def 'findPetsByTags() test with client through path /pet/findByTags'() {
        given:
        var uri = UriTemplate.of('/pet/findByTags').expand([:])
        MutableHttpRequest request = HttpRequest.GET(uri)
            .accept('application/json')
        request.getParameters()
            .add('tags', ['example'].toString()) // The query parameter format should be csv

        when:
        HttpResponse response = client.toBlocking().exchange(request, Argument.of(List.class, Pet.class));

        then:
        response.status() == HttpStatus.OK
    }

    /**
     * This test is used to validate the implementation of getPetById() method
     *
     * The method should: Find pet by ID
     *
     * Returns a single pet
     *
     * TODO fill in the parameters and test return value.
     */
    @Ignore("Not Implemented")
    def 'getPetById() method test'() {
        given:
        Long petId = 56L

        when:
        Pet result = controller.getPetById(petId).block()

        then:
        true
    }

    /**
     * This test is used to check that the api available to client through
     * '/pet/{petId}' to the features of getPetById() works as desired.
     *
     * TODO fill in the request parameters and test response.
     */
    @Ignore("Not Implemented")
    def 'getPetById() test with client through path /pet/{petId}'() {
        given:
        var uri = UriTemplate.of('/pet/{petId}').expand([
            // Fill in the path variables
            'petId': 56L
        ])
        MutableHttpRequest request = HttpRequest.GET(uri)
            .accept('application/json')

        when:
        HttpResponse response = client.toBlocking().exchange(request, Pet.class);

        then:
        response.status() == HttpStatus.OK
    }

    /**
     * This test is used to validate the implementation of updatePet() method
     *
     * The method should: Update an existing pet
     *
     * TODO fill in the parameters and test return value.
     */
    @Ignore("Not Implemented")
    def 'updatePet() method test'() {
        given:
        Pet pet = new Pet('doggie', ['example'])

        when:
        Pet result = controller.updatePet(pet).block()

        then:
        true
    }

    /**
     * This test is used to check that the api available to client through
     * '/pet' to the features of updatePet() works as desired.
     *
     * TODO fill in the request parameters and test response.
     */
    @Ignore("Not Implemented")
    def 'updatePet() test with client through path /pet'() {
        given:
        Pet body = new Pet('doggie', ['example'])
        var uri = UriTemplate.of('/pet').expand([:])
        MutableHttpRequest request = HttpRequest.PUT(uri, body)
            .contentType('application/json')
            .accept('application/json')

        when:
        HttpResponse response = client.toBlocking().exchange(request, Pet.class);

        then:
        response.status() == HttpStatus.OK
    }

    /**
     * This test is used to validate the implementation of updatePetWithForm() method
     *
     * The method should: Updates a pet in the store with form data
     *
     * TODO fill in the parameters and test return value.
     */
    @Ignore("Not Implemented")
    def 'updatePetWithForm() method test'() {
        given:
        Long petId = 56L
        String name = 'example'
        String status = 'example'

        when:
        controller.updatePetWithForm(petId, name, status).block()

        then:
        true
    }

    /**
     * This test is used to check that the api available to client through
     * '/pet/{petId}' to the features of updatePetWithForm() works as desired.
     *
     * TODO fill in the request parameters and test response.
     */
    @Ignore("Not Implemented")
    def 'updatePetWithForm() test with client through path /pet/{petId}'() {
        given:
        var form = [
            // Fill in the body form parameters
            'name': 'example',
            'status': 'example'
        ]
        var uri = UriTemplate.of('/pet/{petId}').expand([
            // Fill in the path variables
            'petId': 56L
        ])
        MutableHttpRequest request = HttpRequest.POST(uri, form)
            .contentType('application/x-www-form-urlencoded')
            .accept('application/json')

        when:
        HttpResponse response = client.toBlocking().exchange(request); // To retrieve body you must specify required type (e.g. Map.class) as second argument 

        then:
        response.status() == HttpStatus.OK
    }

    /**
     * This test is used to validate the implementation of uploadFile() method
     *
     * The method should: uploads an image
     *
     * TODO fill in the parameters and test return value.
     */
    @Ignore("Not Implemented")
    def 'uploadFile() method test'() {
        given:
        Long petId = 56L
        String additionalMetadata = 'example'
        CompletedFileUpload file = null

        when:
        ModelApiResponse result = controller.uploadFile(petId, additionalMetadata, file).block()

        then:
        true
    }

    /**
     * This test is used to check that the api available to client through
     * '/pet/{petId}/uploadImage' to the features of uploadFile() works as desired.
     *
     * TODO fill in the request parameters and test response.
     */
    @Ignore("Not Implemented")
    def 'uploadFile() test with client through path /pet/{petId}/uploadImage'() {
        given:
        var body = MultipartBody.builder() // Create multipart body
            .addPart('additionalMetadata', 'example')
            .addPart('file', 'filename', File.createTempFile('test', '.tmp'))
            .build()
        var uri = UriTemplate.of('/pet/{petId}/uploadImage').expand([
            // Fill in the path variables
            'petId': 56L
        ])
        MutableHttpRequest request = HttpRequest.POST(uri, body)
            .contentType('multipart/form-data')
            .accept('application/json')

        when:
        HttpResponse response = client.toBlocking().exchange(request, ModelApiResponse.class);

        then:
        response.status() == HttpStatus.OK
    }

}
