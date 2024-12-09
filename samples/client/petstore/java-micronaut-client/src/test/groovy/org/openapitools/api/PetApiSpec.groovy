package org.openapitools.api

import java.io.File
import org.openapitools.model.ModelApiResponse
import org.openapitools.model.Pet
import java.util.Set
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
 * API tests for PetApi
 */
@MicronautTest
class PetApiSpec extends Specification {

    @Inject
    PetApi api

    
    /**
     * Add a new pet to the store
     */
    @Ignore("Not Implemented")
    void 'addPet() test'() {
        given:
        Pet _body = new Pet()

        when:
        api.addPet(_body).block()

        then:
        true
        // TODO: test validations
    }

    
    /**
     * Deletes a pet
     */
    @Ignore("Not Implemented")
    void 'deletePet() test'() {
        given:
        Long petId = 56L
        String apiKey = 'example'

        when:
        api.deletePet(petId, apiKey).block()

        then:
        true
        // TODO: test validations
    }

    
    /**
     * Finds Pets by status
     *
     * Multiple status values can be provided with comma separated strings
     */
    @Ignore("Not Implemented")
    void 'findPetsByStatus() test'() {
        given:
        List<String> status = ['available']

        when:
        List<Pet> body = api.findPetsByStatus(status).block()

        then:
        true
        // TODO: test validations
    }

    
    /**
     * Finds Pets by tags
     *
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     */
    @Ignore("Not Implemented")
    void 'findPetsByTags() test'() {
        given:
        Set<String> tags = [].asSet()

        when:
        Set<Pet> body = api.findPetsByTags(tags).block()

        then:
        true
        // TODO: test validations
    }

    
    /**
     * Find pet by ID
     *
     * Returns a single pet
     */
    @Ignore("Not Implemented")
    void 'getPetById() test'() {
        given:
        Long petId = 56L

        when:
        Pet body = api.getPetById(petId).block()

        then:
        true
        // TODO: test validations
    }

    
    /**
     * Update an existing pet
     */
    @Ignore("Not Implemented")
    void 'updatePet() test'() {
        given:
        Pet _body = new Pet()

        when:
        api.updatePet(_body).block()

        then:
        true
        // TODO: test validations
    }

    
    /**
     * Updates a pet in the store with form data
     */
    @Ignore("Not Implemented")
    void 'updatePetWithForm() test'() {
        given:
        Long petId = 56L
        String name = 'example'
        String status = 'example'

        when:
        api.updatePetWithForm(petId, name, status).block()

        then:
        true
        // TODO: test validations
    }

    
    /**
     * uploads an image
     */
    @Ignore("Not Implemented")
    void 'uploadFile() test'() {
        given:
        Long petId = 56L
        String additionalMetadata = 'example'
        File _file = null

        when:
        ModelApiResponse body = api.uploadFile(petId, additionalMetadata, _file).block()

        then:
        true
        // TODO: test validations
    }

    
    /**
     * uploads an image (required)
     */
    @Ignore("Not Implemented")
    void 'uploadFileWithRequiredFile() test'() {
        given:
        Long petId = 56L
        File requiredFile = null
        String additionalMetadata = 'example'

        when:
        ModelApiResponse body = api.uploadFileWithRequiredFile(petId, requiredFile, additionalMetadata).block()

        then:
        true
        // TODO: test validations
    }

    
}
