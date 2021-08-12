package org.openapitools.api

import io.micronaut.http.HttpStatus
import io.micronaut.http.client.exceptions.HttpClientResponseException
import org.openapitools.model.Category
import org.openapitools.model.Tag


import org.openapitools.model.Pet
import io.micronaut.test.extensions.spock.annotation.MicronautTest
import spock.lang.Specification
import jakarta.inject.Inject


/**
 * API tests for PetApi
 */
@MicronautTest
class PetApiSpec extends Specification {

    @Inject
    PetApi api

    /**
     * Find pet by ID
     * Returns a pet when ID &lt; 10.
     */
    void "getPetById() test"() {
        given:
        Long petId = 5L
        Pet response = api.getPetById(petId).block()

        expect:
        response != null
        response.getId() == petId
        response.getName() != null
    }

    /**
     * ID &gt; 10 or nonintegers will simulate API error conditions
     */
    void "getPetById() error test"() {
        given:
        Long petId = -11L

        when:
        api.getPetById(petId).block()

        then:
        var e = thrown(HttpClientResponseException.class)
        e.getMessage() == "Pet not found"
        e.getStatus() == HttpStatus.NOT_FOUND
    }

    void "add and get pet test"() {
        given:
        Pet pet = new Pet()
                .id(1002L)
                .name("Bob")
                .status(Pet.StatusEnum.AVAILABLE)
                .category(new Category().id(0L).name("string"))
                .photoUrls(["string"].toSet())
                .tags([new Tag().id(0L).name("string")])

        api.addPet(pet).block()
        Pet received = api.getPetById(pet.getId()).block()

        expect:
        received != null
        pet == received
    }

    /**
     * Finds Pets by status
     * Multiple status values can be provided with comma separated strings
     */
    void "findPetsByStatus() test"() {
        given:
        Pet pet = new Pet()
                .id(1005L)
                .name("Tom")
                .status(Pet.StatusEnum.AVAILABLE)
                .category(new Category().id(105L).name("Cat category"))
                .photoUrls([].toSet())
                .tags([new Tag().id(1L).name("meowing"), new Tag().id(2L).name("purring")])

        Pet pet2 = new Pet().id(1007L).name("Bob").status(Pet.StatusEnum.AVAILABLE)
        Pet pet3 = new Pet().id(1008L).name("Unnamed").status(Pet.StatusEnum.SOLD)
                .category(new Category().id(6L).name("Young dog category"))
                .tags(new ArrayList<>()).photoUrls([].toSet())

        when:
        api.addPet(pet).block()
        api.addPet(pet2).block()
        api.addPet(pet3).block()

        List<Pet> available = api.findPetsByStatus(["available"]).block()
        List<Pet> sold = api.findPetsByStatus(["sold"]).block()
        // The description says that it needs to be comma separated, but the actual format is multi
        List<Pet> both = api.findPetsByStatus(["available,sold"]).block()

        then:
        // Available pets
        available != null
        and: "The first pet should be present and match the added pet"
        Pet receivedPet = available.stream().filter(p -> p.getId() == pet.getId()).findFirst().orElse(null)
        receivedPet != null
        pet == receivedPet
        and: "The second added pet should be present in the available pets"
        available.stream().anyMatch(p -> p.getId() == pet2.getId() && p.getName() == pet2.getName())
        and: "The third pet was sold and should not be present in available pets"
        available.stream().allMatch(p -> p.getId() != pet3.getId())

        // Sold pets
        and: "The third added pet should be present in the sold pets"
        sold.stream().anyMatch(p -> p.getId() == pet3.getId() && p.getName() == pet3.getName())
        and: "The first pet was sold and should not be present in available pets"
        sold.stream().allMatch(p -> p.getId() != pet.getId())
        and: "The second pet was sold and should not be present in available pets"
        sold.stream().allMatch(p -> p.getId() != pet2.getId())

        // Both pets
        // The formatting of query needs to be correct for this one
        and: "The third pet should be present in all the pets and match the added pet"
        Pet receivedPet3 = both.stream().filter(p -> p.getId() == pet3.getId()).findFirst().orElse(null)
        receivedPet3 != null
        pet3 == receivedPet3
        and: "The second added pet should be present in the all pets"
        both.stream().anyMatch(p -> p.getId() == pet2.getId() && p.getName() == pet2.getName())
        and: "The first added pet should be present in the all pets"
        both.stream().anyMatch(p -> p.getId() == pet.getId() && p.getName() == pet.getName())
    }

    void "delete non-existing pet error test"() {
        when:
        api.deletePet(-1L, "special-key").block()

        then:
        var e = thrown(HttpClientResponseException.class)
        e.getMessage() == "Not Found"
    }

    void "add and delete pet test"() {
        given:
        List<String> status = ["available"]

        List<Pet> initialPets = api.findPetsByStatus(status).block()

        when:
        Pet pet1 = new Pet().id(101L).name("Bob").status(Pet.StatusEnum.AVAILABLE)
        Pet pet2 = new Pet().id(103L).name("Rat").status(Pet.StatusEnum.AVAILABLE)
        api.addPet(pet1).block()
        api.addPet(pet2).block()

        List<Pet> pets = api.findPetsByStatus(status).block()

        then: "The 2 pets should have been added correctly"
        pets != null
        pets.size() - initialPets.size() == 2
        pets.stream().anyMatch(p -> p.getId() == pet1.getId() && p.getName() == pet1.getName())
        pets.stream().anyMatch(p -> p.getId() == pet2.getId() && p.getName() == pet2.getName())

        when:
        api.deletePet(pet1.getId(), "special-key").block()
        api.deletePet(pet2.getId(), "special-key").block()

        List<Pet> finalPets = api.findPetsByStatus(status).block()

        then: "The 2 pets should have been deleted"
        finalPets != null
        pets.size() - finalPets.size() == 2
        finalPets.stream().allMatch(p -> p.getId() != pet1.getId())
        finalPets.stream().allMatch(p -> p.getId() != pet2.getId())
    }

    /**
     * Update an existing pet
     */
    void "updatePet() test"() {
        given:
        Pet pet = new Pet().id(1L).name("Jazz")

        when:
        api.updatePet(pet).block()

        then: "The pet should be available and match the updates"
        Pet received = api.getPetById(1L).block()
        pet.getId() == received.getId()
        pet.getName() == received.getName()
    }


    /**
     * Updates a pet in the store with form data
     */
    void "updatePetWithForm() test"() {
        given:
        Long petId = 2L
        String name = "Jack the Bull"
        String status = "sold"

        when:
        api.updatePetWithForm(petId, name, status).block()

        then:
        Pet pet = api.getPetById(2L).block()
        petId == pet.getId()
        name == pet.getName()
        status == pet.getStatus().toString()
    }


    /**
     * uploads an image
     */
    void "upload file test"() throws IOException {
        given:
        Long petId = 2L
        String additionalMetadata = "a test file"

        File file = new File("hello.txt")
        BufferedWriter writer = new BufferedWriter(new FileWriter(file))
        writer.write("Hello world!")
        writer.close()

        when:
        api.uploadFile(petId, additionalMetadata, new File(file.getAbsolutePath()))

        then:
        notThrown()
    }

    void "findPetByTags() test"() {
        given:
        Tag tag = new Tag().name("cute").id(2L)

        Pet pet1 = new Pet().id(2000L).name("Conor").category(new Category().name("cats").id(10L))
                .photoUrls([].toSet()).tags([tag])
        Pet pet2 = new Pet().id(2001L).name("Mike").category(new Category().name("dogs").id(10L))
                .photoUrls([].toSet()).tags([tag])

        when:
        api.addPet(pet1).block()
        api.addPet(pet2).block()

        then: "The 2 pets should be successfully found with the given tag"
        Set<Pet> pets = api.findPetsByTags([tag.getName()].toSet()).block()

        pets.stream().allMatch(p -> p.getTags().contains(tag))

        Pet receivedPet1 = pets.stream().filter(p -> p.getId() == pet1.getId()).findFirst().orElse(null)
        Pet receivedPet2 = pets.stream().filter(p -> p.getId() == pet2.getId()).findFirst().orElse(null)
        receivedPet1 != null
        receivedPet2 != null
        pet1 == receivedPet1
        pet2 == receivedPet2
    }
}
