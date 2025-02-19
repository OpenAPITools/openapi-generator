package openapitools

import org.junit.Test
import org.openapitools.api.PetApi
import org.openapitools.model.Category
import org.openapitools.model.Pet
import org.openapitools.model.Tag


class PetApiTest extends GroovyTestCase  {

    Long petId = 10009;
    PetApi petApi = new PetApi()

    @Test
    void testAddAndGetPet() {
        def pet = new Pet()
        pet.setId(this.petId)
        pet.setName("groovy client test")
        pet.setPhotoUrls(["http://test_groovy_unit_test.com"])
        pet.setCategory(new Category(this.petId, "test groovy category"))
        pet.setTags([new Tag(this.petId, "test groovy tag")])
        this.petApi.addPet(pet) {
        }
        {
            statusCode, message ->
                assertEquals(200, statusCode)
        };

        this.petApi.getPetById(this.petId) {
            def petGetted = (Pet)it
            assertEquals(this.petId, petGetted.getId())
            assertEquals("groovy client test", petGetted.getName())
            assertEquals(this.petId, petGetted.getCategory().getId())
            assertEquals("test groovy category", petGetted.getCategory().getName())
            assertEquals(this.petId, petGetted.getTags()[0].id)
            assertEquals("test groovy tag", petGetted.getTags()[0].name)
        }
        {
            statusCode, message ->
                assertEquals(200, statusCode)
        };

    }

    @Test
    void testUpdateOnePet() {
        def pet = new Pet()
        pet.setId(this.petId)
        pet.setName("groovy client updatetest")
        pet.setStatus("pending")
        this.petApi.updatePet(pet) {
        }
        {
            statusCode, message ->
                assertEquals(200, statusCode)
        };

        this.petApi.getPetById(this.petId) {
            def petGetted = (Pet)it
            assertEquals(this.petId, petGetted.getId())
            assertEquals("groovy client updatetest", petGetted.getName())
            assertEquals("pending", petGetted.getStatus())
        }
        {
            statusCode, message ->
                assertEquals(200, statusCode)
        };

        this.petApi.updatePetWithForm(this.petId, "groovy client updatetestwithform", "sold") {
        }
                {
                    statusCode, message ->
                        assertEquals(200, statusCode)
                };

        this.petApi.getPetById(this.petId) {
            def petGetted = (Pet)it
            assertEquals(this.petId, petGetted.getId())
            assertEquals("groovy client updatetestwithform", petGetted.getName())
            assertEquals("sold", petGetted.getStatus())
        }
                {
                    statusCode, message ->
                        assertEquals(200, statusCode)
                };

        this.petApi.deletePet(this.petId, "apiKey") {
        }
{
            statusCode, message ->
                assertEquals(200, statusCode)
        };

        // should throw a 404 after delete
        this.petApi.getPetById(this.petId) {
            assertEquals(404, 200)
        }
{
            statusCode, message ->
                assertEquals(404, statusCode)
        };

    }

    @Test
    void testGetPetByStatus() {
        this.petApi.findPetsByStatus(["sold"]) {
            def listPets = (ArrayList)it
            assertTrue(listPets.size() > 0)
        }
        {
            statusCode, message ->
                assertEquals(200, statusCode)
        };

    }

}
