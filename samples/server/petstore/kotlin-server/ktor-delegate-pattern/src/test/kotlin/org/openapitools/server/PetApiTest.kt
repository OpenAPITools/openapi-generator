package org.openapitools.server

import io.ktor.client.call.*
import io.ktor.client.request.*
import io.ktor.client.request.forms.*
import io.ktor.http.*
import io.ktor.server.plugins.di.*
import io.ktor.server.routing.*
import io.ktor.utils.io.asByteWriteChannel
import io.ktor.utils.io.copyAndClose
import io.ktor.utils.io.copyTo
import io.ktor.utils.io.toByteArray
import kotlinx.io.Buffer
import kotlinx.io.readString
import org.openapitools.server.apis.PetApiDelegate
import org.openapitools.server.apis.PetApiDelegate.UploadFileMultiPartReceiver
import org.openapitools.server.models.Category
import org.openapitools.server.models.ModelApiResponse
import org.openapitools.server.models.Pet
import kotlin.test.Test
import kotlin.test.assertNull
import kotlin.test.assertEquals

class PetApiTest {

    @Test
    fun testAddPetShouldAddPet() = petstoreTestApplication {
        var receivedPet: Pet? = null
        class PetApiImpl: PetApiDelegate {
            override suspend fun addPet(pet: Pet, call: RoutingCall): Pet {
                receivedPet = pet
                return pet.copy()
            }
        }
        application.dependencies.provide<PetApiDelegate> { PetApiImpl() }
        val pet = Pet(name = "test", photoUrls = listOf("test.png"), category = Category(id = 1, name = "test"))
        val response = client.post("/pet") {
            contentType(ContentType.Application.Json)
            setBody(pet)
        }
        assertEquals(HttpStatusCode.OK, response.status)
        assertEquals(pet, receivedPet)
        assertEquals(pet, response.body<Pet>())
    }

    @Test
    fun testAddPetShouldAddPetWithStatus201() = petstoreTestApplication {
        var receivedPet: Pet? = null
        class PetApiImpl: PetApiDelegate {
            override suspend fun addPet(pet: Pet, call: RoutingCall): Pet {
                receivedPet = pet
                call.response.status(HttpStatusCode.Created)
                return pet.copy()
            }
        }
        application.dependencies.provide<PetApiDelegate> { PetApiImpl() }
        val pet = Pet(name = "test", photoUrls = listOf("test.png"), category = Category(id = 1, name = "test"))
        val response = client.post("/pet") {
            contentType(ContentType.Application.Json)
            setBody(pet)
        }
        assertEquals(HttpStatusCode.Created, response.status)
        assertEquals(pet, receivedPet)
        assertEquals(pet, response.body<Pet>())
    }

    @Test
    fun testAddPetShouldValidateCategoryName() = petstoreTestApplication {
        var receivedPet: Pet? = null
        class PetApiImpl: PetApiDelegate {
            override suspend fun addPet(pet: Pet, call: RoutingCall): Pet {
                receivedPet = pet
                return pet.copy()
            }
        }
        application.dependencies.provide<PetApiDelegate> { PetApiImpl() }

        val pet = Pet(name = "test", photoUrls = listOf("test.png"), category = Category(id = 1, name = "+test"))
        val response = client.post("/pet") {
            contentType(ContentType.Application.Json)
            setBody(pet)
        }
        assertEquals(HttpStatusCode.BadRequest, response.status)
        assertNull(receivedPet)
    }

    @Test
    fun testAddPetShouldRejectEmptyBody() = petstoreTestApplication {
        var receivedPet: Pet? = null
        class PetApiImpl: PetApiDelegate {
            override suspend fun addPet(pet: Pet, call: RoutingCall): Pet {
                receivedPet = pet
                return pet.copy()
            }
        }
        application.dependencies.provide<PetApiDelegate> { PetApiImpl() }

        val response = client.post("/pet") {
            contentType(ContentType.Application.Json)
            setBody("""{}""")
        }
        assertEquals(HttpStatusCode.BadRequest, response.status)
        assertNull(receivedPet)
    }

    @Test
    fun testAddPetShouldRejectMissingName() = petstoreTestApplication {
        var receivedPet: Pet? = null
        class PetApiImpl: PetApiDelegate {
            override suspend fun addPet(pet: Pet, call: RoutingCall): Pet {
                receivedPet = pet
                return pet.copy()
            }
        }
        application.dependencies.provide<PetApiDelegate> { PetApiImpl() }

        val response = client.post("/pet") {
            contentType(ContentType.Application.Json)
            setBody("""{"photoUrls":["test.png"]}""")
        }
        assertEquals(HttpStatusCode.BadRequest, response.status)
        assertNull(receivedPet)
    }

    @Test
    fun testAddPetShouldRejectMissingPhotoUrls() = petstoreTestApplication {
        var receivedPet: Pet? = null
        class PetApiImpl: PetApiDelegate {
            override suspend fun addPet(pet: Pet, call: RoutingCall): Pet {
                receivedPet = pet
                return pet.copy()
            }
        }
        application.dependencies.provide<PetApiDelegate> { PetApiImpl() }

        val response = client.post("/pet") {
            contentType(ContentType.Application.Json)
            setBody("""{"name":"test"}""")
        }
        assertEquals(HttpStatusCode.BadRequest, response.status)
        assertNull(receivedPet)
    }

    @Test
    fun testAddPetShouldRejectInvalidStatus() = petstoreTestApplication {
        var receivedPet: Pet? = null
        class PetApiImpl: PetApiDelegate {
            override suspend fun addPet(pet: Pet, call: RoutingCall): Pet {
                receivedPet = pet
                return pet.copy()
            }
        }
        application.dependencies.provide<PetApiDelegate> { PetApiImpl() }

        val response = client.post("/pet") {
            contentType(ContentType.Application.Json)
            setBody("""{"name":"test", "photoUrls":["test.png"], "status":"invalid"}""")
        }
        assertEquals(HttpStatusCode.BadRequest, response.status)
        assertNull(receivedPet)
    }

    @Test
    fun testAddPetShouldRejectInvalidPhotoUrls() = petstoreTestApplication {
        var receivedPet: Pet? = null
        class PetApiImpl: PetApiDelegate {
            override suspend fun addPet(pet: Pet, call: RoutingCall): Pet {
                receivedPet = pet
                return pet.copy()
            }
        }
        application.dependencies.provide<PetApiDelegate> { PetApiImpl() }

        val response = client.post("/pet") {
            contentType(ContentType.Application.Json)
            setBody("""{"name":"test", "photoUrls":"invalid"}""")
        }
        assertEquals(HttpStatusCode.BadRequest, response.status)
        assertNull(receivedPet)
    }

    @Test
    fun testAddPetShouldRejectEmptyCategoryName() = petstoreTestApplication {
        var receivedPet: Pet? = null
        class PetApiImpl: PetApiDelegate {
            override suspend fun addPet(pet: Pet, call: RoutingCall): Pet {
                receivedPet = pet
                return pet.copy()
            }
        }
        application.dependencies.provide<PetApiDelegate> { PetApiImpl() }

        val response = client.post("/pet") {
            contentType(ContentType.Application.Json)
            setBody("""{"name":"test", "photoUrls":["test.png"], "category":{"id":1, "name":""}}""")
        }
        assertEquals(HttpStatusCode.BadRequest, response.status)
        assertNull(receivedPet)
    }

    @Test
    fun testAddPetShouldRejectInvalidIdFormat() = petstoreTestApplication {
        var receivedPet: Pet? = null
        class PetApiImpl: PetApiDelegate {
            override suspend fun addPet(pet: Pet, call: RoutingCall): Pet {
                receivedPet = pet
                return pet.copy()
            }
        }
        application.dependencies.provide<PetApiDelegate> { PetApiImpl() }

        val response = client.post("/pet") {
            contentType(ContentType.Application.Json)
            setBody("""{"name":"test", "photoUrls":["test.png"], "id":"not-a-long"}""")
        }
        assertEquals(HttpStatusCode.BadRequest, response.status)
        assertNull(receivedPet)
    }

    @Test
    fun testAddPetShouldRejectInvalidCategoryIdFormat() = petstoreTestApplication {
        var receivedPet: Pet? = null
        class PetApiImpl: PetApiDelegate {
            override suspend fun addPet(pet: Pet, call: RoutingCall): Pet {
                receivedPet = pet
                return pet.copy()
            }
        }
        application.dependencies.provide<PetApiDelegate> { PetApiImpl() }

        val response = client.post("/pet") {
            contentType(ContentType.Application.Json)
            setBody("""{"name":"test", "photoUrls":["test.png"], "category":{"id":"not-a-long", "name":"test"}}""")
        }
        assertEquals(HttpStatusCode.BadRequest, response.status)
        assertNull(receivedPet)
    }

    @Test
    fun testAddPetShouldReturnRightStatusCodeWhenNotImplemented() = petstoreTestApplication {
        val pet = Pet(name = "test", photoUrls = listOf("test.png"), category = Category(id = 1, name = "test"))
        val response = client.post("/pet") {
            contentType(ContentType.Application.Json)
            setBody(pet)
        }
        assertEquals(HttpStatusCode.NotImplemented, response.status)
    }

    @Test
    fun testUpdatePetShouldUpdatePet() = petstoreTestApplication {
        var receivedPet: Pet? = null
        class PetApiImpl: PetApiDelegate {
            override suspend fun updatePet(pet: Pet, call: RoutingCall): Pet {
                receivedPet = pet
                return pet.copy()
            }
        }
        application.dependencies.provide<PetApiDelegate> { PetApiImpl() }
        val pet = Pet(name = "test", photoUrls = listOf("test.png"), category = Category(id = 1, name = "test"))
        val response = client.put("/pet") {
            contentType(ContentType.Application.Json)
            setBody(pet)
        }
        assertEquals(HttpStatusCode.OK, response.status)
        assertEquals(pet, receivedPet)
        assertEquals(pet, response.body<Pet>())
    }

    @Test
    fun testFindPetsByStatusShouldReturnPets() = petstoreTestApplication {
        val pets = listOf(Pet(name = "test", photoUrls = listOf("test.png")))
        var receivedStatuses: List<String>? = null
        class PetApiImpl: PetApiDelegate {
            override suspend fun findPetsByStatus(status: List<String>, call: RoutingCall): List<Pet> {
                receivedStatuses = status
                return pets
            }
        }
        application.dependencies.provide<PetApiDelegate> { PetApiImpl() }
        val response = client.get("/pet/findByStatus?status=available")
        assertEquals(HttpStatusCode.OK, response.status)
        assertEquals(listOf("available"), receivedStatuses)
        assertEquals(pets, response.body<List<Pet>>())
    }

    @Test
    fun testFindPetsByStatusShouldRejectMissingStatus() = petstoreTestApplication {
        val response = client.get("/pet/findByStatus")
        assertEquals(HttpStatusCode.BadRequest, response.status)
    }

    @Test
    fun testFindPetsByStatusShouldRejectInvalidStatus() = petstoreTestApplication {
        val response = client.get("/pet/findByStatus?status=invalid")
        assertEquals(HttpStatusCode.NotImplemented, response.status)
    }

    @Test
    fun testFindPetsByTagsShouldReturnPets() = petstoreTestApplication {
        val pets = listOf(Pet(name = "test", photoUrls = listOf("test.png")))
        var receivedTags: List<String>?=null
        class PetApiImpl: PetApiDelegate {
            override suspend fun findPetsByTags(tags: List<String>, call: RoutingCall): List<Pet> {
                receivedTags = tags
                return pets
            }
        }
        application.dependencies.provide<PetApiDelegate> { PetApiImpl() }
        val response = client.get("/pet/findByTags?tags=tag1")
        assertEquals(HttpStatusCode.OK, response.status)
        assertEquals(pets, response.body<List<Pet>>())
        assertEquals(listOf("tag1"), receivedTags)
    }

    @Test
    fun testGetPetByIdShouldReturnPet() = petstoreTestApplication {
        val pet = Pet(name = "test", photoUrls = listOf("test.png"))
        var receivedPetId: Long? = null
        class PetApiImpl: PetApiDelegate {
            override suspend fun getPetById(petId: Long, call: RoutingCall): Pet {
                receivedPetId = petId
                return pet
            }
        }
        application.dependencies.provide<PetApiDelegate> { PetApiImpl() }
        val response = client.get("/pet/1")
        assertEquals(HttpStatusCode.OK, response.status)
        assertEquals(pet, response.body<Pet>())
        assertEquals(1L, receivedPetId)
    }

    @Test
    fun testGetPetByIdShouldRejectInvalidId() = petstoreTestApplication {
        val response = client.get("/pet/invalid")
        assertEquals(HttpStatusCode.BadRequest, response.status)
    }

    @Test
    fun testUpdatePetWithFormShouldUpdatePet() = petstoreTestApplication {
        var receivedPetId: Long? = null
        var receivedName: String? = null
        var receivedStatus: String? = null
        class PetApiImpl: PetApiDelegate {
            override suspend fun updatePetWithForm(petId: Long, name: String?, status: String?, call: RoutingCall) {
                receivedPetId = petId
                receivedName = name
                receivedStatus = status
            }
        }
        application.dependencies.provide<PetApiDelegate> { PetApiImpl() }
        val response = client.post("/pet/1") {
            setBody(FormDataContent(Parameters.build {
                append("name", "new name")
                append("status", "sold")
            }))
        }
        assertEquals(HttpStatusCode.OK, response.status)
        assertEquals(1L, receivedPetId)
        assertEquals("new name", receivedName)
        assertEquals("sold", receivedStatus)
    }

    @Test
    fun testDeletePetShouldDeletePet() = petstoreTestApplication {
        var receivedPetId: Long? = null
        class PetApiImpl: PetApiDelegate {
            override suspend fun deletePet(petId: Long, apiKey: String?, call: RoutingCall) {
                receivedPetId = petId
            }
        }
        application.dependencies.provide<PetApiDelegate> { PetApiImpl() }
        val response = client.delete("/pet/1") {
            header("api_key", "test-key")
        }
        assertEquals(HttpStatusCode.OK, response.status)
        assertEquals(1L, receivedPetId)
    }

    @Test
    fun testUploadFileShouldUploadFile() = petstoreTestApplication {
        var receivedPetId: Long? = null
        var receivedAdditionalMetadata: String? = null
        var receivedFileName: String? = null
        var receivedFileContent:String? = null
        class PetApiImpl: PetApiDelegate {
            override suspend fun uploadFile(petId: kotlin.Long,  uploadFileMultipartReceiver: UploadFileMultiPartReceiver, call:RoutingCall): ModelApiResponse {
                receivedPetId = petId
                val receivedMultipart = uploadFileMultipartReceiver.receiveMultipart{
                    onReceiveFile {
                        receivedFileName = originalFileName
                        receivedFileContent = provider().toByteArray().decodeToString()
                    }
                }
                receivedAdditionalMetadata = receivedMultipart.additionalMetadata


                return ModelApiResponse(code = 200, message = "ok")
            }
        }
        application.dependencies.provide<PetApiDelegate> { PetApiImpl() }
        val response = client.submitFormWithBinaryData(
            url = "/pet/1/uploadImage",
            formData = formData {
                append("additionalMetadata", "test metadata")
                append("file", "test content".toByteArray(), Headers.build {
                    append(HttpHeaders.ContentType, "image/png")
                    append(HttpHeaders.ContentDisposition, "filename=\"test.png\"")
                })
            }
        )
        assertEquals(HttpStatusCode.OK, response.status)
        assertEquals(1L, receivedPetId)
        assertEquals("test metadata", receivedAdditionalMetadata)
        assertEquals("test.png", receivedFileName)
        assertEquals("test content", receivedFileContent)
    }
}


