package org.openapitools.api

import org.openapitools.model.ModelApiResponse
import org.openapitools.model.Pet
import org.springframework.http.HttpStatus
import org.springframework.http.MediaType
import org.springframework.http.ResponseEntity
import org.springframework.web.context.request.NativeWebRequest
import org.springframework.http.server.reactive.ServerHttpRequest

import java.util.Optional

/**
 * A delegate to be called by the {@link PetApiController}}.
 * Implement this interface with a {@link org.springframework.stereotype.Service} annotated class.
 */
@javax.annotation.Generated(value = ["org.openapitools.codegen.languages.KotlinSpringServerCodegen"], comments = "Generator version: 7.23.0-SNAPSHOT")
interface PetApiDelegate {

    fun getRequest(): Optional<NativeWebRequest> = Optional.empty()

    /**
     * @see PetApi#addPet
     */
    fun addPet(pet: Pet): ResponseEntity<Pet> {
        getRequest().ifPresent { request ->
            for (mediaType in MediaType.parseMediaTypes(request.getHeader("Accept"))) {
                if (mediaType.isCompatibleWith(MediaType.valueOf("application/json"))) {
                    ApiUtil.setExampleResponse(request, "application/json", "{\n  \"id\" : 0,\n  \"category\" : {\n    \"id\" : 6,\n    \"name\" : \"name\"\n  },\n  \"name\" : \"doggie\",\n  \"photoUrls\" : [ \"photoUrls\", \"photoUrls\" ],\n  \"tags\" : [ {\n    \"id\" : 1,\n    \"name\" : \"name\"\n  }, {\n    \"id\" : 1,\n    \"name\" : \"name\"\n  } ],\n  \"status\" : \"available\"\n}")
                    break
                }
                if (mediaType.isCompatibleWith(MediaType.valueOf("application/xml"))) {
                    ApiUtil.setExampleResponse(request, "application/xml", "<Pet>\n  <id>123456789</id>\n  <Category>\n    <id>123456789</id>\n    <name>aeiou</name>\n  </Category>\n  <name>doggie</name>\n  <photoUrls>\n    <photoUrls>aeiou</photoUrls>\n  </photoUrls>\n  <tags>\n    <Tag>\n      <id>123456789</id>\n      <name>aeiou</name>\n    </Tag>\n  </tags>\n  <status>aeiou</status>\n</Pet>")
                    break
                }
            }
        }
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)

    }


    /**
     * @see PetApi#deletePet
     */
    fun deletePet(petId: kotlin.Long,
        apiKey: kotlin.String?): ResponseEntity<Unit> {
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)

    }


    /**
     * @see PetApi#findPetsByStatus
     */
    fun findPetsByStatus(status: kotlin.collections.List<kotlin.String>): ResponseEntity<List<Pet>> {
        getRequest().ifPresent { request ->
            for (mediaType in MediaType.parseMediaTypes(request.getHeader("Accept"))) {
                if (mediaType.isCompatibleWith(MediaType.valueOf("application/json"))) {
                    ApiUtil.setExampleResponse(request, "application/json", "[ {\n  \"id\" : 0,\n  \"category\" : {\n    \"id\" : 6,\n    \"name\" : \"name\"\n  },\n  \"name\" : \"doggie\",\n  \"photoUrls\" : [ \"photoUrls\", \"photoUrls\" ],\n  \"tags\" : [ {\n    \"id\" : 1,\n    \"name\" : \"name\"\n  }, {\n    \"id\" : 1,\n    \"name\" : \"name\"\n  } ],\n  \"status\" : \"available\"\n}, {\n  \"id\" : 0,\n  \"category\" : {\n    \"id\" : 6,\n    \"name\" : \"name\"\n  },\n  \"name\" : \"doggie\",\n  \"photoUrls\" : [ \"photoUrls\", \"photoUrls\" ],\n  \"tags\" : [ {\n    \"id\" : 1,\n    \"name\" : \"name\"\n  }, {\n    \"id\" : 1,\n    \"name\" : \"name\"\n  } ],\n  \"status\" : \"available\"\n} ]")
                    break
                }
                if (mediaType.isCompatibleWith(MediaType.valueOf("application/xml"))) {
                    ApiUtil.setExampleResponse(request, "application/xml", "<Pet>\n  <id>123456789</id>\n  <Category>\n    <id>123456789</id>\n    <name>aeiou</name>\n  </Category>\n  <name>doggie</name>\n  <photoUrls>\n    <photoUrls>aeiou</photoUrls>\n  </photoUrls>\n  <tags>\n    <Tag>\n      <id>123456789</id>\n      <name>aeiou</name>\n    </Tag>\n  </tags>\n  <status>aeiou</status>\n</Pet>")
                    break
                }
            }
        }
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)

    }


    /**
     * @see PetApi#findPetsByTags
     */
    fun findPetsByTags(tags: kotlin.collections.List<kotlin.String>): ResponseEntity<List<Pet>> {
        getRequest().ifPresent { request ->
            for (mediaType in MediaType.parseMediaTypes(request.getHeader("Accept"))) {
                if (mediaType.isCompatibleWith(MediaType.valueOf("application/json"))) {
                    ApiUtil.setExampleResponse(request, "application/json", "[ {\n  \"id\" : 0,\n  \"category\" : {\n    \"id\" : 6,\n    \"name\" : \"name\"\n  },\n  \"name\" : \"doggie\",\n  \"photoUrls\" : [ \"photoUrls\", \"photoUrls\" ],\n  \"tags\" : [ {\n    \"id\" : 1,\n    \"name\" : \"name\"\n  }, {\n    \"id\" : 1,\n    \"name\" : \"name\"\n  } ],\n  \"status\" : \"available\"\n}, {\n  \"id\" : 0,\n  \"category\" : {\n    \"id\" : 6,\n    \"name\" : \"name\"\n  },\n  \"name\" : \"doggie\",\n  \"photoUrls\" : [ \"photoUrls\", \"photoUrls\" ],\n  \"tags\" : [ {\n    \"id\" : 1,\n    \"name\" : \"name\"\n  }, {\n    \"id\" : 1,\n    \"name\" : \"name\"\n  } ],\n  \"status\" : \"available\"\n} ]")
                    break
                }
                if (mediaType.isCompatibleWith(MediaType.valueOf("application/xml"))) {
                    ApiUtil.setExampleResponse(request, "application/xml", "<Pet>\n  <id>123456789</id>\n  <Category>\n    <id>123456789</id>\n    <name>aeiou</name>\n  </Category>\n  <name>doggie</name>\n  <photoUrls>\n    <photoUrls>aeiou</photoUrls>\n  </photoUrls>\n  <tags>\n    <Tag>\n      <id>123456789</id>\n      <name>aeiou</name>\n    </Tag>\n  </tags>\n  <status>aeiou</status>\n</Pet>")
                    break
                }
            }
        }
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)

    }


    /**
     * @see PetApi#getPetById
     */
    fun getPetById(petId: kotlin.Long): ResponseEntity<Pet> {
        getRequest().ifPresent { request ->
            for (mediaType in MediaType.parseMediaTypes(request.getHeader("Accept"))) {
                if (mediaType.isCompatibleWith(MediaType.valueOf("application/json"))) {
                    ApiUtil.setExampleResponse(request, "application/json", "{\n  \"id\" : 0,\n  \"category\" : {\n    \"id\" : 6,\n    \"name\" : \"name\"\n  },\n  \"name\" : \"doggie\",\n  \"photoUrls\" : [ \"photoUrls\", \"photoUrls\" ],\n  \"tags\" : [ {\n    \"id\" : 1,\n    \"name\" : \"name\"\n  }, {\n    \"id\" : 1,\n    \"name\" : \"name\"\n  } ],\n  \"status\" : \"available\"\n}")
                    break
                }
                if (mediaType.isCompatibleWith(MediaType.valueOf("application/xml"))) {
                    ApiUtil.setExampleResponse(request, "application/xml", "<Pet>\n  <id>123456789</id>\n  <Category>\n    <id>123456789</id>\n    <name>aeiou</name>\n  </Category>\n  <name>doggie</name>\n  <photoUrls>\n    <photoUrls>aeiou</photoUrls>\n  </photoUrls>\n  <tags>\n    <Tag>\n      <id>123456789</id>\n      <name>aeiou</name>\n    </Tag>\n  </tags>\n  <status>aeiou</status>\n</Pet>")
                    break
                }
            }
        }
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)

    }


    /**
     * @see PetApi#updatePet
     */
    fun updatePet(pet: Pet): ResponseEntity<Pet> {
        getRequest().ifPresent { request ->
            for (mediaType in MediaType.parseMediaTypes(request.getHeader("Accept"))) {
                if (mediaType.isCompatibleWith(MediaType.valueOf("application/json"))) {
                    ApiUtil.setExampleResponse(request, "application/json", "{\n  \"id\" : 0,\n  \"category\" : {\n    \"id\" : 6,\n    \"name\" : \"name\"\n  },\n  \"name\" : \"doggie\",\n  \"photoUrls\" : [ \"photoUrls\", \"photoUrls\" ],\n  \"tags\" : [ {\n    \"id\" : 1,\n    \"name\" : \"name\"\n  }, {\n    \"id\" : 1,\n    \"name\" : \"name\"\n  } ],\n  \"status\" : \"available\"\n}")
                    break
                }
                if (mediaType.isCompatibleWith(MediaType.valueOf("application/xml"))) {
                    ApiUtil.setExampleResponse(request, "application/xml", "<Pet>\n  <id>123456789</id>\n  <name>doggie</name>\n  <photoUrls>\n    <photoUrls>aeiou</photoUrls>\n  </photoUrls>\n  <tags>\n  </tags>\n  <status>aeiou</status>\n</Pet>")
                    break
                }
            }
        }
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)

    }


    /**
     * @see PetApi#updatePetWithForm
     */
    fun updatePetWithForm(petId: kotlin.Long,
        name: kotlin.String?,
        status: kotlin.String?): ResponseEntity<Unit> {
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)

    }


    /**
     * @see PetApi#uploadFile
     */
    fun uploadFile(petId: kotlin.Long,
        additionalMetadata: kotlin.String?,
        file: org.springframework.web.multipart.MultipartFile): ResponseEntity<ModelApiResponse> {
        getRequest().ifPresent { request ->
            for (mediaType in MediaType.parseMediaTypes(request.getHeader("Accept"))) {
                if (mediaType.isCompatibleWith(MediaType.valueOf("application/json"))) {
                    ApiUtil.setExampleResponse(request, "application/json", "{\n  \"code\" : 0,\n  \"type\" : \"type\",\n  \"message\" : \"message\"\n}")
                    break
                }
            }
        }
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)

    }

}
