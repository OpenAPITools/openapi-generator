package org.openapitools.api

import org.openapitools.model.ModelApiResponse
import org.openapitools.model.Pet
import org.springframework.http.HttpStatus
import org.springframework.http.MediaType
import org.springframework.web.context.request.NativeWebRequest

import java.util.Optional

/**
 * A delegate to be called by the {@link PetApiController}}.
 * Implement this interface with a {@link org.springframework.stereotype.Service} annotated class.
 */
@javax.annotation.Generated(value = ["org.openapitools.codegen.languages.KotlinSpringServerCodegen"], comments = "Generator version: 7.18.0-SNAPSHOT")
interface PetApiDelegate {

    fun getRequest(): Optional<NativeWebRequest> = Optional.empty()

    /**
     * @see PetApi#addPet
     */
    fun addPet(body: Pet): Unit {
        return TODO("Not yet implemented")

    }


    /**
     * @see PetApi#deletePet
     */
    fun deletePet(petId: kotlin.Long,
        apiKey: kotlin.String?): Unit {
        return TODO("Not yet implemented")

    }


    /**
     * @see PetApi#findPetsByStatus
     */
    fun findPetsByStatus(status: kotlin.collections.List<kotlin.String>): List<Pet> {
        getRequest().ifPresent { request ->
            for (mediaType in MediaType.parseMediaTypes(request.getHeader("Accept"))) {
                if (mediaType.isCompatibleWith(MediaType.valueOf("application/json"))) {
                    ApiUtil.setExampleResponse(request, "application/json", "[ {  \"photoUrls\" : [ \"photoUrls\", \"photoUrls\" ],  \"name\" : \"doggie\",  \"id\" : 0,  \"category\" : {    \"name\" : \"name\",    \"id\" : 6  },  \"tags\" : [ {    \"name\" : \"name\",    \"id\" : 1  }, {    \"name\" : \"name\",    \"id\" : 1  } ],  \"status\" : \"available\"}, {  \"photoUrls\" : [ \"photoUrls\", \"photoUrls\" ],  \"name\" : \"doggie\",  \"id\" : 0,  \"category\" : {    \"name\" : \"name\",    \"id\" : 6  },  \"tags\" : [ {    \"name\" : \"name\",    \"id\" : 1  }, {    \"name\" : \"name\",    \"id\" : 1  } ],  \"status\" : \"available\"} ]")
                    break
                }
                if (mediaType.isCompatibleWith(MediaType.valueOf("application/xml"))) {
                    ApiUtil.setExampleResponse(request, "application/xml", "<Pet>  <id>123456789</id>  <Category>    <id>123456789</id>    <name>aeiou</name>  </Category>  <name>doggie</name>  <photoUrls>    <photoUrls>aeiou</photoUrls>  </photoUrls>  <tags>    <Tag>      <id>123456789</id>      <name>aeiou</name>    </Tag>  </tags>  <status>aeiou</status></Pet>")
                    break
                }
            }
        }
        return TODO("Not yet implemented")

    }


    /**
     * @see PetApi#findPetsByTags
     */
    fun findPetsByTags(tags: kotlin.collections.List<kotlin.String>): List<Pet> {
        getRequest().ifPresent { request ->
            for (mediaType in MediaType.parseMediaTypes(request.getHeader("Accept"))) {
                if (mediaType.isCompatibleWith(MediaType.valueOf("application/json"))) {
                    ApiUtil.setExampleResponse(request, "application/json", "[ {  \"photoUrls\" : [ \"photoUrls\", \"photoUrls\" ],  \"name\" : \"doggie\",  \"id\" : 0,  \"category\" : {    \"name\" : \"name\",    \"id\" : 6  },  \"tags\" : [ {    \"name\" : \"name\",    \"id\" : 1  }, {    \"name\" : \"name\",    \"id\" : 1  } ],  \"status\" : \"available\"}, {  \"photoUrls\" : [ \"photoUrls\", \"photoUrls\" ],  \"name\" : \"doggie\",  \"id\" : 0,  \"category\" : {    \"name\" : \"name\",    \"id\" : 6  },  \"tags\" : [ {    \"name\" : \"name\",    \"id\" : 1  }, {    \"name\" : \"name\",    \"id\" : 1  } ],  \"status\" : \"available\"} ]")
                    break
                }
                if (mediaType.isCompatibleWith(MediaType.valueOf("application/xml"))) {
                    ApiUtil.setExampleResponse(request, "application/xml", "<Pet>  <id>123456789</id>  <Category>    <id>123456789</id>    <name>aeiou</name>  </Category>  <name>doggie</name>  <photoUrls>    <photoUrls>aeiou</photoUrls>  </photoUrls>  <tags>    <Tag>      <id>123456789</id>      <name>aeiou</name>    </Tag>  </tags>  <status>aeiou</status></Pet>")
                    break
                }
            }
        }
        return TODO("Not yet implemented")

    }


    /**
     * @see PetApi#getPetById
     */
    fun getPetById(petId: kotlin.Long): Pet {
        getRequest().ifPresent { request ->
            for (mediaType in MediaType.parseMediaTypes(request.getHeader("Accept"))) {
                if (mediaType.isCompatibleWith(MediaType.valueOf("application/json"))) {
                    ApiUtil.setExampleResponse(request, "application/json", "{  \"photoUrls\" : [ \"photoUrls\", \"photoUrls\" ],  \"name\" : \"doggie\",  \"id\" : 0,  \"category\" : {    \"name\" : \"name\",    \"id\" : 6  },  \"tags\" : [ {    \"name\" : \"name\",    \"id\" : 1  }, {    \"name\" : \"name\",    \"id\" : 1  } ],  \"status\" : \"available\"}")
                    break
                }
                if (mediaType.isCompatibleWith(MediaType.valueOf("application/xml"))) {
                    ApiUtil.setExampleResponse(request, "application/xml", "<Pet>  <id>123456789</id>  <Category>    <id>123456789</id>    <name>aeiou</name>  </Category>  <name>doggie</name>  <photoUrls>    <photoUrls>aeiou</photoUrls>  </photoUrls>  <tags>    <Tag>      <id>123456789</id>      <name>aeiou</name>    </Tag>  </tags>  <status>aeiou</status></Pet>")
                    break
                }
            }
        }
        return TODO("Not yet implemented")

    }


    /**
     * @see PetApi#updatePet
     */
    fun updatePet(body: Pet): Unit {
        return TODO("Not yet implemented")

    }


    /**
     * @see PetApi#updatePetWithForm
     */
    fun updatePetWithForm(petId: kotlin.Long,
        name: kotlin.String?,
        status: kotlin.String?): Unit {
        return TODO("Not yet implemented")

    }


    /**
     * @see PetApi#uploadFile
     */
    fun uploadFile(petId: kotlin.Long,
        additionalMetadata: kotlin.String?,
        file: org.springframework.web.multipart.MultipartFile): ModelApiResponse {
        getRequest().ifPresent { request ->
            for (mediaType in MediaType.parseMediaTypes(request.getHeader("Accept"))) {
                if (mediaType.isCompatibleWith(MediaType.valueOf("application/json"))) {
                    ApiUtil.setExampleResponse(request, "application/json", "{  \"code\" : 0,  \"type\" : \"type\",  \"message\" : \"message\"}")
                    break
                }
            }
        }
        return TODO("Not yet implemented")

    }

}
