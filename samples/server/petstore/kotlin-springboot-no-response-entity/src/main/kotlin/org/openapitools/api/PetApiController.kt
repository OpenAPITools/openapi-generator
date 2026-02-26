package org.openapitools.api

import org.openapitools.model.ModelApiResponse
import org.openapitools.model.Pet
import org.springframework.http.HttpStatus
import org.springframework.http.MediaType

import org.springframework.web.bind.annotation.*
import org.springframework.validation.annotation.Validated
import org.springframework.web.context.request.NativeWebRequest
import org.springframework.beans.factory.annotation.Autowired

import javax.validation.Valid
import javax.validation.constraints.DecimalMax
import javax.validation.constraints.DecimalMin
import javax.validation.constraints.Email
import javax.validation.constraints.Max
import javax.validation.constraints.Min
import javax.validation.constraints.NotNull
import javax.validation.constraints.Pattern
import javax.validation.constraints.Size

import kotlin.collections.List
import kotlin.collections.Map

@RestController
@Validated
@RequestMapping("\${api.base-path:/v2}")
class PetApiController(@Autowired(required = true) val service: PetApiService) {

    @ResponseStatus(HttpStatus.METHOD_NOT_ALLOWED)
    @RequestMapping(
        method = [RequestMethod.POST],
        // "/pet"
        value = [PATH_ADD_PET],
        consumes = ["application/json", "application/xml"]
    )
    fun addPet(
        @Valid @RequestBody body: Pet
    ): Unit {
        return service.addPet(body)
    }

    @ResponseStatus(HttpStatus.BAD_REQUEST)
    @RequestMapping(
        method = [RequestMethod.DELETE],
        // "/pet/{petId}"
        value = [PATH_DELETE_PET]
    )
    fun deletePet(
        @PathVariable("petId") petId: kotlin.Long,
        @RequestHeader(value = "api_key", required = false) apiKey: kotlin.String?
    ): Unit {
        return service.deletePet(petId, apiKey)
    }

    @ResponseStatus(HttpStatus.OK)
    @RequestMapping(
        method = [RequestMethod.GET],
        // "/pet/findByStatus"
        value = [PATH_FIND_PETS_BY_STATUS],
        produces = ["application/xml", "application/json"]
    )
    fun findPetsByStatus(
        @NotNull @Valid @RequestParam(value = "status", required = true) status: kotlin.collections.List<kotlin.String>
    ): List<Pet> {
        return service.findPetsByStatus(status)
    }

    @ResponseStatus(HttpStatus.OK)
    @RequestMapping(
        method = [RequestMethod.GET],
        // "/pet/findByTags"
        value = [PATH_FIND_PETS_BY_TAGS],
        produces = ["application/xml", "application/json"]
    )
    fun findPetsByTags(
        @NotNull @Valid @RequestParam(value = "tags", required = true) tags: kotlin.collections.List<kotlin.String>
    ): List<Pet> {
        return service.findPetsByTags(tags)
    }

    @ResponseStatus(HttpStatus.OK)
    @RequestMapping(
        method = [RequestMethod.GET],
        // "/pet/{petId}"
        value = [PATH_GET_PET_BY_ID],
        produces = ["application/xml", "application/json"]
    )
    fun getPetById(
        @PathVariable("petId") petId: kotlin.Long
    ): Pet {
        return service.getPetById(petId)
    }

    @ResponseStatus(HttpStatus.BAD_REQUEST)
    @RequestMapping(
        method = [RequestMethod.PUT],
        // "/pet"
        value = [PATH_UPDATE_PET],
        consumes = ["application/json", "application/xml"]
    )
    fun updatePet(
        @Valid @RequestBody body: Pet
    ): Unit {
        return service.updatePet(body)
    }

    @ResponseStatus(HttpStatus.METHOD_NOT_ALLOWED)
    @RequestMapping(
        method = [RequestMethod.POST],
        // "/pet/{petId}"
        value = [PATH_UPDATE_PET_WITH_FORM],
        consumes = ["application/x-www-form-urlencoded"]
    )
    fun updatePetWithForm(
        @PathVariable("petId") petId: kotlin.Long,
        @Valid @RequestParam(value = "name", required = false) name: kotlin.String?,
        @Valid @RequestParam(value = "status", required = false) status: kotlin.String?
    ): Unit {
        return service.updatePetWithForm(petId, name, status)
    }

    @ResponseStatus(HttpStatus.OK)
    @RequestMapping(
        method = [RequestMethod.POST],
        // "/pet/{petId}/uploadImage"
        value = [PATH_UPLOAD_FILE],
        produces = ["application/json"],
        consumes = ["multipart/form-data"]
    )
    fun uploadFile(
        @PathVariable("petId") petId: kotlin.Long,
        @Valid @RequestParam(value = "additionalMetadata", required = false) additionalMetadata: kotlin.String?,
        @Valid @RequestPart("file", required = false) file: org.springframework.web.multipart.MultipartFile
    ): ModelApiResponse {
        return service.uploadFile(petId, additionalMetadata, file)
    }

    companion object {
        //for your own safety never directly reuse these path definitions in tests
        const val BASE_PATH: String = "/v2"
        const val PATH_ADD_PET: String = "/pet"
        const val PATH_DELETE_PET: String = "/pet/{petId}"
        const val PATH_FIND_PETS_BY_STATUS: String = "/pet/findByStatus"
        const val PATH_FIND_PETS_BY_TAGS: String = "/pet/findByTags"
        const val PATH_GET_PET_BY_ID: String = "/pet/{petId}"
        const val PATH_UPDATE_PET: String = "/pet"
        const val PATH_UPDATE_PET_WITH_FORM: String = "/pet/{petId}"
        const val PATH_UPLOAD_FILE: String = "/pet/{petId}/uploadImage"
    }
}
