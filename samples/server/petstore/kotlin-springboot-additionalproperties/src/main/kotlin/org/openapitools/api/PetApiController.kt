package org.openapitools.api

import org.openapitools.model.ModelApiResponse
import org.openapitools.model.Pet
import org.springframework.http.HttpStatus
import org.springframework.http.MediaType
import org.springframework.http.ResponseEntity

import org.springframework.web.bind.annotation.*
import org.springframework.validation.annotation.Validated
import org.springframework.web.context.request.NativeWebRequest
import org.springframework.beans.factory.annotation.Autowired

import jakarta.validation.Valid
import jakarta.validation.constraints.DecimalMax
import jakarta.validation.constraints.DecimalMin
import jakarta.validation.constraints.Email
import jakarta.validation.constraints.Max
import jakarta.validation.constraints.Min
import jakarta.validation.constraints.NotNull
import jakarta.validation.constraints.Pattern
import jakarta.validation.constraints.Size

import kotlin.collections.List
import kotlin.collections.Map

@RestController
@Validated
class PetApiController(@Autowired(required = true) val service: PetApiService) {


    @RequestMapping(
        method = [RequestMethod.POST],
        // "/pet"
        value = [PATH_ADD_PET],
        produces = ["application/xml", "application/json"],
        consumes = ["application/json", "application/xml"]
    )
    fun addPet(
        @Valid @RequestBody pet: Pet
    ): ResponseEntity<Pet> {
        return ResponseEntity(service.addPet(pet), HttpStatus.valueOf(200))
    }


    @RequestMapping(
        method = [RequestMethod.DELETE],
        // "/pet/{petId}"
        value = [PATH_DELETE_PET]
    )
    fun deletePet(
        @PathVariable("petId") petId: kotlin.Long,
        @RequestHeader(value = "api_key", required = false) apiKey: kotlin.String?
    ): ResponseEntity<Unit> {
        return ResponseEntity(service.deletePet(petId, apiKey), HttpStatus.valueOf(400))
    }


    @RequestMapping(
        method = [RequestMethod.GET],
        // "/pet/findByStatus"
        value = [PATH_FIND_PETS_BY_STATUS],
        produces = ["application/xml", "application/json"]
    )
    fun findPetsByStatus(
        @NotNull @Valid @RequestParam(value = "status", required = true) status: kotlin.collections.List<kotlin.String>
    ): ResponseEntity<List<Pet>> {
        return ResponseEntity(service.findPetsByStatus(status), HttpStatus.valueOf(200))
    }


    @RequestMapping(
        method = [RequestMethod.GET],
        // "/pet/findByTags"
        value = [PATH_FIND_PETS_BY_TAGS],
        produces = ["application/xml", "application/json"]
    )
    fun findPetsByTags(
        @NotNull @Valid @RequestParam(value = "tags", required = true) tags: kotlin.collections.List<kotlin.String>
    ): ResponseEntity<List<Pet>> {
        return ResponseEntity(service.findPetsByTags(tags), HttpStatus.valueOf(200))
    }


    @RequestMapping(
        method = [RequestMethod.GET],
        // "/pet/{petId}"
        value = [PATH_GET_PET_BY_ID],
        produces = ["application/xml", "application/json"]
    )
    fun getPetById(
        @PathVariable("petId") petId: kotlin.Long
    ): ResponseEntity<Pet> {
        return ResponseEntity(service.getPetById(petId), HttpStatus.valueOf(200))
    }


    @RequestMapping(
        method = [RequestMethod.PUT],
        // "/pet"
        value = [PATH_UPDATE_PET],
        produces = ["application/xml", "application/json"],
        consumes = ["application/json", "application/xml"]
    )
    fun updatePet(
        @Valid @RequestBody pet: Pet
    ): ResponseEntity<Pet> {
        return ResponseEntity(service.updatePet(pet), HttpStatus.valueOf(200))
    }


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
    ): ResponseEntity<Unit> {
        return ResponseEntity(service.updatePetWithForm(petId, name, status), HttpStatus.valueOf(405))
    }


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
    ): ResponseEntity<ModelApiResponse> {
        return ResponseEntity(service.uploadFile(petId, additionalMetadata, file), HttpStatus.valueOf(200))
    }

    companion object {
        //for your own safety never directly reuse these path definitions in tests
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
