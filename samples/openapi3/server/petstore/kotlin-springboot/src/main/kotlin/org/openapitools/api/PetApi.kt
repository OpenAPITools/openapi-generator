package org.openapitools.api

import org.openapitools.model.ModelApiResponse
import org.openapitools.model.Pet
import org.springframework.http.HttpStatus
import org.springframework.http.MediaType
import org.springframework.http.ResponseEntity

import org.springframework.web.bind.annotation.RequestBody
import org.springframework.web.bind.annotation.RequestPart
import org.springframework.web.bind.annotation.RequestParam
import org.springframework.web.bind.annotation.PathVariable
import org.springframework.web.bind.annotation.RequestHeader
import org.springframework.web.bind.annotation.RequestMethod
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.web.bind.annotation.RestController
import org.springframework.validation.annotation.Validated
import org.springframework.web.context.request.NativeWebRequest
import org.springframework.beans.factory.annotation.Autowired

import javax.validation.Valid
import javax.validation.constraints.DecimalMax
import javax.validation.constraints.DecimalMin
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
class PetApiController() {


    @RequestMapping(
        value = ["/pet"],
        produces = ["application/xml", "application/json"], 
        consumes = ["application/json", "application/xml"],
        method = [RequestMethod.POST])
    fun addPet( @Valid @RequestBody pet: Pet
): ResponseEntity<Pet> {
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)
    }


    @RequestMapping(
        value = ["/pet/{petId}"],
        method = [RequestMethod.DELETE])
    fun deletePet( @PathVariable("petId") petId: kotlin.Long
, @RequestHeader(value="api_key", required=false) apiKey: kotlin.String?
): ResponseEntity<Unit> {
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)
    }


    @RequestMapping(
        value = ["/pet/findByStatus"],
        produces = ["application/xml", "application/json"], 
        method = [RequestMethod.GET])
    fun findPetsByStatus(@NotNull  @RequestParam(value = "status", required = true) status: kotlin.collections.List<kotlin.String>
): ResponseEntity<List<Pet>> {
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)
    }


    @RequestMapping(
        value = ["/pet/findByTags"],
        produces = ["application/xml", "application/json"], 
        method = [RequestMethod.GET])
    fun findPetsByTags(@NotNull  @RequestParam(value = "tags", required = true) tags: kotlin.collections.List<kotlin.String>
): ResponseEntity<List<Pet>> {
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)
    }


    @RequestMapping(
        value = ["/pet/{petId}"],
        produces = ["application/xml", "application/json"], 
        method = [RequestMethod.GET])
    fun getPetById( @PathVariable("petId") petId: kotlin.Long
): ResponseEntity<Pet> {
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)
    }


    @RequestMapping(
        value = ["/pet"],
        produces = ["application/xml", "application/json"], 
        consumes = ["application/json", "application/xml"],
        method = [RequestMethod.PUT])
    fun updatePet( @Valid @RequestBody pet: Pet
): ResponseEntity<Pet> {
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)
    }


    @RequestMapping(
        value = ["/pet/{petId}"],
        consumes = ["application/x-www-form-urlencoded"],
        method = [RequestMethod.POST])
    fun updatePetWithForm( @PathVariable("petId") petId: kotlin.Long
, @RequestParam(value="name", required=false) name: kotlin.String? 
, @RequestParam(value="status", required=false) status: kotlin.String? 
): ResponseEntity<Unit> {
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)
    }


    @RequestMapping(
        value = ["/pet/{petId}/uploadImage"],
        produces = ["application/json"], 
        consumes = ["multipart/form-data"],
        method = [RequestMethod.POST])
    fun uploadFile( @PathVariable("petId") petId: kotlin.Long
, @RequestParam(value="additionalMetadata", required=false) additionalMetadata: kotlin.String? 
, @Valid @RequestPart("file") file: org.springframework.core.io.Resource?
): ResponseEntity<ModelApiResponse> {
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)
    }
}
