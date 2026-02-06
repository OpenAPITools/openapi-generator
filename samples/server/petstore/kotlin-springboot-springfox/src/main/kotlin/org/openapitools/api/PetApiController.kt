package org.openapitools.api

import org.openapitools.model.ModelApiResponse
import org.openapitools.model.Pet
import io.swagger.annotations.Api
import io.swagger.annotations.ApiOperation
import io.swagger.annotations.ApiParam
import io.swagger.annotations.ApiResponse
import io.swagger.annotations.ApiResponses
import io.swagger.annotations.Authorization
import io.swagger.annotations.AuthorizationScope
import org.springframework.http.HttpStatus
import org.springframework.http.MediaType
import org.springframework.http.ResponseEntity

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
@Api(value = "pet", description = "The pet API")
@RequestMapping("\${api.base-path:/v2}")
class PetApiController(@Autowired(required = true) val service: PetApiService) {


    @ApiOperation(
        value = "Add a new pet to the store",
        nickname = "addPet",
        notes = "",
        authorizations = [Authorization(value = "petstore_auth", scopes = [AuthorizationScope(scope = "write:pets", description = "modify pets in your account"), AuthorizationScope(scope = "read:pets", description = "read your pets")])])
    @ApiResponses(
        value = [ApiResponse(code = 405, message = "Invalid input")])
    @RequestMapping(
        method = [RequestMethod.POST],
        // "/pet"
        value = [PATH_ADD_PET],
        consumes = ["application/json", "application/xml"]
    )
    fun addPet(
        @ApiParam(value = "Pet object that needs to be added to the store", required = true) @Valid @RequestBody body: Pet
    ): ResponseEntity<Unit> {
        return ResponseEntity(service.addPet(body), HttpStatus.valueOf(405))
    }


    @ApiOperation(
        value = "Deletes a pet",
        nickname = "deletePet",
        notes = "",
        authorizations = [Authorization(value = "petstore_auth", scopes = [AuthorizationScope(scope = "write:pets", description = "modify pets in your account"), AuthorizationScope(scope = "read:pets", description = "read your pets")])])
    @ApiResponses(
        value = [ApiResponse(code = 400, message = "Invalid pet value")])
    @RequestMapping(
        method = [RequestMethod.DELETE],
        // "/pet/{petId}"
        value = [PATH_DELETE_PET]
    )
    fun deletePet(
        @ApiParam(value = "Pet id to delete", required = true) @PathVariable("petId") petId: kotlin.Long,
        @ApiParam(value = "") @RequestHeader(value = "api_key", required = false) apiKey: kotlin.String?
    ): ResponseEntity<Unit> {
        return ResponseEntity(service.deletePet(petId, apiKey), HttpStatus.valueOf(400))
    }


    @ApiOperation(
        value = "Finds Pets by status",
        nickname = "findPetsByStatus",
        notes = "Multiple status values can be provided with comma separated strings",
        response = Pet::class,
        responseContainer = "List",
        authorizations = [Authorization(value = "petstore_auth", scopes = [AuthorizationScope(scope = "write:pets", description = "modify pets in your account"), AuthorizationScope(scope = "read:pets", description = "read your pets")])])
    @ApiResponses(
        value = [ApiResponse(code = 200, message = "successful operation", response = Pet::class, responseContainer = "List"),ApiResponse(code = 400, message = "Invalid status value")])
    @RequestMapping(
        method = [RequestMethod.GET],
        // "/pet/findByStatus"
        value = [PATH_FIND_PETS_BY_STATUS],
        produces = ["application/xml", "application/json"]
    )
    fun findPetsByStatus(
        @NotNull @ApiParam(value = "Status values that need to be considered for filter", required = true, allowableValues = "available, pending, sold") @Valid @RequestParam(value = "status", required = true) status: kotlin.collections.List<kotlin.String>
    ): ResponseEntity<List<Pet>> {
        return ResponseEntity(service.findPetsByStatus(status), HttpStatus.valueOf(200))
    }


    @ApiOperation(
        value = "Finds Pets by tags",
        nickname = "findPetsByTags",
        notes = "Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.",
        response = Pet::class,
        responseContainer = "List",
        authorizations = [Authorization(value = "petstore_auth", scopes = [AuthorizationScope(scope = "write:pets", description = "modify pets in your account"), AuthorizationScope(scope = "read:pets", description = "read your pets")])])
    @ApiResponses(
        value = [ApiResponse(code = 200, message = "successful operation", response = Pet::class, responseContainer = "List"),ApiResponse(code = 400, message = "Invalid tag value")])
    @RequestMapping(
        method = [RequestMethod.GET],
        // "/pet/findByTags"
        value = [PATH_FIND_PETS_BY_TAGS],
        produces = ["application/xml", "application/json"]
    )
    fun findPetsByTags(
        @NotNull @ApiParam(value = "Tags to filter by", required = true) @Valid @RequestParam(value = "tags", required = true) tags: kotlin.collections.List<kotlin.String>
    ): ResponseEntity<List<Pet>> {
        return ResponseEntity(service.findPetsByTags(tags), HttpStatus.valueOf(200))
    }


    @ApiOperation(
        value = "Find pet by ID",
        nickname = "getPetById",
        notes = "Returns a single pet",
        response = Pet::class,
        authorizations = [Authorization(value = "api_key")])
    @ApiResponses(
        value = [ApiResponse(code = 200, message = "successful operation", response = Pet::class),ApiResponse(code = 400, message = "Invalid ID supplied"),ApiResponse(code = 404, message = "Pet not found")])
    @RequestMapping(
        method = [RequestMethod.GET],
        // "/pet/{petId}"
        value = [PATH_GET_PET_BY_ID],
        produces = ["application/xml", "application/json"]
    )
    fun getPetById(
        @ApiParam(value = "ID of pet to return", required = true) @PathVariable("petId") petId: kotlin.Long
    ): ResponseEntity<Pet> {
        return ResponseEntity(service.getPetById(petId), HttpStatus.valueOf(200))
    }


    @ApiOperation(
        value = "Update an existing pet",
        nickname = "updatePet",
        notes = "",
        authorizations = [Authorization(value = "petstore_auth", scopes = [AuthorizationScope(scope = "write:pets", description = "modify pets in your account"), AuthorizationScope(scope = "read:pets", description = "read your pets")])])
    @ApiResponses(
        value = [ApiResponse(code = 400, message = "Invalid ID supplied"),ApiResponse(code = 404, message = "Pet not found"),ApiResponse(code = 405, message = "Validation exception")])
    @RequestMapping(
        method = [RequestMethod.PUT],
        // "/pet"
        value = [PATH_UPDATE_PET],
        consumes = ["application/json", "application/xml"]
    )
    fun updatePet(
        @ApiParam(value = "Pet object that needs to be added to the store", required = true) @Valid @RequestBody body: Pet
    ): ResponseEntity<Unit> {
        return ResponseEntity(service.updatePet(body), HttpStatus.valueOf(400))
    }


    @ApiOperation(
        value = "Updates a pet in the store with form data",
        nickname = "updatePetWithForm",
        notes = "",
        authorizations = [Authorization(value = "petstore_auth", scopes = [AuthorizationScope(scope = "write:pets", description = "modify pets in your account"), AuthorizationScope(scope = "read:pets", description = "read your pets")])])
    @ApiResponses(
        value = [ApiResponse(code = 405, message = "Invalid input")])
    @RequestMapping(
        method = [RequestMethod.POST],
        // "/pet/{petId}"
        value = [PATH_UPDATE_PET_WITH_FORM],
        consumes = ["application/x-www-form-urlencoded"]
    )
    fun updatePetWithForm(
        @ApiParam(value = "ID of pet that needs to be updated", required = true) @PathVariable("petId") petId: kotlin.Long,
        @ApiParam(value = "Updated name of the pet") @Valid @RequestParam(value = "name", required = false) name: kotlin.String?,
        @ApiParam(value = "Updated status of the pet") @Valid @RequestParam(value = "status", required = false) status: kotlin.String?
    ): ResponseEntity<Unit> {
        return ResponseEntity(service.updatePetWithForm(petId, name, status), HttpStatus.valueOf(405))
    }


    @ApiOperation(
        value = "uploads an image",
        nickname = "uploadFile",
        notes = "",
        response = ModelApiResponse::class,
        authorizations = [Authorization(value = "petstore_auth", scopes = [AuthorizationScope(scope = "write:pets", description = "modify pets in your account"), AuthorizationScope(scope = "read:pets", description = "read your pets")])])
    @ApiResponses(
        value = [ApiResponse(code = 200, message = "successful operation", response = ModelApiResponse::class)])
    @RequestMapping(
        method = [RequestMethod.POST],
        // "/pet/{petId}/uploadImage"
        value = [PATH_UPLOAD_FILE],
        produces = ["application/json"],
        consumes = ["multipart/form-data"]
    )
    fun uploadFile(
        @ApiParam(value = "ID of pet to update", required = true) @PathVariable("petId") petId: kotlin.Long,
        @ApiParam(value = "Additional data to pass to server") @Valid @RequestParam(value = "additionalMetadata", required = false) additionalMetadata: kotlin.String?,
        @ApiParam(value = "file detail") @Valid @RequestPart("file", required = false) file: org.springframework.web.multipart.MultipartFile
    ): ResponseEntity<ModelApiResponse> {
        return ResponseEntity(service.uploadFile(petId, additionalMetadata, file), HttpStatus.valueOf(200))
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
