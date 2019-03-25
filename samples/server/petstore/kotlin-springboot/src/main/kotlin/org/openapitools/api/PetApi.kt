package org.openapitools.api

import org.openapitools.model.ModelApiResponse
import org.openapitools.model.Pet
import io.swagger.annotations.*
import org.springframework.http.HttpStatus
import org.springframework.http.MediaType
import org.springframework.http.ResponseEntity
import org.springframework.stereotype.Controller
import org.springframework.web.bind.annotation.RequestBody
import org.springframework.web.bind.annotation.RequestPart
import org.springframework.web.bind.annotation.RequestParam
import org.springframework.web.bind.annotation.PathVariable
import org.springframework.web.bind.annotation.RequestHeader
import org.springframework.web.bind.annotation.RequestMethod
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.validation.annotation.Validated
import org.springframework.web.context.request.NativeWebRequest
import org.springframework.web.multipart.MultipartFile
import org.springframework.beans.factory.annotation.Autowired

import javax.validation.Valid
import javax.validation.constraints.*

import kotlin.collections.List
import kotlin.collections.Map

@Controller
@Validated
@Api(value = "Pet", description = "The Pet API")
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
            value = ["/pet"],
            consumes = ["application/json", "application/xml"],
            method = [RequestMethod.POST])
    fun addPet(@ApiParam(value = "Pet object that needs to be added to the store" ,required=true ) @Valid @RequestBody body: Pet): ResponseEntity<Unit> {
        return ResponseEntity(service.addPet(body), HttpStatus.OK)
    }

    @ApiOperation(
            value = "Deletes a pet",
            nickname = "deletePet",
            notes = "",
            authorizations = [Authorization(value = "petstore_auth", scopes = [AuthorizationScope(scope = "write:pets", description = "modify pets in your account"), AuthorizationScope(scope = "read:pets", description = "read your pets")])])
    @ApiResponses(
            value = [ApiResponse(code = 400, message = "Invalid pet value")])
    @RequestMapping(
            value = ["/pet/{petId}"],
            method = [RequestMethod.DELETE])
    fun deletePet(@ApiParam(value = "Pet id to delete", required=true, defaultValue="null") @PathVariable("petId") petId: Long,@ApiParam(value = "" , defaultValue="null") @RequestHeader(value="api_key", required=false) apiKey: String): ResponseEntity<Unit> {
        return ResponseEntity(service.deletePet(petId, apiKey), HttpStatus.OK)
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
            value = ["/pet/findByStatus"],
            produces = ["application/xml", "application/json"], 
            method = [RequestMethod.GET])
    fun findPetsByStatus(@NotNull @ApiParam(value = "Status values that need to be considered for filter", required = true, allowableValues = "available, pending, sold", defaultValue = "null") @Valid @RequestParam(value = "status", required = true, defaultValue="null") status: List<String>): ResponseEntity<List<Pet>> {
        return ResponseEntity(service.findPetsByStatus(status), HttpStatus.OK)
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
            value = ["/pet/findByTags"],
            produces = ["application/xml", "application/json"], 
            method = [RequestMethod.GET])
    fun findPetsByTags(@NotNull @ApiParam(value = "Tags to filter by", required = true, defaultValue = "null") @Valid @RequestParam(value = "tags", required = true, defaultValue="null") tags: List<String>): ResponseEntity<List<Pet>> {
        return ResponseEntity(service.findPetsByTags(tags), HttpStatus.OK)
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
            value = ["/pet/{petId}"],
            produces = ["application/xml", "application/json"], 
            method = [RequestMethod.GET])
    fun getPetById(@ApiParam(value = "ID of pet to return", required=true, defaultValue="null") @PathVariable("petId") petId: Long): ResponseEntity<Pet> {
        return ResponseEntity(service.getPetById(petId), HttpStatus.OK)
    }

    @ApiOperation(
            value = "Update an existing pet",
            nickname = "updatePet",
            notes = "",
            authorizations = [Authorization(value = "petstore_auth", scopes = [AuthorizationScope(scope = "write:pets", description = "modify pets in your account"), AuthorizationScope(scope = "read:pets", description = "read your pets")])])
    @ApiResponses(
            value = [ApiResponse(code = 400, message = "Invalid ID supplied"),ApiResponse(code = 404, message = "Pet not found"),ApiResponse(code = 405, message = "Validation exception")])
    @RequestMapping(
            value = ["/pet"],
            consumes = ["application/json", "application/xml"],
            method = [RequestMethod.PUT])
    fun updatePet(@ApiParam(value = "Pet object that needs to be added to the store" ,required=true ) @Valid @RequestBody body: Pet): ResponseEntity<Unit> {
        return ResponseEntity(service.updatePet(body), HttpStatus.OK)
    }

    @ApiOperation(
            value = "Updates a pet in the store with form data",
            nickname = "updatePetWithForm",
            notes = "",
            authorizations = [Authorization(value = "petstore_auth", scopes = [AuthorizationScope(scope = "write:pets", description = "modify pets in your account"), AuthorizationScope(scope = "read:pets", description = "read your pets")])])
    @ApiResponses(
            value = [ApiResponse(code = 405, message = "Invalid input")])
    @RequestMapping(
            value = ["/pet/{petId}"],
            consumes = ["application/x-www-form-urlencoded"],
            method = [RequestMethod.POST])
    fun updatePetWithForm(@ApiParam(value = "ID of pet that needs to be updated", required=true, defaultValue="null") @PathVariable("petId") petId: Long,@ApiParam(value = "Updated name of the pet", defaultValue="null") @RequestParam(value="name", required=false) name: String ,@ApiParam(value = "Updated status of the pet", defaultValue="null") @RequestParam(value="status", required=false) status: String ): ResponseEntity<Unit> {
        return ResponseEntity(service.updatePetWithForm(petId, name, status), HttpStatus.OK)
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
            value = ["/pet/{petId}/uploadImage"],
            produces = ["application/json"], 
            consumes = ["multipart/form-data"],
            method = [RequestMethod.POST])
    fun uploadFile(@ApiParam(value = "ID of pet to update", required=true, defaultValue="null") @PathVariable("petId") petId: Long,@ApiParam(value = "Additional data to pass to server", defaultValue="null") @RequestParam(value="additionalMetadata", required=false) additionalMetadata: String ,@ApiParam(value = "file detail") @Valid @RequestPart("file") file: MultipartFile): ResponseEntity<ModelApiResponse> {
        return ResponseEntity(service.uploadFile(petId, additionalMetadata, file), HttpStatus.OK)
    }
}
