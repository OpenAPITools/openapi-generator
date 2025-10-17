package org.openapitools.api

import org.openapitools.model.Annotation
import io.swagger.v3.oas.annotations.*
import io.swagger.v3.oas.annotations.enums.*
import io.swagger.v3.oas.annotations.media.*
import io.swagger.v3.oas.annotations.responses.*
import io.swagger.v3.oas.annotations.security.*
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
@RequestMapping("\${api.base-path:/v2}")
class FakeApiController() {

    @Operation(
        summary = "annotate",
        operationId = "annotations",
        description = """""",
        responses = [
            ApiResponse(responseCode = "200", description = "OK") ]
    )
    @RequestMapping(
        method = [RequestMethod.POST],
        value = ["/fake/annotations"],
        consumes = ["application/json"]
    )
    fun annotations(@Parameter(description = "", required = true) @Valid @RequestBody `annotation`: Annotation): ResponseEntity<Unit> {
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)
    }

    @Operation(
        summary = "Updates a pet in the store with form data (number)",
        operationId = "updatePetWithFormNumber",
        description = """""",
        responses = [
            ApiResponse(responseCode = "405", description = "Invalid input") ],
        security = [ SecurityRequirement(name = "petstore_auth", scopes = [ "write:pets", "read:pets" ]) ]
    )
    @RequestMapping(
        method = [RequestMethod.PUT],
        value = ["/fake/annotations"],
        consumes = ["application/x-www-form-urlencoded"]
    )
    fun updatePetWithFormNumber(@Parameter(description = "ID of pet that needs to be updated", required = true) @PathVariable("petId") petId: kotlin.Long,@Parameter(description = "Updated name of the pet") @Valid @RequestParam(value = "name", required = false) name: kotlin.String? ,@Parameter(description = "integer type") @Valid @RequestParam(value = "status", required = false) status: kotlin.Int? ,@Parameter(description = "number type") @Valid @RequestParam(value = "status2", required = false) status2: java.math.BigDecimal? ): ResponseEntity<Unit> {
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)
    }
}
