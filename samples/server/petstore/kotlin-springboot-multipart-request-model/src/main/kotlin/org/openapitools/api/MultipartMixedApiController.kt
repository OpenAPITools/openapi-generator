package org.openapitools.api

import org.openapitools.model.MultipartMixedRequestMarker
import org.openapitools.model.MultipartMixedStatus
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
@RequestMapping("\${api.base-path:}")
class MultipartMixedApiController() {

    @Operation(
        summary = "",
        operationId = "multipartMixed",
        description = """Mixed MultipartFile test""",
        responses = [
            ApiResponse(responseCode = "204", description = "Successful operation") ]
    )
    @RequestMapping(
        method = [RequestMethod.POST],
        value = ["/multipart-mixed"],
        consumes = ["multipart/form-data"]
    )
    fun multipartMixed(@Parameter(description = "", required = true, schema = Schema(allowableValues = ["ALLOWED", "IN_PROGRESS", "REJECTED"])) @Valid @RequestParam(value = "status", required = true) status: MultipartMixedStatus ,@Parameter(description = "a file") @Valid @RequestPart("file", required = true) file: org.springframework.web.multipart.MultipartFile,@Parameter(description = "") @Valid @RequestPart(value = "marker", required = false) marker: MultipartMixedRequestMarker? ): ResponseEntity<Unit> {
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)
    }
}
