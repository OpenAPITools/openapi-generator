package org.openapitools.api

import org.openapitools.model.ItemWithDollarAttributesAndExamples
import org.openapitools.model.ItemsItemIdSomethingItemSubIdGet200Response
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
class ItemsApiController(@Autowired(required = true) val service: ItemsApiService) {

    @Operation(
        summary = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some",
        operationId = "itemsItemIdSomethingItemSubIdGet",
        description = """SQ = "; SBS = \; DBS = \\; SD = ${'$'}some""",
        responses = [
            ApiResponse(responseCode = "200", description = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some", content = [Content(schema = Schema(implementation = ItemsItemIdSomethingItemSubIdGet200Response::class))]) ]
    )
    @RequestMapping(
        method = [RequestMethod.GET],
        value = [PATH_ITEMS_ITEM_ID_SOMETHING_ITEM_SUB_ID_GET /* "/items/{item$Id}/something/{item$SubId}" */],
        produces = ["application/json"]
    )
    fun itemsItemIdSomethingItemSubIdGet(
        @Parameter(description = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some", required = true) @PathVariable("item\$Id") itemDollarId: kotlin.String,
        @Parameter(description = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some", required = true) @PathVariable("item\$SubId") itemDollarSubId: kotlin.String,
        @Parameter(description = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some", schema = Schema(defaultValue = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some")) @Valid @RequestParam(value = "filter\$Type", required = false, defaultValue = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some") filterDollarType: kotlin.String,
        @Parameter(description = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some", schema = Schema(defaultValue = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some")) @Valid @RequestParam(value = "filter\$SubType", required = false, defaultValue = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some") filterDollarSubType: kotlin.String,
        @Parameter(description = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some", `in` = ParameterIn.HEADER) @RequestHeader(value = "X-Custom_Header", required = false) xCustomHeader: kotlin.String?,
        @Parameter(description = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some", `in` = ParameterIn.HEADER) @RequestHeader(value = "X-Custom_Header_two", required = false) xCustomHeaderTwo: kotlin.String?,
        @CookieValue(name = "session\$Token", required = false) sessionDollarToken: kotlin.String?,
        @CookieValue(name = "session\$TokenTwo", required = false) sessionDollarTokenTwo: kotlin.String?
    ): ResponseEntity<ItemsItemIdSomethingItemSubIdGet200Response> {
        return ResponseEntity(service.itemsItemIdSomethingItemSubIdGet(itemDollarId, itemDollarSubId, filterDollarType, filterDollarSubType, xCustomHeader, xCustomHeaderTwo, sessionDollarToken, sessionDollarTokenTwo), HttpStatus.valueOf(200))
    }

    @Operation(
        summary = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some",
        operationId = "itemsPost",
        description = """SQ = "; SBS = \; DBS = \\; SD = ${'$'}some""",
        responses = [
            ApiResponse(responseCode = "201", description = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some", content = [Content(schema = Schema(implementation = ItemWithDollarAttributesAndExamples::class))]) ]
    )
    @RequestMapping(
        method = [RequestMethod.POST],
        value = [PATH_ITEMS_POST /* "/items" */],
        produces = ["application/json"],
        consumes = ["application/x-www-form-urlencoded"]
    )
    fun itemsPost(
        @Parameter(description = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some", `in` = ParameterIn.HEADER) @RequestHeader(value = "X-Post_Header", required = false) xPostHeader: kotlin.String?,
        @Parameter(description = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some") @Valid @RequestParam(value = "form\$Name", required = false) formDollarName: kotlin.String?,
        @Parameter(description = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some", schema = Schema(defaultValue = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some")) @Valid @RequestParam(value = "form\$Value", required = false) formDollarValue: kotlin.String
    ): ResponseEntity<ItemWithDollarAttributesAndExamples> {
        return ResponseEntity(service.itemsPost(xPostHeader, formDollarName, formDollarValue), HttpStatus.valueOf(201))
    }

    companion object {
        //for your own safety never directly reuse these path definitions in tests
        const val PATH_ITEMS_ITEM_ID_SOMETHING_ITEM_SUB_ID_GET: String = "/items/{item\$Id}/something/{item\$SubId}"
        const val PATH_ITEMS_POST: String = "/items"
    }
}
