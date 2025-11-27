package org.openapitools.api

import org.openapitools.model.ItemWithDollarAttributesAndExamples
import org.openapitools.model.ItemsItemIdSomethingItemSubIdGet200Response
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
import org.openapitools.api.ItemsApiController.Companion.BASE_PATH

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
@Api(value = "items", description = "The items API")
@RequestMapping("\${openapi.openAPIPetstore.base-path:\${api.base-path:$BASE_PATH}}")
class ItemsApiController() {


    @ApiOperation(
        value = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some",
        nickname = "itemsItemIdSomethingItemSubIdGet",
        notes = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some",
        response = ItemsItemIdSomethingItemSubIdGet200Response::class)
    @ApiResponses(
        value = [ApiResponse(code = 200, message = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some", response = ItemsItemIdSomethingItemSubIdGet200Response::class)])
    @RequestMapping(
        method = [RequestMethod.GET],
        value = [PATH_ITEMS_ITEM_ID_SOMETHING_ITEM_SUB_ID_GET /* "/items/{item$Id}/something/{item$SubId}" */],
        produces = ["application/json"]
    )
    fun itemsItemIdSomethingItemSubIdGet(
        @ApiParam(value = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some", required = true) @PathVariable("item\$Id") itemDollarId: kotlin.String,
        @ApiParam(value = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some", required = true) @PathVariable("item\$SubId") itemDollarSubId: kotlin.String,
        @ApiParam(value = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some", defaultValue = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some") @Valid @RequestParam(value = "filter\$Type", required = false, defaultValue = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some") filterDollarType: kotlin.String,
        @ApiParam(value = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some", defaultValue = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some") @Valid @RequestParam(value = "filter\$SubType", required = false, defaultValue = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some") filterDollarSubType: kotlin.String,
        @ApiParam(value = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some") @RequestHeader(value = "X-Custom_Header", required = false) xCustomHeader: kotlin.String?,
        @ApiParam(value = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some") @RequestHeader(value = "X-Custom_Header_two", required = false) xCustomHeaderTwo: kotlin.String?,
        @CookieValue(name = "session$Token", required = false) sessionDollarToken: kotlin.String?,
        @CookieValue(name = "session$TokenTwo", required = false) sessionDollarTokenTwo: kotlin.String?,
        @ApiParam(hidden = true) request: javax.servlet.http.HttpServletRequest
    ): ResponseEntity<ItemsItemIdSomethingItemSubIdGet200Response> {
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)
    }


    @ApiOperation(
        value = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some",
        nickname = "itemsPost",
        notes = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some",
        response = ItemWithDollarAttributesAndExamples::class)
    @ApiResponses(
        value = [ApiResponse(code = 201, message = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some", response = ItemWithDollarAttributesAndExamples::class)])
    @RequestMapping(
        method = [RequestMethod.POST],
        value = [PATH_ITEMS_POST /* "/items" */],
        produces = ["application/json"],
        consumes = ["application/x-www-form-urlencoded"]
    )
    fun itemsPost(
        @ApiParam(value = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some") @RequestHeader(value = "X-Post_Header", required = false) xPostHeader: kotlin.String?,
        @ApiParam(value = "SQ = \\\"; SBS = \\\\; DBS = \\\\\\\\; SD = $some") @Valid @RequestParam(value = "form$Name", required = false) formDollarName: kotlin.String?,
        @ApiParam(value = "SQ = \\\"; SBS = \\\\; DBS = \\\\\\\\; SD = $some", defaultValue = ""SQ = \"; SBS = \\; DBS = \\\\; SD = $some"") @Valid @RequestParam(value = "form$Value", required = false) formDollarValue: kotlin.String,
        @ApiParam(hidden = true) request: javax.servlet.http.HttpServletRequest
    ): ResponseEntity<ItemWithDollarAttributesAndExamples> {
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)
    }

    companion object {
        //for your own safety never directly reuse these path definitions in tests
        const val BASE_PATH: String = "/v2"
        const val PATH_ITEMS_ITEM_ID_SOMETHING_ITEM_SUB_ID_GET: String = "/items/{item\$Id}/something/{item\$SubId}"
        const val PATH_ITEMS_POST: String = "/items"
    }
}
