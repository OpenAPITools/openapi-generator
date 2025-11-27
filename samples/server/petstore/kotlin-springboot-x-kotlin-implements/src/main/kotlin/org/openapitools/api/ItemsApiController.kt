package org.openapitools.api

import org.openapitools.model.ItemWithDollarAttributesAndExamples
import org.openapitools.model.ItemsItemIdGet200Response
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
        value = "Retrieve item \"by\" $Id",
        nickname = "itemsItemIdGet",
        notes = "Get an item using the $symbol in \"parameter\" and property names.",
        response = ItemsItemIdGet200Response::class)
    @ApiResponses(
        value = [ApiResponse(code = 200, message = "\"Successful $response\"", response = ItemsItemIdGet200Response::class)])
    @RequestMapping(
        method = [RequestMethod.GET],
        value = [PATH_ITEMS_ITEM_ID_GET /* "/items/{item\$Id}" */],
        produces = ["application/json"]
    )
    fun itemsItemIdGet(
        @ApiParam(value = "The $prefixed \"ID\" of the item.", required = true) @PathVariable("item$Id") itemDollarId: kotlin.String,
        @ApiParam(value = "Optional $filter \"for\" item type.", defaultValue = "\"type\"$ADefault") @Valid @RequestParam(value = "filter$Type", required = false, defaultValue = "\"type\"$ADefault") filterDollarType: kotlin.String,
        @ApiParam(value = "Header param with $") @RequestHeader(value = "X-Custom$Header", required = false) xCustomDollarHeader: kotlin.String?,
        @CookieValue(name = "session$Token", required = false) sessionDollarToken: kotlin.String?,
        @ApiParam(hidden = true) request: javax.servlet.http.HttpServletRequest
    ): ResponseEntity<ItemsItemIdGet200Response> {
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)
    }


    @ApiOperation(
        value = "Create $it\"em\"",
        nickname = "itemsPost",
        notes = "POST using form params and $every\"where\".",
        response = ItemWithDollarAttributesAndExamples::class)
    @ApiResponses(
        value = [ApiResponse(code = 201, message = "$test \"created\"", response = ItemWithDollarAttributesAndExamples::class)])
    @RequestMapping(
        method = [RequestMethod.POST],
        value = [PATH_ITEMS_POST /* "/items" */],
        produces = ["application/json"],
        consumes = ["application/x-www-form-urlencoded"]
    )
    fun itemsPost(
        @ApiParam(value = "Header \"param\" with $") @RequestHeader(value = "X-Post$Header", required = false) xPostDollarHeader: kotlin.String?,
        @ApiParam(value = "Form field with $issue \\\"fdsfsd\\\"") @Valid @RequestParam(value = "form$Name", required = false) formDollarName: kotlin.String?,
        @ApiParam(value = "Another $form \\\"field\\\"") @Valid @RequestParam(value = "form$Value", required = false) formDollarValue: kotlin.String?,
        @ApiParam(hidden = true) request: javax.servlet.http.HttpServletRequest
    ): ResponseEntity<ItemWithDollarAttributesAndExamples> {
        return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)
    }

    companion object {
        //for your own safety never directly reuse these path definitions in tests
        const val BASE_PATH: String = "/v2"
        const val PATH_ITEMS_ITEM_ID_GET: String = "/items/{item$Id}"
        const val PATH_ITEMS_POST: String = "/items"
    }
}
