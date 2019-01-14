package org.openapitools.api

import org.openapitools.model.SysMailFormData
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
@Api(value = "Default", description = "The Default API")
@RequestMapping("\${api.base-path:/v2}")
class DefaultApiController(@Autowired(required = true) val service: DefaultApiService) {

    @ApiOperation(
            value = "",
            nickname = "updateSysMailAtKey",
            notes = "Update SysMail template")
    @ApiResponses(
            value = [ApiResponse(code = 200, message = "OK")])
    @RequestMapping(
            value = ["/sysmails/{key}"],
            consumes = ["application/json"],
            method = [RequestMethod.PUT])
    fun updateSysMailAtKey(@ApiParam(value = "", required=true, defaultValue="null") @PathVariable("key") key: String,@ApiParam(value = "" ,required=true ) @Valid @RequestBody sysMailFormData: SysMailFormData): ResponseEntity<Unit> {
        return ResponseEntity(service.updateSysMailAtKey(key, sysMailFormData), HttpStatus.OK)
    }
}
