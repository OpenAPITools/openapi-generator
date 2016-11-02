package io.swagger.api;

import io.swagger.model.Client;
import org.joda.time.LocalDate;
import org.joda.time.DateTime;
import java.math.BigDecimal;

import io.swagger.annotations.*;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;


@Api(value = "fake", description = "the fake API")
public interface FakeApi {

    @ApiOperation(value = "To test \"client\" model", notes = "", response = Client.class, tags={ "fake", })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Client.class) })
    @RequestMapping(value = "/fake",
        produces = { "application/json" }, 
        consumes = { "application/json" },
        method = RequestMethod.PATCH)
    ResponseEntity<Client> testClientModel(@ApiParam(value = "client model" ,required=true ) @RequestBody Client body);


    @ApiOperation(value = "Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 ", notes = "Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 ", response = Void.class, authorizations = {
        @Authorization(value = "http_basic_test")
    }, tags={ "fake", })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid username supplied", response = Void.class),
        @ApiResponse(code = 404, message = "User not found", response = Void.class) })
    @RequestMapping(value = "/fake",
        produces = { "application/xml; charset=utf-8", "application/json; charset=utf-8" }, 
        consumes = { "application/xml; charset=utf-8", "application/json; charset=utf-8" },
        method = RequestMethod.POST)
    ResponseEntity<Void> testEndpointParameters(@ApiParam(value = "None", required=true ) @RequestPart(value="number", required=true)  BigDecimal number,
        @ApiParam(value = "None", required=true ) @RequestPart(value="_double", required=true)  Double _double,
        @ApiParam(value = "None", required=true ) @RequestPart(value="patternWithoutDelimiter", required=true)  String patternWithoutDelimiter,
        @ApiParam(value = "None", required=true ) @RequestPart(value="_byte", required=true)  byte[] _byte,
        @ApiParam(value = "None" ) @RequestPart(value="integer", required=false)  Integer integer,
        @ApiParam(value = "None" ) @RequestPart(value="int32", required=false)  Integer int32,
        @ApiParam(value = "None" ) @RequestPart(value="int64", required=false)  Long int64,
        @ApiParam(value = "None" ) @RequestPart(value="_float", required=false)  Float _float,
        @ApiParam(value = "None" ) @RequestPart(value="string", required=false)  String string,
        @ApiParam(value = "None" ) @RequestPart(value="binary", required=false)  byte[] binary,
        @ApiParam(value = "None" ) @RequestPart(value="date", required=false)  LocalDate date,
        @ApiParam(value = "None" ) @RequestPart(value="dateTime", required=false)  DateTime dateTime,
        @ApiParam(value = "None" ) @RequestPart(value="password", required=false)  String password,
        @ApiParam(value = "None" ) @RequestPart(value="paramCallback", required=false)  String paramCallback);


    @ApiOperation(value = "To test enum parameters", notes = "", response = Void.class, tags={ "fake", })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid request", response = Void.class),
        @ApiResponse(code = 404, message = "Not found", response = Void.class) })
    @RequestMapping(value = "/fake",
        produces = { "application/json" }, 
        consumes = { "application/json" },
        method = RequestMethod.GET)
    ResponseEntity<Void> testEnumParameters(@ApiParam(value = "Form parameter enum test (string array)" , allowableValues="GREATER_THAN, DOLLAR") @RequestPart(value="enumFormStringArray", required=false)  List<String> enumFormStringArray,
        @ApiParam(value = "Form parameter enum test (string)" , allowableValues="_ABC, _EFG, _XYZ_", defaultValue="-efg") @RequestPart(value="enumFormString", required=false)  String enumFormString,
        @ApiParam(value = "Header parameter enum test (string array)"  , allowableValues="GREATER_THAN, DOLLAR") @RequestHeader(value="enum_header_string_array", required=false) List<String> enumHeaderStringArray,
        @ApiParam(value = "Header parameter enum test (string)"  , allowableValues="_ABC, _EFG, _XYZ_", defaultValue="-efg") @RequestHeader(value="enum_header_string", required=false) String enumHeaderString,
        @ApiParam(value = "Query parameter enum test (string array)", allowableValues = "GREATER_THAN, DOLLAR") @RequestParam(value = "enumQueryStringArray", required = false) List<String> enumQueryStringArray,
        @ApiParam(value = "Query parameter enum test (string)", allowableValues = "_ABC, _EFG, _XYZ_", defaultValue = "-efg") @RequestParam(value = "enumQueryString", required = false, defaultValue="-efg") String enumQueryString,
        @ApiParam(value = "Query parameter enum test (double)") @RequestParam(value = "enumQueryInteger", required = false) BigDecimal enumQueryInteger,
        @ApiParam(value = "Query parameter enum test (double)" ) @RequestPart(value="enumQueryDouble", required=false)  Double enumQueryDouble);

}
