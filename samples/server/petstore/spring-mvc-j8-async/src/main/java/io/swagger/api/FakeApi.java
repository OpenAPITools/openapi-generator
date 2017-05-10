package io.swagger.api;

import java.math.BigDecimal;
import io.swagger.model.Client;
import java.time.LocalDate;
import java.time.OffsetDateTime;

import io.swagger.annotations.*;
import org.springframework.http.HttpStatus;
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
import java.util.concurrent.CompletableFuture;
import javax.validation.constraints.*;
import javax.validation.Valid;

@Api(value = "fake", description = "the fake API")
public interface FakeApi {

    @ApiOperation(value = "To test \"client\" model", notes = "To test \"client\" model", response = Client.class, tags={ "fake", })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Client.class) })
    
    @RequestMapping(value = "/fake",
        produces = { "application/json" }, 
        consumes = { "application/json" },
        method = RequestMethod.PATCH)
    default CompletableFuture<ResponseEntity<Client>> testClientModel(@ApiParam(value = "client model" ,required=true )  @Valid @RequestBody Client body) {
        // do some magic!
        return CompletableFuture.completedFuture(new ResponseEntity<Client>(HttpStatus.OK));
    }


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
    default CompletableFuture<ResponseEntity<Void>> testEndpointParameters(@ApiParam(value = "None", required=true) @RequestPart(value="number", required=true)  BigDecimal number,@ApiParam(value = "None", required=true) @RequestPart(value="double", required=true)  Double _double,@ApiParam(value = "None", required=true) @RequestPart(value="pattern_without_delimiter", required=true)  String patternWithoutDelimiter,@ApiParam(value = "None", required=true) @RequestPart(value="byte", required=true)  byte[] _byte,@ApiParam(value = "None") @RequestPart(value="integer", required=false)  Integer integer,@ApiParam(value = "None") @RequestPart(value="int32", required=false)  Integer int32,@ApiParam(value = "None") @RequestPart(value="int64", required=false)  Long int64,@ApiParam(value = "None") @RequestPart(value="float", required=false)  Float _float,@ApiParam(value = "None") @RequestPart(value="string", required=false)  String string,@ApiParam(value = "None") @RequestPart(value="binary", required=false)  byte[] binary,@ApiParam(value = "None") @RequestPart(value="date", required=false)  LocalDate date,@ApiParam(value = "None") @RequestPart(value="dateTime", required=false)  OffsetDateTime dateTime,@ApiParam(value = "None") @RequestPart(value="password", required=false)  String password,@ApiParam(value = "None") @RequestPart(value="callback", required=false)  String paramCallback) {
        // do some magic!
        return CompletableFuture.completedFuture(new ResponseEntity<Void>(HttpStatus.OK));
    }


    @ApiOperation(value = "To test enum parameters", notes = "To test enum parameters", response = Void.class, tags={ "fake", })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid request", response = Void.class),
        @ApiResponse(code = 404, message = "Not found", response = Void.class) })
    
    @RequestMapping(value = "/fake",
        produces = { "*/*" }, 
        consumes = { "*/*" },
        method = RequestMethod.GET)
    default CompletableFuture<ResponseEntity<Void>> testEnumParameters(@ApiParam(value = "Form parameter enum test (string array)", allowableValues=">, $") @RequestPart(value="enum_form_string_array", required=false)  List<String> enumFormStringArray,@ApiParam(value = "Form parameter enum test (string)", allowableValues="_abc, -efg, (xyz)", defaultValue="-efg") @RequestPart(value="enum_form_string", required=false)  String enumFormString,@ApiParam(value = "Header parameter enum test (string array)" , allowableValues=">, $") @RequestHeader(value="enum_header_string_array", required=false) List<String> enumHeaderStringArray,@ApiParam(value = "Header parameter enum test (string)" , allowableValues="_abc, -efg, (xyz)", defaultValue="-efg") @RequestHeader(value="enum_header_string", required=false) String enumHeaderString,@ApiParam(value = "Query parameter enum test (string array)", allowableValues = ">, $") @RequestParam(value = "enum_query_string_array", required = false) List<String> enumQueryStringArray,@ApiParam(value = "Query parameter enum test (string)", allowableValues = "_abc, -efg, (xyz)", defaultValue = "-efg") @RequestParam(value = "enum_query_string", required = false, defaultValue="-efg") String enumQueryString,@ApiParam(value = "Query parameter enum test (double)", allowableValues = "1, -2") @RequestParam(value = "enum_query_integer", required = false) Integer enumQueryInteger,@ApiParam(value = "Query parameter enum test (double)", allowableValues="1.1, -1.2") @RequestPart(value="enum_query_double", required=false)  Double enumQueryDouble) {
        // do some magic!
        return CompletableFuture.completedFuture(new ResponseEntity<Void>(HttpStatus.OK));
    }

}
