package io.swagger.api;

import java.math.BigDecimal;
import io.swagger.model.Client;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import io.swagger.model.OuterComposite;

import io.swagger.annotations.*;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

import javax.validation.constraints.*;
import javax.validation.Valid;

@Controller
public class FakeApiController implements FakeApi {
    private final FakeApiDelegate delegate;

    @org.springframework.beans.factory.annotation.Autowired
    public FakeApiController(FakeApiDelegate delegate) {
        this.delegate = delegate;
    }


    public ResponseEntity<Boolean> fakeOuterBooleanSerialize(@ApiParam(value = "Input boolean as post body"  )  @Valid @RequestBody Boolean body) {
        // do some magic!
        return delegate.fakeOuterBooleanSerialize(body);
    }

    public ResponseEntity<OuterComposite> fakeOuterCompositeSerialize(@ApiParam(value = "Input composite as post body"  )  @Valid @RequestBody OuterComposite body) {
        // do some magic!
        return delegate.fakeOuterCompositeSerialize(body);
    }

    public ResponseEntity<BigDecimal> fakeOuterNumberSerialize(@ApiParam(value = "Input number as post body"  )  @Valid @RequestBody BigDecimal body) {
        // do some magic!
        return delegate.fakeOuterNumberSerialize(body);
    }

    public ResponseEntity<String> fakeOuterStringSerialize(@ApiParam(value = "Input string as post body"  )  @Valid @RequestBody String body) {
        // do some magic!
        return delegate.fakeOuterStringSerialize(body);
    }

    public ResponseEntity<Client> testClientModel(@ApiParam(value = "client model" ,required=true )  @Valid @RequestBody Client body) {
        // do some magic!
        return delegate.testClientModel(body);
    }

    public ResponseEntity<Void> testEndpointParameters(@ApiParam(value = "None", required=true) @RequestPart(value="number", required=true)  BigDecimal number,
        @ApiParam(value = "None", required=true) @RequestPart(value="double", required=true)  Double _double,
        @ApiParam(value = "None", required=true) @RequestPart(value="pattern_without_delimiter", required=true)  String patternWithoutDelimiter,
        @ApiParam(value = "None", required=true) @RequestPart(value="byte", required=true)  byte[] _byte,
        @ApiParam(value = "None") @RequestPart(value="integer", required=false)  Integer integer,
        @ApiParam(value = "None") @RequestPart(value="int32", required=false)  Integer int32,
        @ApiParam(value = "None") @RequestPart(value="int64", required=false)  Long int64,
        @ApiParam(value = "None") @RequestPart(value="float", required=false)  Float _float,
        @ApiParam(value = "None") @RequestPart(value="string", required=false)  String string,
        @ApiParam(value = "None") @RequestPart(value="binary", required=false)  byte[] binary,
        @ApiParam(value = "None") @RequestPart(value="date", required=false)  LocalDate date,
        @ApiParam(value = "None") @RequestPart(value="dateTime", required=false)  OffsetDateTime dateTime,
        @ApiParam(value = "None") @RequestPart(value="password", required=false)  String password,
        @ApiParam(value = "None") @RequestPart(value="callback", required=false)  String paramCallback) {
        // do some magic!
        return delegate.testEndpointParameters(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, paramCallback);
    }

    public ResponseEntity<Void> testEnumParameters(@ApiParam(value = "Form parameter enum test (string array)", allowableValues=">, $") @RequestPart(value="enum_form_string_array", required=false)  List<String> enumFormStringArray,
        @ApiParam(value = "Form parameter enum test (string)", allowableValues="_abc, -efg, (xyz)", defaultValue="-efg") @RequestPart(value="enum_form_string", required=false)  String enumFormString,
        @ApiParam(value = "Header parameter enum test (string array)" , allowableValues=">, $") @RequestHeader(value="enum_header_string_array", required=false) List<String> enumHeaderStringArray,
        @ApiParam(value = "Header parameter enum test (string)" , allowableValues="_abc, -efg, (xyz)", defaultValue="-efg") @RequestHeader(value="enum_header_string", required=false) String enumHeaderString,
        @ApiParam(value = "Query parameter enum test (string array)", allowableValues = ">, $") @RequestParam(value = "enum_query_string_array", required = false) List<String> enumQueryStringArray,
        @ApiParam(value = "Query parameter enum test (string)", allowableValues = "_abc, -efg, (xyz)", defaultValue = "-efg") @RequestParam(value = "enum_query_string", required = false, defaultValue="-efg") String enumQueryString,
        @ApiParam(value = "Query parameter enum test (double)", allowableValues = "1, -2") @RequestParam(value = "enum_query_integer", required = false) Integer enumQueryInteger,
        @ApiParam(value = "Query parameter enum test (double)", allowableValues="1.1, -1.2") @RequestPart(value="enum_query_double", required=false)  Double enumQueryDouble) {
        // do some magic!
        return delegate.testEnumParameters(enumFormStringArray, enumFormString, enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble);
    }

}
