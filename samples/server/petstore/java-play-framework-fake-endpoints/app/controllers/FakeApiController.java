package controllers;

import java.math.BigDecimal;
import apimodels.Client;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import apimodels.OuterComposite;

import play.mvc.Controller;
import play.mvc.Result;
import play.mvc.Http;
import java.util.List;
import java.util.Map;
import java.util.ArrayList;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.JsonNode;
import com.google.inject.Inject;
import java.io.File;
import swagger.SwaggerUtils;
import com.fasterxml.jackson.core.type.TypeReference;

import javax.validation.constraints.*;

import swagger.SwaggerUtils.ApiAction;


public class FakeApiController extends Controller {

    private final FakeApiControllerImpInterface imp;
    private final ObjectMapper mapper;

    @Inject
    private FakeApiController(FakeApiControllerImpInterface imp) {
        this.imp = imp;
        mapper = new ObjectMapper();
    }


    @ApiAction
    public Result fakeOuterBooleanSerialize() throws Exception {
        JsonNode nodebody = request().body().asJson();
        Boolean body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), Boolean.class);
        
        body.validate();
        } else {
            body = null;
        }
        Boolean obj = imp.fakeOuterBooleanSerialize(body);
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
    }

    @ApiAction
    public Result fakeOuterCompositeSerialize() throws Exception {
        JsonNode nodebody = request().body().asJson();
        OuterComposite body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), OuterComposite.class);
        
        body.validate();
        } else {
            body = null;
        }
        OuterComposite obj = imp.fakeOuterCompositeSerialize(body);
        obj.validate();
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
    }

    @ApiAction
    public Result fakeOuterNumberSerialize() throws Exception {
        JsonNode nodebody = request().body().asJson();
        BigDecimal body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), BigDecimal.class);
        
        body.validate();
        } else {
            body = null;
        }
        BigDecimal obj = imp.fakeOuterNumberSerialize(body);
        obj.validate();
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
    }

    @ApiAction
    public Result fakeOuterStringSerialize() throws Exception {
        JsonNode nodebody = request().body().asJson();
        String body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), String.class);
        
        body.validate();
        } else {
            body = null;
        }
        String obj = imp.fakeOuterStringSerialize(body);
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
    }

    @ApiAction
    public Result testClientModel() throws Exception {
        JsonNode nodebody = request().body().asJson();
        Client body;

        body = mapper.readValue(nodebody.toString(), Client.class);
        body.validate();

        Client obj = imp.testClientModel(body);
        obj.validate();
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
    }

    @ApiAction
    public Result testEndpointParameters() throws Exception {
        String valueinteger = (request().body().asMultipartFormData().asFormUrlEncoded().get("integer"))[0];
        Integer integer;
        if (valueinteger != null) {
            integer = Integer.parseInt(valueinteger);
        
        } else {
            integer = null;
        }
        String valueint32 = (request().body().asMultipartFormData().asFormUrlEncoded().get("int32"))[0];
        Integer int32;
        if (valueint32 != null) {
            int32 = Integer.parseInt(valueint32);
        
        } else {
            int32 = null;
        }
        String valueint64 = (request().body().asMultipartFormData().asFormUrlEncoded().get("int64"))[0];
        Long int64;
        if (valueint64 != null) {
            int64 = Long.parseLong(valueint64);
        
        } else {
            int64 = null;
        }
        String valuenumber = (request().body().asMultipartFormData().asFormUrlEncoded().get("number"))[0];
        BigDecimal number;

        number = Float.parseFloat(valuenumber);

        String value_float = (request().body().asMultipartFormData().asFormUrlEncoded().get("float"))[0];
        Float _float;
        if (value_float != null) {
            _float = Float.parseFloat(value_float);
        
        } else {
            _float = null;
        }
        String value_double = (request().body().asMultipartFormData().asFormUrlEncoded().get("double"))[0];
        Double _double;

        _double = Double.parseDouble(value_double);

        String valuestring = (request().body().asMultipartFormData().asFormUrlEncoded().get("string"))[0];
        String string;
        if (valuestring != null) {
            string = valuestring;
        
        } else {
            string = null;
        }
        String valuepatternWithoutDelimiter = (request().body().asMultipartFormData().asFormUrlEncoded().get("pattern_without_delimiter"))[0];
        String patternWithoutDelimiter;

        patternWithoutDelimiter = valuepatternWithoutDelimiter;

        String value_byte = (request().body().asMultipartFormData().asFormUrlEncoded().get("byte"))[0];
        byte[] _byte;

        _byte = value_byte;

        String valuebinary = (request().body().asMultipartFormData().asFormUrlEncoded().get("binary"))[0];
        byte[] binary;
        if (valuebinary != null) {
            binary = valuebinary;
        
        } else {
            binary = null;
        }
        String valuedate = (request().body().asMultipartFormData().asFormUrlEncoded().get("date"))[0];
        LocalDate date;
        if (valuedate != null) {
            date = valuedate;
        
        } else {
            date = null;
        }
        String valuedateTime = (request().body().asMultipartFormData().asFormUrlEncoded().get("dateTime"))[0];
        OffsetDateTime dateTime;
        if (valuedateTime != null) {
            dateTime = OffsetDateTime.parse(valuedateTime);
        
        } else {
            dateTime = null;
        }
        String valuepassword = (request().body().asMultipartFormData().asFormUrlEncoded().get("password"))[0];
        String password;
        if (valuepassword != null) {
            password = valuepassword;
        
        } else {
            password = null;
        }
        String valueparamCallback = (request().body().asMultipartFormData().asFormUrlEncoded().get("callback"))[0];
        String paramCallback;
        if (valueparamCallback != null) {
            paramCallback = valueparamCallback;
        
        } else {
            paramCallback = null;
        }
        imp.testEndpointParameters(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, paramCallback);
        return ok();
    }

    @ApiAction
    public Result testEnumParameters() throws Exception {
        List<String> enumQueryStringArrayList = SwaggerUtils.parametersToList("csv", request().queryString().get("enum_query_string_array"));
        List<String> enumQueryStringArray = new ArrayList<String>();
        for (String curParam : enumQueryStringArrayList) {
            //noinspection UseBulkOperation
            enumQueryStringArray.add(curParam);
        }
        String valueenumQueryString = request().getQueryString("enumQueryString");
        String enumQueryString;
        if (valueenumQueryString != null) {
            enumQueryString = valueenumQueryString;
        
        } else {
            enumQueryString = "-efg";
        }
        String valueenumQueryInteger = request().getQueryString("enumQueryInteger");
        Integer enumQueryInteger;
        if (valueenumQueryInteger != null) {
            enumQueryInteger = Integer.parseInt(valueenumQueryInteger);
        
        } else {
            enumQueryInteger = null;
        }
        List<String> enumFormStringArrayList = SwaggerUtils.parametersToList("csv", request().body().asMultipartFormData().asFormUrlEncoded().get("enum_form_string_array"));
        List<String> enumFormStringArray = new ArrayList<String>();
        for (String curParam : enumFormStringArrayList) {
            //noinspection UseBulkOperation
            enumFormStringArray.add(curParam);
        }
        String valueenumFormString = (request().body().asMultipartFormData().asFormUrlEncoded().get("enum_form_string"))[0];
        String enumFormString;
        if (valueenumFormString != null) {
            enumFormString = valueenumFormString;
        
        } else {
            enumFormString = "-efg";
        }
        String valueenumQueryDouble = (request().body().asMultipartFormData().asFormUrlEncoded().get("enum_query_double"))[0];
        Double enumQueryDouble;
        if (valueenumQueryDouble != null) {
            enumQueryDouble = Double.parseDouble(valueenumQueryDouble);
        
        } else {
            enumQueryDouble = null;
        }
        List<String> enumHeaderStringArrayList = SwaggerUtils.parametersToList("csv", request().headers().get("enum_header_string_array"));
        List<String> enumHeaderStringArray = new ArrayList<String>();
        for (String curParam : enumHeaderStringArrayList) {
            //noinspection UseBulkOperation
            enumHeaderStringArray.add(curParam);
        }
        String valueenumHeaderString = request().getHeader("enum_header_string");
        String enumHeaderString;
        if (valueenumHeaderString != null) {
            enumHeaderString = valueenumHeaderString;
        
        } else {
            enumHeaderString = "-efg";
        }
        imp.testEnumParameters(enumFormStringArray, enumFormString, enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble);
        return ok();
    }

    @ApiAction
    public Result testJsonFormData() throws Exception {
        String valueparam = (request().body().asMultipartFormData().asFormUrlEncoded().get("param"))[0];
        String param;

        param = valueparam;

        String valueparam2 = (request().body().asMultipartFormData().asFormUrlEncoded().get("param2"))[0];
        String param2;

        param2 = valueparam2;

        imp.testJsonFormData(param, param2);
        return ok();
    }
}
