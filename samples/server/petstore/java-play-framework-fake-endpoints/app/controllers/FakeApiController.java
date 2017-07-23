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
import java.util.ArrayList;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.JsonNode;
import com.google.inject.Inject;
import java.io.IOException;
import swagger.SwaggerUtils;
import com.fasterxml.jackson.core.type.TypeReference;

import javax.validation.constraints.*;

import swagger.SwaggerUtils.ApiAction;


public class FakeApiController extends Controller {

    private final FakeApiControllerImp imp;
    private final ObjectMapper mapper;

    @Inject
    private FakeApiController(FakeApiControllerImp imp) {
        this.imp = imp;
        mapper = new ObjectMapper();
    }


    @ApiAction
    public Result fakeOuterBooleanSerialize() throws Exception {
        JsonNode nodebody = request().body().asJson();
        Boolean body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), Boolean.class);
        
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
        
        } else {
            body = null;
        }
        OuterComposite obj = imp.fakeOuterCompositeSerialize(body);
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
        
    }

    @ApiAction
    public Result fakeOuterNumberSerialize() throws Exception {
        JsonNode nodebody = request().body().asJson();
        BigDecimal body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), BigDecimal.class);
        
        } else {
            body = null;
        }
        BigDecimal obj = imp.fakeOuterNumberSerialize(body);
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
        
    }

    @ApiAction
    public Result fakeOuterStringSerialize() throws Exception {
        JsonNode nodebody = request().body().asJson();
        String body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), String.class);
        
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

        Client obj = imp.testClientModel(body);
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
        
    }

    @ApiAction
    public Result testEndpointParameters() throws Exception {
        String valueinteger = ((String[]) request().body().asMultipartFormData().asFormUrlEncoded().get("integer"))[0];
        Integer integer;
        if (valueinteger != null) {
            integer = Integer.parseInt(valueinteger);
        
        } else {
            integer = 0;
        }
        String valueint32 = ((String[]) request().body().asMultipartFormData().asFormUrlEncoded().get("int32"))[0];
        Integer int32;
        if (valueint32 != null) {
            int32 = Integer.parseInt(valueint32);
        
        } else {
            int32 = 0;
        }
        String valueint64 = ((String[]) request().body().asMultipartFormData().asFormUrlEncoded().get("int64"))[0];
        Long int64;
        if (valueint64 != null) {
            int64 = Long.parseLong(valueint64);
        
        } else {
            int64 = 0L;
        }
        String valuenumber = ((String[]) request().body().asMultipartFormData().asFormUrlEncoded().get("number"))[0];
        BigDecimal number;

        number = Float.parseFloat(valuenumber);

        String value_float = ((String[]) request().body().asMultipartFormData().asFormUrlEncoded().get("float"))[0];
        Float _float;
        if (value_float != null) {
            _float = Float.parseFloat(value_float);
        
        } else {
            _float = 0.0;
        }
        String value_double = ((String[]) request().body().asMultipartFormData().asFormUrlEncoded().get("double"))[0];
        Double _double;

        _double = Double.parseDouble(value_double);

        String valuestring = ((String[]) request().body().asMultipartFormData().asFormUrlEncoded().get("string"))[0];
        String string;
        if (valuestring != null) {
            string = (String)valuestring;
        
        } else {
            string = "";
        }
        String valuepatternWithoutDelimiter = ((String[]) request().body().asMultipartFormData().asFormUrlEncoded().get("pattern_without_delimiter"))[0];
        String patternWithoutDelimiter;

        patternWithoutDelimiter = (String)valuepatternWithoutDelimiter;

        String value_byte = ((String[]) request().body().asMultipartFormData().asFormUrlEncoded().get("byte"))[0];
        byte[] _byte;

        _byte = value_byte;

        String valuebinary = ((String[]) request().body().asMultipartFormData().asFormUrlEncoded().get("binary"))[0];
        byte[] binary;
        if (valuebinary != null) {
            binary = valuebinary;
        
        } else {
            binary = ;
        }
        String valuedate = ((String[]) request().body().asMultipartFormData().asFormUrlEncoded().get("date"))[0];
        LocalDate date;
        if (valuedate != null) {
            date = valuedate;
        
        } else {
            date = ;
        }
        String valuedateTime = ((String[]) request().body().asMultipartFormData().asFormUrlEncoded().get("dateTime"))[0];
        OffsetDateTime dateTime;
        if (valuedateTime != null) {
            dateTime = valuedateTime;
        
        } else {
            dateTime = ;
        }
        String valuepassword = ((String[]) request().body().asMultipartFormData().asFormUrlEncoded().get("password"))[0];
        String password;
        if (valuepassword != null) {
            password = (String)valuepassword;
        
        } else {
            password = "";
        }
        String valueparamCallback = ((String[]) request().body().asMultipartFormData().asFormUrlEncoded().get("callback"))[0];
        String paramCallback;
        if (valueparamCallback != null) {
            paramCallback = (String)valueparamCallback;
        
        } else {
            paramCallback = "";
        }
        imp.testEndpointParameters(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, paramCallback);
        
        return ok();
    }

    @ApiAction
    public Result testEnumParameters() throws Exception {
        //TODO: Support this later
        //List<Pair> enumQueryStringArrayPair = SwaggerUtils.parameterToPairs("csv", "enumQueryStringArray", request().getQueryString("enum_query_string_array"));
        List<String> enumQueryStringArray = new ArrayList<String>();
        //for (Pair pair : enumQueryStringArrayPair) {
        //    enumQueryStringArray.add(pair.getValue());
        //}
        String valueenumQueryString = request().getQueryString("enumQueryString");
        String enumQueryString;
        if (valueenumQueryString != null) {
            enumQueryString = (String)valueenumQueryString;
        
        } else {
            enumQueryString = "";
        }
        String valueenumQueryInteger = request().getQueryString("enumQueryInteger");
        Integer enumQueryInteger;
        if (valueenumQueryInteger != null) {
            enumQueryInteger = Integer.parseInt(valueenumQueryInteger);
        
        } else {
            enumQueryInteger = 0;
        }
        //TODO: Support this later
        //List<Pair> enumFormStringArrayPair = SwaggerUtils.parameterToPairs("csv", "enumFormStringArray", ((String[]) request().body().asMultipartFormData().asFormUrlEncoded().get("enum_form_string_array"))[0]);
        List<String> enumFormStringArray = new ArrayList<String>();
        //for (Pair pair : enumFormStringArrayPair) {
        //    enumFormStringArray.add(pair.getValue());
        //}
        String valueenumFormString = ((String[]) request().body().asMultipartFormData().asFormUrlEncoded().get("enum_form_string"))[0];
        String enumFormString;
        if (valueenumFormString != null) {
            enumFormString = (String)valueenumFormString;
        
        } else {
            enumFormString = "";
        }
        String valueenumQueryDouble = ((String[]) request().body().asMultipartFormData().asFormUrlEncoded().get("enum_query_double"))[0];
        Double enumQueryDouble;
        if (valueenumQueryDouble != null) {
            enumQueryDouble = Double.parseDouble(valueenumQueryDouble);
        
        } else {
            enumQueryDouble = 0.0;
        }
        //TODO: Support this later
        //List<Pair> enumHeaderStringArrayPair = SwaggerUtils.parameterToPairs("csv", "enumHeaderStringArray", request().getHeader("enum_header_string_array"));
        //List<String> enumHeaderStringArray = new ArrayList<String>();
        //for (Pair pair : enumHeaderStringArrayPair) {
        //    enumHeaderStringArray.add(pair.getValue());
        //}
        String valueenumHeaderString = request().getHeader("enum_header_string");
        String enumHeaderString;
        if (valueenumHeaderString != null) {
            enumHeaderString = (String)valueenumHeaderString;
        
        } else {
            enumHeaderString = "";
        }
        imp.testEnumParameters(enumFormStringArray, enumFormString, enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble);
        
        return ok();
    }

    @ApiAction
    public Result testJsonFormData() throws Exception {
        String valueparam = ((String[]) request().body().asMultipartFormData().asFormUrlEncoded().get("param"))[0];
        String param;

        param = (String)valueparam;

        String valueparam2 = ((String[]) request().body().asMultipartFormData().asFormUrlEncoded().get("param2"))[0];
        String param2;

        param2 = (String)valueparam2;

        imp.testJsonFormData(param, param2);
        
        return ok();
    }
}
