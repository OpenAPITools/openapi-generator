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
import play.Configuration;

import swagger.SwaggerUtils.ApiAction;


public class FakeApiController extends Controller {

    private final FakeApiControllerImpInterface imp;
    private final ObjectMapper mapper;
    private final Configuration configuration;

    @Inject
    private FakeApiController(Configuration configuration, FakeApiControllerImpInterface imp) {
        this.imp = imp;
        mapper = new ObjectMapper();
        this.configuration = configuration;
    }


    @ApiAction
    public Result fakeOuterBooleanSerialize() throws Exception {
        JsonNode nodebody = request().body().asJson();
        Boolean body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), Boolean.class);
            if (configuration.getBoolean("useInputBeanValidation")) {
                SwaggerUtils.validate(body);
            }
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
            if (configuration.getBoolean("useInputBeanValidation")) {
                SwaggerUtils.validate(body);
            }
        } else {
            body = null;
        }
        OuterComposite obj = imp.fakeOuterCompositeSerialize(body);
        if (configuration.getBoolean("useOutputBeanValidation")) {
            SwaggerUtils.validate(obj);
        }
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
    }

    @ApiAction
    public Result fakeOuterNumberSerialize() throws Exception {
        JsonNode nodebody = request().body().asJson();
        BigDecimal body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), BigDecimal.class);
            if (configuration.getBoolean("useInputBeanValidation")) {
                SwaggerUtils.validate(body);
            }
        } else {
            body = null;
        }
        BigDecimal obj = imp.fakeOuterNumberSerialize(body);
        if (configuration.getBoolean("useOutputBeanValidation")) {
            SwaggerUtils.validate(obj);
        }
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
    }

    @ApiAction
    public Result fakeOuterStringSerialize() throws Exception {
        JsonNode nodebody = request().body().asJson();
        String body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), String.class);
            if (configuration.getBoolean("useInputBeanValidation")) {
                SwaggerUtils.validate(body);
            }
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
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), Client.class);
            if (configuration.getBoolean("useInputBeanValidation")) {
                SwaggerUtils.validate(body);
            }
        } else {
            throw new IllegalArgumentException("'body' parameter is required");
        }
        Client obj = imp.testClientModel(body);
        if (configuration.getBoolean("useOutputBeanValidation")) {
            SwaggerUtils.validate(obj);
        }
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
        if (valuenumber != null) {
            number = new BigDecimal(valuenumber);
        } else {
            throw new IllegalArgumentException("'number' parameter is required");
        }
        String value_float = (request().body().asMultipartFormData().asFormUrlEncoded().get("float"))[0];
        Float _float;
        if (value_float != null) {
            _float = Float.parseFloat(value_float);
        } else {
            _float = null;
        }
        String value_double = (request().body().asMultipartFormData().asFormUrlEncoded().get("double"))[0];
        Double _double;
        if (value_double != null) {
            _double = Double.parseDouble(value_double);
        } else {
            throw new IllegalArgumentException("'double' parameter is required");
        }
        String valuestring = (request().body().asMultipartFormData().asFormUrlEncoded().get("string"))[0];
        String string;
        if (valuestring != null) {
            string = valuestring;
        } else {
            string = null;
        }
        String valuepatternWithoutDelimiter = (request().body().asMultipartFormData().asFormUrlEncoded().get("pattern_without_delimiter"))[0];
        String patternWithoutDelimiter;
        if (valuepatternWithoutDelimiter != null) {
            patternWithoutDelimiter = valuepatternWithoutDelimiter;
        } else {
            throw new IllegalArgumentException("'pattern_without_delimiter' parameter is required");
        }
        String value_byte = (request().body().asMultipartFormData().asFormUrlEncoded().get("byte"))[0];
        byte[] _byte;
        if (value_byte != null) {
            _byte = value_byte.getBytes();
        } else {
            throw new IllegalArgumentException("'byte' parameter is required");
        }
        String valuebinary = (request().body().asMultipartFormData().asFormUrlEncoded().get("binary"))[0];
        byte[] binary;
        if (valuebinary != null) {
            binary = valuebinary.getBytes();
        } else {
            binary = null;
        }
        String valuedate = (request().body().asMultipartFormData().asFormUrlEncoded().get("date"))[0];
        LocalDate date;
        if (valuedate != null) {
            date = LocalDate.parse(valuedate);
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
        String[] enumQueryStringArrayArray = request().queryString().get("enum_query_string_array");
        List<String> enumQueryStringArrayList = SwaggerUtils.parametersToList("csv", enumQueryStringArrayArray);
        List<String> enumQueryStringArray = new ArrayList<String>();
        for (String curParam : enumQueryStringArrayList) {
            if (!curParam.isEmpty()) {
                //noinspection UseBulkOperation
                enumQueryStringArray.add(curParam);
            }
        }
        String valueenumQueryString = request().getQueryString("enum_query_string");
        String enumQueryString;
        if (valueenumQueryString != null) {
            enumQueryString = valueenumQueryString;
        } else {
            enumQueryString = "-efg";
        }
        String valueenumQueryInteger = request().getQueryString("enum_query_integer");
        Integer enumQueryInteger;
        if (valueenumQueryInteger != null) {
            enumQueryInteger = Integer.parseInt(valueenumQueryInteger);
        } else {
            enumQueryInteger = null;
        }
        String[] enumFormStringArrayArray = request().body().asMultipartFormData().asFormUrlEncoded().get("enum_form_string_array");
        List<String> enumFormStringArrayList = SwaggerUtils.parametersToList("csv", enumFormStringArrayArray);
        List<String> enumFormStringArray = new ArrayList<String>();
        for (String curParam : enumFormStringArrayList) {
            if (!curParam.isEmpty()) {
                //noinspection UseBulkOperation
                enumFormStringArray.add(curParam);
            }
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
        String[] enumHeaderStringArrayArray = request().headers().get("enum_header_string_array");
        List<String> enumHeaderStringArrayList = SwaggerUtils.parametersToList("csv", enumHeaderStringArrayArray);
        List<String> enumHeaderStringArray = new ArrayList<String>();
        for (String curParam : enumHeaderStringArrayList) {
            if (!curParam.isEmpty()) {
                //noinspection UseBulkOperation
                enumHeaderStringArray.add(curParam);
            }
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
    public Result testInlineAdditionalProperties() throws Exception {
        JsonNode nodeparam = request().body().asJson();
        Object param;
        if (nodeparam != null) {
            param = mapper.readValue(nodeparam.toString(), Object.class);
            if (configuration.getBoolean("useInputBeanValidation")) {
                SwaggerUtils.validate(param);
            }
        } else {
            throw new IllegalArgumentException("'param' parameter is required");
        }
        imp.testInlineAdditionalProperties(param);
        return ok();
    }

    @ApiAction
    public Result testJsonFormData() throws Exception {
        String valueparam = (request().body().asMultipartFormData().asFormUrlEncoded().get("param"))[0];
        String param;
        if (valueparam != null) {
            param = valueparam;
        } else {
            throw new IllegalArgumentException("'param' parameter is required");
        }
        String valueparam2 = (request().body().asMultipartFormData().asFormUrlEncoded().get("param2"))[0];
        String param2;
        if (valueparam2 != null) {
            param2 = valueparam2;
        } else {
            throw new IllegalArgumentException("'param2' parameter is required");
        }
        imp.testJsonFormData(param, param2);
        return ok();
    }
}
