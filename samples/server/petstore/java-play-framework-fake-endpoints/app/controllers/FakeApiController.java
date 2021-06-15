package controllers;

import java.math.BigDecimal;
import apimodels.Client;
import apimodels.FileSchemaTestClass;
import java.io.InputStream;
import java.time.LocalDate;
import java.util.Map;
import java.time.OffsetDateTime;
import apimodels.OuterComposite;
import apimodels.User;
import apimodels.XmlItem;

import com.typesafe.config.Config;
import play.mvc.Controller;
import play.mvc.Result;
import play.mvc.Http;
import java.util.List;
import java.util.Map;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.JsonNode;
import com.google.inject.Inject;
import java.io.File;
import play.libs.Files.TemporaryFile;
import openapitools.OpenAPIUtils;
import com.fasterxml.jackson.core.type.TypeReference;

import javax.validation.constraints.*;
import com.typesafe.config.Config;

import openapitools.OpenAPIUtils.ApiAction;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaPlayFrameworkCodegen")
public class FakeApiController extends Controller {
    private final FakeApiControllerImpInterface imp;
    private final ObjectMapper mapper;
    private final Config configuration;

    @Inject
    private FakeApiController(Config configuration, FakeApiControllerImpInterface imp) {
        this.imp = imp;
        mapper = new ObjectMapper();
        this.configuration = configuration;
    }

    @ApiAction
    public Result createXmlItem(Http.Request request) throws Exception {
        JsonNode nodexmlItem = request.body().asJson();
        XmlItem xmlItem;
        if (nodexmlItem != null) {
            xmlItem = mapper.readValue(nodexmlItem.toString(), XmlItem.class);
            if (configuration.getBoolean("useInputBeanValidation")) {
                OpenAPIUtils.validate(xmlItem);
            }
        } else {
            throw new IllegalArgumentException("'XmlItem' parameter is required");
        }
        return imp.createXmlItemHttp(request, xmlItem);
    }

    @ApiAction
    public Result fakeOuterBooleanSerialize(Http.Request request) throws Exception {
        JsonNode nodebody = request.body().asJson();
        Boolean body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), Boolean.class);
            if (configuration.getBoolean("useInputBeanValidation")) {
                OpenAPIUtils.validate(body);
            }
        } else {
            body = null;
        }
        return imp.fakeOuterBooleanSerializeHttp(request, body);
    }

    @ApiAction
    public Result fakeOuterCompositeSerialize(Http.Request request) throws Exception {
        JsonNode nodebody = request.body().asJson();
        OuterComposite body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), OuterComposite.class);
            if (configuration.getBoolean("useInputBeanValidation")) {
                OpenAPIUtils.validate(body);
            }
        } else {
            body = null;
        }
        return imp.fakeOuterCompositeSerializeHttp(request, body);
    }

    @ApiAction
    public Result fakeOuterNumberSerialize(Http.Request request) throws Exception {
        JsonNode nodebody = request.body().asJson();
        BigDecimal body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), BigDecimal.class);
            if (configuration.getBoolean("useInputBeanValidation")) {
                OpenAPIUtils.validate(body);
            }
        } else {
            body = null;
        }
        return imp.fakeOuterNumberSerializeHttp(request, body);
    }

    @ApiAction
    public Result fakeOuterStringSerialize(Http.Request request) throws Exception {
        JsonNode nodebody = request.body().asJson();
        String body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), String.class);
            if (configuration.getBoolean("useInputBeanValidation")) {
                OpenAPIUtils.validate(body);
            }
        } else {
            body = null;
        }
        return imp.fakeOuterStringSerializeHttp(request, body);
    }

    @ApiAction
    public Result testBodyWithFileSchema(Http.Request request) throws Exception {
        JsonNode nodebody = request.body().asJson();
        FileSchemaTestClass body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), FileSchemaTestClass.class);
            if (configuration.getBoolean("useInputBeanValidation")) {
                OpenAPIUtils.validate(body);
            }
        } else {
            throw new IllegalArgumentException("'body' parameter is required");
        }
        return imp.testBodyWithFileSchemaHttp(request, body);
    }

    @ApiAction
    public Result testBodyWithQueryParams(Http.Request request) throws Exception {
        JsonNode nodebody = request.body().asJson();
        User body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), User.class);
            if (configuration.getBoolean("useInputBeanValidation")) {
                OpenAPIUtils.validate(body);
            }
        } else {
            throw new IllegalArgumentException("'body' parameter is required");
        }
        String valuequery = request.getQueryString("query");
        String query;
        if (valuequery != null) {
            query = valuequery;
        } else {
            throw new IllegalArgumentException("'query' parameter is required");
        }
        return imp.testBodyWithQueryParamsHttp(request, query, body);
    }

    @ApiAction
    public Result testClientModel(Http.Request request) throws Exception {
        JsonNode nodebody = request.body().asJson();
        Client body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), Client.class);
            if (configuration.getBoolean("useInputBeanValidation")) {
                OpenAPIUtils.validate(body);
            }
        } else {
            throw new IllegalArgumentException("'body' parameter is required");
        }
        return imp.testClientModelHttp(request, body);
    }

    @ApiAction
    public Result testEndpointParameters(Http.Request request) throws Exception {
        String valueinteger = (request.body().asMultipartFormData().asFormUrlEncoded().get("integer"))[0];
        Integer integer;
        if (valueinteger != null) {
            integer = Integer.parseInt(valueinteger);
        } else {
            integer = null;
        }
        String valueint32 = (request.body().asMultipartFormData().asFormUrlEncoded().get("int32"))[0];
        Integer int32;
        if (valueint32 != null) {
            int32 = Integer.parseInt(valueint32);
        } else {
            int32 = null;
        }
        String valueint64 = (request.body().asMultipartFormData().asFormUrlEncoded().get("int64"))[0];
        Long int64;
        if (valueint64 != null) {
            int64 = Long.parseLong(valueint64);
        } else {
            int64 = null;
        }
        String valuenumber = (request.body().asMultipartFormData().asFormUrlEncoded().get("number"))[0];
        BigDecimal number;
        if (valuenumber != null) {
            number = new BigDecimal(valuenumber);
        } else {
            throw new IllegalArgumentException("'number' parameter is required");
        }
        String value_float = (request.body().asMultipartFormData().asFormUrlEncoded().get("float"))[0];
        Float _float;
        if (value_float != null) {
            _float = Float.parseFloat(value_float);
        } else {
            _float = null;
        }
        String value_double = (request.body().asMultipartFormData().asFormUrlEncoded().get("double"))[0];
        Double _double;
        if (value_double != null) {
            _double = Double.parseDouble(value_double);
        } else {
            throw new IllegalArgumentException("'double' parameter is required");
        }
        String valuestring = (request.body().asMultipartFormData().asFormUrlEncoded().get("string"))[0];
        String string;
        if (valuestring != null) {
            string = valuestring;
        } else {
            string = null;
        }
        String valuepatternWithoutDelimiter = (request.body().asMultipartFormData().asFormUrlEncoded().get("pattern_without_delimiter"))[0];
        String patternWithoutDelimiter;
        if (valuepatternWithoutDelimiter != null) {
            patternWithoutDelimiter = valuepatternWithoutDelimiter;
        } else {
            throw new IllegalArgumentException("'pattern_without_delimiter' parameter is required");
        }
        String value_byte = (request.body().asMultipartFormData().asFormUrlEncoded().get("byte"))[0];
        byte[] _byte;
        if (value_byte != null) {
            _byte = value_byte.getBytes();
        } else {
            throw new IllegalArgumentException("'byte' parameter is required");
        }
        Http.MultipartFormData<TemporaryFile> bodybinary = request.body().asMultipartFormData();
        Http.MultipartFormData.FilePart<TemporaryFile> binary = bodybinary.getFile("binary");
        String valuedate = (request.body().asMultipartFormData().asFormUrlEncoded().get("date"))[0];
        LocalDate date;
        if (valuedate != null) {
            date = LocalDate.parse(valuedate);
        } else {
            date = null;
        }
        String valuedateTime = (request.body().asMultipartFormData().asFormUrlEncoded().get("dateTime"))[0];
        OffsetDateTime dateTime;
        if (valuedateTime != null) {
            dateTime = OffsetDateTime.parse(valuedateTime);
        } else {
            dateTime = null;
        }
        String valuepassword = (request.body().asMultipartFormData().asFormUrlEncoded().get("password"))[0];
        String password;
        if (valuepassword != null) {
            password = valuepassword;
        } else {
            password = null;
        }
        String valueparamCallback = (request.body().asMultipartFormData().asFormUrlEncoded().get("callback"))[0];
        String paramCallback;
        if (valueparamCallback != null) {
            paramCallback = valueparamCallback;
        } else {
            paramCallback = null;
        }
        return imp.testEndpointParametersHttp(request, number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, paramCallback);
    }

    @ApiAction
    public Result testEnumParameters(Http.Request request) throws Exception {
        String[] enumQueryStringArrayArray = request.queryString().get("enum_query_string_array");
        List<String> enumQueryStringArrayList = OpenAPIUtils.parametersToList("csv", enumQueryStringArrayArray);
        List<String> enumQueryStringArray = new ArrayList<>();
        for (String curParam : enumQueryStringArrayList) {
            if (!curParam.isEmpty()) {
                //noinspection UseBulkOperation
                enumQueryStringArray.add(curParam);
            }
        }
        String valueenumQueryString = request.getQueryString("enum_query_string");
        String enumQueryString;
        if (valueenumQueryString != null) {
            enumQueryString = valueenumQueryString;
        } else {
            enumQueryString = "-efg";
        }
        String valueenumQueryInteger = request.getQueryString("enum_query_integer");
        Integer enumQueryInteger;
        if (valueenumQueryInteger != null) {
            enumQueryInteger = Integer.parseInt(valueenumQueryInteger);
        } else {
            enumQueryInteger = null;
        }
        String valueenumQueryDouble = request.getQueryString("enum_query_double");
        Double enumQueryDouble;
        if (valueenumQueryDouble != null) {
            enumQueryDouble = Double.parseDouble(valueenumQueryDouble);
        } else {
            enumQueryDouble = null;
        }
        String[] enumFormStringArrayArray = request.body().asMultipartFormData().asFormUrlEncoded().get("enum_form_string_array");
        List<String> enumFormStringArrayList = OpenAPIUtils.parametersToList("csv", enumFormStringArrayArray);
        List<String> enumFormStringArray = new ArrayList<>();
        for (String curParam : enumFormStringArrayList) {
            if (!curParam.isEmpty()) {
                //noinspection UseBulkOperation
                enumFormStringArray.add(curParam);
            }
        }
        String valueenumFormString = (request.body().asMultipartFormData().asFormUrlEncoded().get("enum_form_string"))[0];
        String enumFormString;
        if (valueenumFormString != null) {
            enumFormString = valueenumFormString;
        } else {
            enumFormString = "-efg";
        }
        String[] enumHeaderStringArrayArray = request.getHeaders().getAll("enum_header_string_array").toArray(new String[0]);
        List<String> enumHeaderStringArrayList = OpenAPIUtils.parametersToList("csv", enumHeaderStringArrayArray);
        List<String> enumHeaderStringArray = new ArrayList<>();
        for (String curParam : enumHeaderStringArrayList) {
            if (!curParam.isEmpty()) {
                //noinspection UseBulkOperation
                enumHeaderStringArray.add(curParam);
            }
        }
        String valueenumHeaderString = request.header("enum_header_string").get();
        String enumHeaderString;
        if (valueenumHeaderString != null) {
            enumHeaderString = valueenumHeaderString;
        } else {
            enumHeaderString = "-efg";
        }
        return imp.testEnumParametersHttp(request, enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumFormStringArray, enumFormString);
    }

    @ApiAction
    public Result testGroupParameters(Http.Request request) throws Exception {
        String valuerequiredStringGroup = request.getQueryString("required_string_group");
        Integer requiredStringGroup;
        if (valuerequiredStringGroup != null) {
            requiredStringGroup = Integer.parseInt(valuerequiredStringGroup);
        } else {
            throw new IllegalArgumentException("'required_string_group' parameter is required");
        }
        String valuerequiredInt64Group = request.getQueryString("required_int64_group");
        Long requiredInt64Group;
        if (valuerequiredInt64Group != null) {
            requiredInt64Group = Long.parseLong(valuerequiredInt64Group);
        } else {
            throw new IllegalArgumentException("'required_int64_group' parameter is required");
        }
        String valuestringGroup = request.getQueryString("string_group");
        Integer stringGroup;
        if (valuestringGroup != null) {
            stringGroup = Integer.parseInt(valuestringGroup);
        } else {
            stringGroup = null;
        }
        String valueint64Group = request.getQueryString("int64_group");
        Long int64Group;
        if (valueint64Group != null) {
            int64Group = Long.parseLong(valueint64Group);
        } else {
            int64Group = null;
        }
        String valuerequiredBooleanGroup = request.header("required_boolean_group").get();
        Boolean requiredBooleanGroup;
        if (valuerequiredBooleanGroup != null) {
            requiredBooleanGroup = Boolean.valueOf(valuerequiredBooleanGroup);
        } else {
            throw new IllegalArgumentException("'required_boolean_group' parameter is required");
        }
        String valuebooleanGroup = request.header("boolean_group").get();
        Boolean booleanGroup;
        if (valuebooleanGroup != null) {
            booleanGroup = Boolean.valueOf(valuebooleanGroup);
        } else {
            booleanGroup = null;
        }
        return imp.testGroupParametersHttp(request, requiredStringGroup, requiredBooleanGroup, requiredInt64Group, stringGroup, booleanGroup, int64Group);
    }

    @ApiAction
    public Result testInlineAdditionalProperties(Http.Request request) throws Exception {
        JsonNode nodeparam = request.body().asJson();
        Map<String, String> param;
        if (nodeparam != null) {
            param = mapper.readValue(nodeparam.toString(), new TypeReference<Map<String, String>>(){});
            if (configuration.getBoolean("useInputBeanValidation")) {
                for (Map.Entry<String, String> entry : param.entrySet()) {
                    OpenAPIUtils.validate(entry.getValue());
                }
            }
        } else {
            throw new IllegalArgumentException("'param' parameter is required");
        }
        return imp.testInlineAdditionalPropertiesHttp(request, param);
    }

    @ApiAction
    public Result testJsonFormData(Http.Request request) throws Exception {
        String valueparam = (request.body().asMultipartFormData().asFormUrlEncoded().get("param"))[0];
        String param;
        if (valueparam != null) {
            param = valueparam;
        } else {
            throw new IllegalArgumentException("'param' parameter is required");
        }
        String valueparam2 = (request.body().asMultipartFormData().asFormUrlEncoded().get("param2"))[0];
        String param2;
        if (valueparam2 != null) {
            param2 = valueparam2;
        } else {
            throw new IllegalArgumentException("'param2' parameter is required");
        }
        return imp.testJsonFormDataHttp(request, param, param2);
    }

    @ApiAction
    public Result testQueryParameterCollectionFormat(Http.Request request) throws Exception {
        String[] pipeArray = request.queryString().get("pipe");
        if (pipeArray == null) {
            throw new IllegalArgumentException("'pipe' parameter is required");
        }
        List<String> pipeList = OpenAPIUtils.parametersToList("csv", pipeArray);
        List<String> pipe = new ArrayList<>();
        for (String curParam : pipeList) {
            if (!curParam.isEmpty()) {
                //noinspection UseBulkOperation
                pipe.add(curParam);
            }
        }
        String[] ioutilArray = request.queryString().get("ioutil");
        if (ioutilArray == null) {
            throw new IllegalArgumentException("'ioutil' parameter is required");
        }
        List<String> ioutilList = OpenAPIUtils.parametersToList("csv", ioutilArray);
        List<String> ioutil = new ArrayList<>();
        for (String curParam : ioutilList) {
            if (!curParam.isEmpty()) {
                //noinspection UseBulkOperation
                ioutil.add(curParam);
            }
        }
        String[] httpArray = request.queryString().get("http");
        if (httpArray == null) {
            throw new IllegalArgumentException("'http' parameter is required");
        }
        List<String> httpList = OpenAPIUtils.parametersToList("ssv", httpArray);
        List<String> http = new ArrayList<>();
        for (String curParam : httpList) {
            if (!curParam.isEmpty()) {
                //noinspection UseBulkOperation
                http.add(curParam);
            }
        }
        String[] urlArray = request.queryString().get("url");
        if (urlArray == null) {
            throw new IllegalArgumentException("'url' parameter is required");
        }
        List<String> urlList = OpenAPIUtils.parametersToList("csv", urlArray);
        List<String> url = new ArrayList<>();
        for (String curParam : urlList) {
            if (!curParam.isEmpty()) {
                //noinspection UseBulkOperation
                url.add(curParam);
            }
        }
        String[] contextArray = request.queryString().get("context");
        if (contextArray == null) {
            throw new IllegalArgumentException("'context' parameter is required");
        }
        List<String> contextList = OpenAPIUtils.parametersToList("multi", contextArray);
        List<String> context = new ArrayList<>();
        for (String curParam : contextList) {
            if (!curParam.isEmpty()) {
                //noinspection UseBulkOperation
                context.add(curParam);
            }
        }
        return imp.testQueryParameterCollectionFormatHttp(request, pipe, ioutil, http, url, context);
    }

}
