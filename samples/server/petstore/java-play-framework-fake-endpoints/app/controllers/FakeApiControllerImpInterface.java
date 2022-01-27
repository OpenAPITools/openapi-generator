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

import com.google.inject.Inject;
import com.typesafe.config.Config;
import play.mvc.Controller;
import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
import play.mvc.Result;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.JsonNode;
import openapitools.OpenAPIUtils;
import openapitools.SecurityAPIUtils;
import static play.mvc.Results.ok;
import static play.mvc.Results.unauthorized;
import play.libs.Files.TemporaryFile;

import javax.validation.constraints.*;

@SuppressWarnings("RedundantThrows")
public abstract class FakeApiControllerImpInterface {
    @Inject private Config configuration;
    @Inject private SecurityAPIUtils securityAPIUtils;
    private ObjectMapper mapper = new ObjectMapper();

    public Result createXmlItemHttp(Http.Request request, XmlItem xmlItem) throws Exception {
        createXmlItem(request, xmlItem);
        return ok();

    }

    public abstract void createXmlItem(Http.Request request, XmlItem xmlItem) throws Exception;

    public Result fakeOuterBooleanSerializeHttp(Http.Request request, Boolean body) throws Exception {
        Boolean obj = fakeOuterBooleanSerialize(request, body);
        JsonNode result = mapper.valueToTree(obj);

        return ok(result);

    }

    public abstract Boolean fakeOuterBooleanSerialize(Http.Request request, Boolean body) throws Exception;

    public Result fakeOuterCompositeSerializeHttp(Http.Request request, OuterComposite body) throws Exception {
        OuterComposite obj = fakeOuterCompositeSerialize(request, body);

        if (configuration.getBoolean("useOutputBeanValidation")) {
            OpenAPIUtils.validate(obj);
        }

        JsonNode result = mapper.valueToTree(obj);

        return ok(result);

    }

    public abstract OuterComposite fakeOuterCompositeSerialize(Http.Request request, OuterComposite body) throws Exception;

    public Result fakeOuterNumberSerializeHttp(Http.Request request, BigDecimal body) throws Exception {
        BigDecimal obj = fakeOuterNumberSerialize(request, body);

        if (configuration.getBoolean("useOutputBeanValidation")) {
            OpenAPIUtils.validate(obj);
        }

        JsonNode result = mapper.valueToTree(obj);

        return ok(result);

    }

    public abstract BigDecimal fakeOuterNumberSerialize(Http.Request request, BigDecimal body) throws Exception;

    public Result fakeOuterStringSerializeHttp(Http.Request request, String body) throws Exception {
        String obj = fakeOuterStringSerialize(request, body);
        JsonNode result = mapper.valueToTree(obj);

        return ok(result);

    }

    public abstract String fakeOuterStringSerialize(Http.Request request, String body) throws Exception;

    public Result testBodyWithFileSchemaHttp(Http.Request request, FileSchemaTestClass body) throws Exception {
        testBodyWithFileSchema(request, body);
        return ok();

    }

    public abstract void testBodyWithFileSchema(Http.Request request, FileSchemaTestClass body) throws Exception;

    public Result testBodyWithQueryParamsHttp(Http.Request request, @NotNull String query, User body) throws Exception {
        testBodyWithQueryParams(request, query, body);
        return ok();

    }

    public abstract void testBodyWithQueryParams(Http.Request request, @NotNull String query, User body) throws Exception;

    public Result testClientModelHttp(Http.Request request, Client body) throws Exception {
        Client obj = testClientModel(request, body);

        if (configuration.getBoolean("useOutputBeanValidation")) {
            OpenAPIUtils.validate(obj);
        }

        JsonNode result = mapper.valueToTree(obj);

        return ok(result);

    }

    public abstract Client testClientModel(Http.Request request, Client body) throws Exception;

    public Result testEndpointParametersHttp(Http.Request request, BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string, Http.MultipartFormData.FilePart<TemporaryFile> binary, LocalDate date, OffsetDateTime dateTime, String password, String paramCallback) throws Exception {
        testEndpointParameters(request, number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, paramCallback);
        return ok();

    }

    public abstract void testEndpointParameters(Http.Request request, BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string, Http.MultipartFormData.FilePart<TemporaryFile> binary, LocalDate date, OffsetDateTime dateTime, String password, String paramCallback) throws Exception;

    public Result testEnumParametersHttp(Http.Request request, List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble, List<String> enumFormStringArray, String enumFormString) throws Exception {
        testEnumParameters(request, enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumFormStringArray, enumFormString);
        return ok();

    }

    public abstract void testEnumParameters(Http.Request request, List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble, List<String> enumFormStringArray, String enumFormString) throws Exception;

    public Result testGroupParametersHttp(Http.Request request, @NotNull Integer requiredStringGroup, Boolean requiredBooleanGroup, @NotNull Long requiredInt64Group, Integer stringGroup, Boolean booleanGroup, Long int64Group) throws Exception {
        testGroupParameters(request, requiredStringGroup, requiredBooleanGroup, requiredInt64Group, stringGroup, booleanGroup, int64Group);
        return ok();

    }

    public abstract void testGroupParameters(Http.Request request, @NotNull Integer requiredStringGroup, Boolean requiredBooleanGroup, @NotNull Long requiredInt64Group, Integer stringGroup, Boolean booleanGroup, Long int64Group) throws Exception;

    public Result testInlineAdditionalPropertiesHttp(Http.Request request, Map<String, String> param) throws Exception {
        testInlineAdditionalProperties(request, param);
        return ok();

    }

    public abstract void testInlineAdditionalProperties(Http.Request request, Map<String, String> param) throws Exception;

    public Result testJsonFormDataHttp(Http.Request request, String param, String param2) throws Exception {
        testJsonFormData(request, param, param2);
        return ok();

    }

    public abstract void testJsonFormData(Http.Request request, String param, String param2) throws Exception;

    public Result testQueryParameterCollectionFormatHttp(Http.Request request, @NotNull List<String> pipe, @NotNull List<String> ioutil, @NotNull List<String> http, @NotNull List<String> url, @NotNull List<String> context) throws Exception {
        testQueryParameterCollectionFormat(request, pipe, ioutil, http, url, context);
        return ok();

    }

    public abstract void testQueryParameterCollectionFormat(Http.Request request, @NotNull List<String> pipe, @NotNull List<String> ioutil, @NotNull List<String> http, @NotNull List<String> url, @NotNull List<String> context) throws Exception;

}
