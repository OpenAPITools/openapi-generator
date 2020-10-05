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

import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;

import javax.validation.constraints.*;

@SuppressWarnings("RedundantThrows")
public interface FakeApiControllerImpInterface {
    default Result createXmlItemHttp(Http.Request request, XmlItem xmlItem) throws Exception {
        createXmlItem(request, xmlItem);
        return ok();
    }

    void createXmlItem(Http.Request request, XmlItem xmlItem) throws Exception;

    default Result fakeOuterBooleanSerializeHttp(Http.Request request, Boolean body) throws Exception {
        Boolean obj = fakeOuterBooleanSerialize(request, body);
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
    }

    Boolean fakeOuterBooleanSerialize(Http.Request request, Boolean body) throws Exception;

    default Result fakeOuterCompositeSerializeHttp(Http.Request request, OuterComposite body) throws Exception {
        OuterComposite obj = fakeOuterCompositeSerialize(request, body);
        if (configuration.getBoolean("useOutputBeanValidation")) {
                OpenAPIUtils.validate(obj);
        }
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
    }

    OuterComposite fakeOuterCompositeSerialize(Http.Request request, OuterComposite body) throws Exception;

    default Result fakeOuterNumberSerializeHttp(Http.Request request, BigDecimal body) throws Exception {
        BigDecimal obj = fakeOuterNumberSerialize(request, body);
        if (configuration.getBoolean("useOutputBeanValidation")) {
                OpenAPIUtils.validate(obj);
        }
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
    }

    BigDecimal fakeOuterNumberSerialize(Http.Request request, BigDecimal body) throws Exception;

    default Result fakeOuterStringSerializeHttp(Http.Request request, String body) throws Exception {
        String obj = fakeOuterStringSerialize(request, body);
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
    }

    String fakeOuterStringSerialize(Http.Request request, String body) throws Exception;

    default Result testBodyWithFileSchemaHttp(Http.Request request, FileSchemaTestClass body) throws Exception {
        testBodyWithFileSchema(request, body);
        return ok();
    }

    void testBodyWithFileSchema(Http.Request request, FileSchemaTestClass body) throws Exception;

    default Result testBodyWithQueryParamsHttp(Http.Request request, @NotNull String query, User body) throws Exception {
        testBodyWithQueryParams(request, query, body);
        return ok();
    }

    void testBodyWithQueryParams(Http.Request request, @NotNull String query, User body) throws Exception;

    default Result testClientModelHttp(Http.Request request, Client body) throws Exception {
        Client obj = testClientModel(request, body);
        if (configuration.getBoolean("useOutputBeanValidation")) {
                OpenAPIUtils.validate(obj);
        }
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
    }

    Client testClientModel(Http.Request request, Client body) throws Exception;

    default Result testEndpointParametersHttp(Http.Request request, BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string, Http.MultipartFormData.FilePart binary, LocalDate date, OffsetDateTime dateTime, String password, String paramCallback) throws Exception {
        testEndpointParameters(request, number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, paramCallback);
        return ok();
    }

    void testEndpointParameters(Http.Request request, BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string, Http.MultipartFormData.FilePart binary, LocalDate date, OffsetDateTime dateTime, String password, String paramCallback) throws Exception;

    default Result testEnumParametersHttp(Http.Request request, List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble, List<String> enumFormStringArray, String enumFormString) throws Exception {
        testEnumParameters(request, enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumFormStringArray, enumFormString);
        return ok();
    }

    void testEnumParameters(Http.Request request, List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble, List<String> enumFormStringArray, String enumFormString) throws Exception;

    default Result testGroupParametersHttp(Http.Request request, @NotNull Integer requiredStringGroup, Boolean requiredBooleanGroup, @NotNull Long requiredInt64Group, Integer stringGroup, Boolean booleanGroup, Long int64Group) throws Exception {
        testGroupParameters(request, requiredStringGroup, requiredBooleanGroup, requiredInt64Group, stringGroup, booleanGroup, int64Group);
        return ok();
    }

    void testGroupParameters(Http.Request request, @NotNull Integer requiredStringGroup, Boolean requiredBooleanGroup, @NotNull Long requiredInt64Group, Integer stringGroup, Boolean booleanGroup, Long int64Group) throws Exception;

    default Result testInlineAdditionalPropertiesHttp(Http.Request request, Map<String, String> param) throws Exception {
        testInlineAdditionalProperties(request, param);
        return ok();
    }

    void testInlineAdditionalProperties(Http.Request request, Map<String, String> param) throws Exception;

    default Result testJsonFormDataHttp(Http.Request request, String param, String param2) throws Exception {
        testJsonFormData(request, param, param2);
        return ok();
    }

    void testJsonFormData(Http.Request request, String param, String param2) throws Exception;

    default Result testQueryParameterCollectionFormatHttp(Http.Request request, @NotNull List<String> pipe, @NotNull List<String> ioutil, @NotNull List<String> http, @NotNull List<String> url, @NotNull List<String> context) throws Exception {
        testQueryParameterCollectionFormat(request, pipe, ioutil, http, url, context);
        return ok();
    }

    void testQueryParameterCollectionFormat(Http.Request request, @NotNull List<String> pipe, @NotNull List<String> ioutil, @NotNull List<String> http, @NotNull List<String> url, @NotNull List<String> context) throws Exception;

}
