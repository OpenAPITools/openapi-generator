package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import io.vertx.core.file.AsyncFile;
import java.math.BigDecimal;
import org.openapitools.client.model.Client;
import org.openapitools.client.model.FileSchemaTestClass;
import org.threeten.bp.LocalDate;
import org.threeten.bp.OffsetDateTime;
import org.openapitools.client.model.OuterComposite;
import org.openapitools.client.model.User;
import org.openapitools.client.model.XmlItem;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;

import java.util.*;

public interface FakeApi {

    void createXmlItem(XmlItem xmlItem, Handler<AsyncResult<Void>> handler);

    void createXmlItem(XmlItem xmlItem, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void fakeOuterBooleanSerialize(Boolean body, Handler<AsyncResult<Boolean>> handler);

    void fakeOuterBooleanSerialize(Boolean body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Boolean>> handler);

    void fakeOuterCompositeSerialize(OuterComposite body, Handler<AsyncResult<OuterComposite>> handler);

    void fakeOuterCompositeSerialize(OuterComposite body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<OuterComposite>> handler);

    void fakeOuterNumberSerialize(BigDecimal body, Handler<AsyncResult<BigDecimal>> handler);

    void fakeOuterNumberSerialize(BigDecimal body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<BigDecimal>> handler);

    void fakeOuterStringSerialize(String body, Handler<AsyncResult<String>> handler);

    void fakeOuterStringSerialize(String body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<String>> handler);

    void testBodyWithFileSchema(FileSchemaTestClass body, Handler<AsyncResult<Void>> handler);

    void testBodyWithFileSchema(FileSchemaTestClass body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void testBodyWithQueryParams(String query, User body, Handler<AsyncResult<Void>> handler);

    void testBodyWithQueryParams(String query, User body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void testClientModel(Client body, Handler<AsyncResult<Client>> handler);

    void testClientModel(Client body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Client>> handler);

    void testEndpointParameters(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string, AsyncFile binary, LocalDate date, OffsetDateTime dateTime, String password, String paramCallback, Handler<AsyncResult<Void>> handler);

    void testEndpointParameters(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string, AsyncFile binary, LocalDate date, OffsetDateTime dateTime, String password, String paramCallback, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void testEnumParameters(List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble, List<String> enumFormStringArray, String enumFormString, Handler<AsyncResult<Void>> handler);

    void testEnumParameters(List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble, List<String> enumFormStringArray, String enumFormString, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void testGroupParameters(Integer requiredStringGroup, Boolean requiredBooleanGroup, Long requiredInt64Group, Integer stringGroup, Boolean booleanGroup, Long int64Group, Handler<AsyncResult<Void>> handler);

    void testGroupParameters(Integer requiredStringGroup, Boolean requiredBooleanGroup, Long requiredInt64Group, Integer stringGroup, Boolean booleanGroup, Long int64Group, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void testInlineAdditionalProperties(Map<String, String> param, Handler<AsyncResult<Void>> handler);

    void testInlineAdditionalProperties(Map<String, String> param, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void testJsonFormData(String param, String param2, Handler<AsyncResult<Void>> handler);

    void testJsonFormData(String param, String param2, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void testQueryParameterCollectionFormat(List<String> pipe, List<String> ioutil, List<String> http, List<String> url, List<String> context, Handler<AsyncResult<Void>> handler);

    void testQueryParameterCollectionFormat(List<String> pipe, List<String> ioutil, List<String> http, List<String> url, List<String> context, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

}
