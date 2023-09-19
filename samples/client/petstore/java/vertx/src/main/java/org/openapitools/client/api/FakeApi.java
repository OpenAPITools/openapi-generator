package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import io.vertx.core.file.AsyncFile;
import java.math.BigDecimal;
import org.openapitools.client.model.ChildWithNullable;
import org.openapitools.client.model.Client;
import org.openapitools.client.model.EnumClass;
import org.openapitools.client.model.FakeBigDecimalMap200Response;
import org.openapitools.client.model.FileSchemaTestClass;
import org.openapitools.client.model.HealthCheckResult;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import org.openapitools.client.model.OuterComposite;
import org.openapitools.client.model.OuterObjectWithEnumProperty;
import org.openapitools.client.model.Pet;
import org.openapitools.client.model.User;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;

import java.util.*;

public interface FakeApi {

    void fakeBigDecimalMap(Handler<AsyncResult<FakeBigDecimalMap200Response>> handler);

    void fakeBigDecimalMap(ApiClient.AuthInfo authInfo, Handler<AsyncResult<FakeBigDecimalMap200Response>> handler);

    void fakeHealthGet(Handler<AsyncResult<HealthCheckResult>> handler);

    void fakeHealthGet(ApiClient.AuthInfo authInfo, Handler<AsyncResult<HealthCheckResult>> handler);

    void fakeHttpSignatureTest(Pet pet, String query1, String header1, Handler<AsyncResult<Void>> handler);

    void fakeHttpSignatureTest(Pet pet, String query1, String header1, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void fakeOuterBooleanSerialize(Boolean body, Handler<AsyncResult<Boolean>> handler);

    void fakeOuterBooleanSerialize(Boolean body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Boolean>> handler);

    void fakeOuterCompositeSerialize(OuterComposite outerComposite, Handler<AsyncResult<OuterComposite>> handler);

    void fakeOuterCompositeSerialize(OuterComposite outerComposite, ApiClient.AuthInfo authInfo, Handler<AsyncResult<OuterComposite>> handler);

    void fakeOuterNumberSerialize(BigDecimal body, Handler<AsyncResult<BigDecimal>> handler);

    void fakeOuterNumberSerialize(BigDecimal body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<BigDecimal>> handler);

    void fakeOuterStringSerialize(String body, Handler<AsyncResult<String>> handler);

    void fakeOuterStringSerialize(String body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<String>> handler);

    void fakePropertyEnumIntegerSerialize(OuterObjectWithEnumProperty outerObjectWithEnumProperty, Handler<AsyncResult<OuterObjectWithEnumProperty>> handler);

    void fakePropertyEnumIntegerSerialize(OuterObjectWithEnumProperty outerObjectWithEnumProperty, ApiClient.AuthInfo authInfo, Handler<AsyncResult<OuterObjectWithEnumProperty>> handler);

    void testBodyWithBinary(AsyncFile body, Handler<AsyncResult<Void>> handler);

    void testBodyWithBinary(AsyncFile body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void testBodyWithFileSchema(FileSchemaTestClass fileSchemaTestClass, Handler<AsyncResult<Void>> handler);

    void testBodyWithFileSchema(FileSchemaTestClass fileSchemaTestClass, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void testBodyWithQueryParams(String query, User user, Handler<AsyncResult<Void>> handler);

    void testBodyWithQueryParams(String query, User user, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void testClientModel(Client client, Handler<AsyncResult<Client>> handler);

    void testClientModel(Client client, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Client>> handler);

    void testEndpointParameters(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string, AsyncFile binary, LocalDate date, OffsetDateTime dateTime, String password, String paramCallback, Handler<AsyncResult<Void>> handler);

    void testEndpointParameters(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string, AsyncFile binary, LocalDate date, OffsetDateTime dateTime, String password, String paramCallback, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void testEnumParameters(List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble, List<EnumClass> enumQueryModelArray, List<String> enumFormStringArray, String enumFormString, Handler<AsyncResult<Void>> handler);

    void testEnumParameters(List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble, List<EnumClass> enumQueryModelArray, List<String> enumFormStringArray, String enumFormString, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void testGroupParameters(Integer requiredStringGroup, Boolean requiredBooleanGroup, Long requiredInt64Group, Integer stringGroup, Boolean booleanGroup, Long int64Group, Handler<AsyncResult<Void>> handler);

    void testGroupParameters(Integer requiredStringGroup, Boolean requiredBooleanGroup, Long requiredInt64Group, Integer stringGroup, Boolean booleanGroup, Long int64Group, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void testInlineAdditionalProperties(Map<String, String> requestBody, Handler<AsyncResult<Void>> handler);

    void testInlineAdditionalProperties(Map<String, String> requestBody, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void testJsonFormData(String param, String param2, Handler<AsyncResult<Void>> handler);

    void testJsonFormData(String param, String param2, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void testNullable(ChildWithNullable childWithNullable, Handler<AsyncResult<Void>> handler);

    void testNullable(ChildWithNullable childWithNullable, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void testQueryParameterCollectionFormat(List<String> pipe, List<String> ioutil, List<String> http, List<String> url, List<String> context, String allowEmpty, Map<String, String> language, Handler<AsyncResult<Void>> handler);

    void testQueryParameterCollectionFormat(List<String> pipe, List<String> ioutil, List<String> http, List<String> url, List<String> context, String allowEmpty, Map<String, String> language, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

}
