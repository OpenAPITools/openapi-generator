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
import org.openapitools.client.model.TestInlineFreeformAdditionalPropertiesRequest;
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

    void fakeHttpSignatureTest(@javax.annotation.Nonnull Pet pet, @javax.annotation.Nullable String query1, @javax.annotation.Nullable String header1, Handler<AsyncResult<Void>> handler);

    void fakeHttpSignatureTest(@javax.annotation.Nonnull Pet pet, @javax.annotation.Nullable String query1, @javax.annotation.Nullable String header1, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void fakeOuterBooleanSerialize(@javax.annotation.Nullable Boolean body, Handler<AsyncResult<Boolean>> handler);

    void fakeOuterBooleanSerialize(@javax.annotation.Nullable Boolean body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Boolean>> handler);

    void fakeOuterCompositeSerialize(@javax.annotation.Nullable OuterComposite outerComposite, Handler<AsyncResult<OuterComposite>> handler);

    void fakeOuterCompositeSerialize(@javax.annotation.Nullable OuterComposite outerComposite, ApiClient.AuthInfo authInfo, Handler<AsyncResult<OuterComposite>> handler);

    void fakeOuterNumberSerialize(@javax.annotation.Nullable BigDecimal body, Handler<AsyncResult<BigDecimal>> handler);

    void fakeOuterNumberSerialize(@javax.annotation.Nullable BigDecimal body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<BigDecimal>> handler);

    void fakeOuterStringSerialize(@javax.annotation.Nullable String body, Handler<AsyncResult<String>> handler);

    void fakeOuterStringSerialize(@javax.annotation.Nullable String body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<String>> handler);

    void fakePropertyEnumIntegerSerialize(@javax.annotation.Nonnull OuterObjectWithEnumProperty outerObjectWithEnumProperty, Handler<AsyncResult<OuterObjectWithEnumProperty>> handler);

    void fakePropertyEnumIntegerSerialize(@javax.annotation.Nonnull OuterObjectWithEnumProperty outerObjectWithEnumProperty, ApiClient.AuthInfo authInfo, Handler<AsyncResult<OuterObjectWithEnumProperty>> handler);

    void testAdditionalPropertiesReference(@javax.annotation.Nonnull Map<String, Object> requestBody, Handler<AsyncResult<Void>> handler);

    void testAdditionalPropertiesReference(@javax.annotation.Nonnull Map<String, Object> requestBody, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void testBodyWithBinary(@javax.annotation.Nullable AsyncFile body, Handler<AsyncResult<Void>> handler);

    void testBodyWithBinary(@javax.annotation.Nullable AsyncFile body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void testBodyWithFileSchema(@javax.annotation.Nonnull FileSchemaTestClass fileSchemaTestClass, Handler<AsyncResult<Void>> handler);

    void testBodyWithFileSchema(@javax.annotation.Nonnull FileSchemaTestClass fileSchemaTestClass, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void testBodyWithQueryParams(@javax.annotation.Nonnull String query, @javax.annotation.Nonnull User user, Handler<AsyncResult<Void>> handler);

    void testBodyWithQueryParams(@javax.annotation.Nonnull String query, @javax.annotation.Nonnull User user, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void testClientModel(@javax.annotation.Nonnull Client client, Handler<AsyncResult<Client>> handler);

    void testClientModel(@javax.annotation.Nonnull Client client, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Client>> handler);

    void testEndpointParameters(@javax.annotation.Nonnull BigDecimal number, @javax.annotation.Nonnull Double _double, @javax.annotation.Nonnull String patternWithoutDelimiter, @javax.annotation.Nonnull byte[] _byte, @javax.annotation.Nullable Integer integer, @javax.annotation.Nullable Integer int32, @javax.annotation.Nullable Long int64, @javax.annotation.Nullable Float _float, @javax.annotation.Nullable String string, @javax.annotation.Nullable AsyncFile binary, @javax.annotation.Nullable LocalDate date, @javax.annotation.Nullable OffsetDateTime dateTime, @javax.annotation.Nullable String password, @javax.annotation.Nullable String paramCallback, Handler<AsyncResult<Void>> handler);

    void testEndpointParameters(@javax.annotation.Nonnull BigDecimal number, @javax.annotation.Nonnull Double _double, @javax.annotation.Nonnull String patternWithoutDelimiter, @javax.annotation.Nonnull byte[] _byte, @javax.annotation.Nullable Integer integer, @javax.annotation.Nullable Integer int32, @javax.annotation.Nullable Long int64, @javax.annotation.Nullable Float _float, @javax.annotation.Nullable String string, @javax.annotation.Nullable AsyncFile binary, @javax.annotation.Nullable LocalDate date, @javax.annotation.Nullable OffsetDateTime dateTime, @javax.annotation.Nullable String password, @javax.annotation.Nullable String paramCallback, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void testEnumParameters(@javax.annotation.Nullable List<String> enumHeaderStringArray, @javax.annotation.Nullable String enumHeaderString, @javax.annotation.Nullable List<String> enumQueryStringArray, @javax.annotation.Nullable String enumQueryString, @javax.annotation.Nullable Integer enumQueryInteger, @javax.annotation.Nullable Double enumQueryDouble, @javax.annotation.Nullable List<EnumClass> enumQueryModelArray, @javax.annotation.Nullable List<String> enumFormStringArray, @javax.annotation.Nullable String enumFormString, Handler<AsyncResult<Void>> handler);

    void testEnumParameters(@javax.annotation.Nullable List<String> enumHeaderStringArray, @javax.annotation.Nullable String enumHeaderString, @javax.annotation.Nullable List<String> enumQueryStringArray, @javax.annotation.Nullable String enumQueryString, @javax.annotation.Nullable Integer enumQueryInteger, @javax.annotation.Nullable Double enumQueryDouble, @javax.annotation.Nullable List<EnumClass> enumQueryModelArray, @javax.annotation.Nullable List<String> enumFormStringArray, @javax.annotation.Nullable String enumFormString, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void testGroupParameters(@javax.annotation.Nonnull Integer requiredStringGroup, @javax.annotation.Nonnull Boolean requiredBooleanGroup, @javax.annotation.Nonnull Long requiredInt64Group, @javax.annotation.Nullable Integer stringGroup, @javax.annotation.Nullable Boolean booleanGroup, @javax.annotation.Nullable Long int64Group, Handler<AsyncResult<Void>> handler);

    void testGroupParameters(@javax.annotation.Nonnull Integer requiredStringGroup, @javax.annotation.Nonnull Boolean requiredBooleanGroup, @javax.annotation.Nonnull Long requiredInt64Group, @javax.annotation.Nullable Integer stringGroup, @javax.annotation.Nullable Boolean booleanGroup, @javax.annotation.Nullable Long int64Group, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void testInlineAdditionalProperties(@javax.annotation.Nonnull Map<String, String> requestBody, Handler<AsyncResult<Void>> handler);

    void testInlineAdditionalProperties(@javax.annotation.Nonnull Map<String, String> requestBody, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void testInlineFreeformAdditionalProperties(@javax.annotation.Nonnull TestInlineFreeformAdditionalPropertiesRequest testInlineFreeformAdditionalPropertiesRequest, Handler<AsyncResult<Void>> handler);

    void testInlineFreeformAdditionalProperties(@javax.annotation.Nonnull TestInlineFreeformAdditionalPropertiesRequest testInlineFreeformAdditionalPropertiesRequest, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void testJsonFormData(@javax.annotation.Nonnull String param, @javax.annotation.Nonnull String param2, Handler<AsyncResult<Void>> handler);

    void testJsonFormData(@javax.annotation.Nonnull String param, @javax.annotation.Nonnull String param2, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void testNullable(@javax.annotation.Nonnull ChildWithNullable childWithNullable, Handler<AsyncResult<Void>> handler);

    void testNullable(@javax.annotation.Nonnull ChildWithNullable childWithNullable, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void testQueryParameterCollectionFormat(@javax.annotation.Nonnull List<String> pipe, @javax.annotation.Nonnull List<String> ioutil, @javax.annotation.Nonnull List<String> http, @javax.annotation.Nonnull List<String> url, @javax.annotation.Nonnull List<String> context, @javax.annotation.Nonnull String allowEmpty, @javax.annotation.Nullable Map<String, String> language, Handler<AsyncResult<Void>> handler);

    void testQueryParameterCollectionFormat(@javax.annotation.Nonnull List<String> pipe, @javax.annotation.Nonnull List<String> ioutil, @javax.annotation.Nonnull List<String> http, @javax.annotation.Nonnull List<String> url, @javax.annotation.Nonnull List<String> context, @javax.annotation.Nonnull String allowEmpty, @javax.annotation.Nullable Map<String, String> language, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void testStringMapReference(@javax.annotation.Nonnull Map<String, String> requestBody, Handler<AsyncResult<Void>> handler);

    void testStringMapReference(@javax.annotation.Nonnull Map<String, String> requestBody, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

}
