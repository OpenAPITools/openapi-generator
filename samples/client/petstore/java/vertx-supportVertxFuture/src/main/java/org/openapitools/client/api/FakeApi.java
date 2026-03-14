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
import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.json.JsonObject;

import java.util.*;

public interface FakeApi {

    void fakeBigDecimalMap(Handler<AsyncResult<FakeBigDecimalMap200Response>> handler);

    default Future<FakeBigDecimalMap200Response> fakeBigDecimalMap(){
        Promise<FakeBigDecimalMap200Response> promise = Promise.promise();
        fakeBigDecimalMap(promise);
        return promise.future();
    }

    void fakeBigDecimalMap(ApiClient.AuthInfo authInfo, Handler<AsyncResult<FakeBigDecimalMap200Response>> handler);

    default Future<FakeBigDecimalMap200Response> fakeBigDecimalMap(ApiClient.AuthInfo authInfo){
        Promise<FakeBigDecimalMap200Response> promise = Promise.promise();
        fakeBigDecimalMap(authInfo, promise);
        return promise.future();
    }

    void fakeHealthGet(Handler<AsyncResult<HealthCheckResult>> handler);

    default Future<HealthCheckResult> fakeHealthGet(){
        Promise<HealthCheckResult> promise = Promise.promise();
        fakeHealthGet(promise);
        return promise.future();
    }

    void fakeHealthGet(ApiClient.AuthInfo authInfo, Handler<AsyncResult<HealthCheckResult>> handler);

    default Future<HealthCheckResult> fakeHealthGet(ApiClient.AuthInfo authInfo){
        Promise<HealthCheckResult> promise = Promise.promise();
        fakeHealthGet(authInfo, promise);
        return promise.future();
    }

    void fakeHttpSignatureTest(@javax.annotation.Nonnull Pet pet, @javax.annotation.Nullable String query1, @javax.annotation.Nullable String header1, Handler<AsyncResult<Void>> handler);

    default Future<Void> fakeHttpSignatureTest(@javax.annotation.Nonnull Pet pet, @javax.annotation.Nullable String query1, @javax.annotation.Nullable String header1){
        Promise<Void> promise = Promise.promise();
        fakeHttpSignatureTest(pet, query1, header1, promise);
        return promise.future();
    }

    void fakeHttpSignatureTest(@javax.annotation.Nonnull Pet pet, @javax.annotation.Nullable String query1, @javax.annotation.Nullable String header1, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    default Future<Void> fakeHttpSignatureTest(@javax.annotation.Nonnull Pet pet, @javax.annotation.Nullable String query1, @javax.annotation.Nullable String header1, ApiClient.AuthInfo authInfo){
        Promise<Void> promise = Promise.promise();
        fakeHttpSignatureTest(pet, query1, header1, authInfo, promise);
        return promise.future();
    }

    void fakeOuterBooleanSerialize(@javax.annotation.Nullable Boolean body, Handler<AsyncResult<Boolean>> handler);

    default Future<Boolean> fakeOuterBooleanSerialize(@javax.annotation.Nullable Boolean body){
        Promise<Boolean> promise = Promise.promise();
        fakeOuterBooleanSerialize(body, promise);
        return promise.future();
    }

    void fakeOuterBooleanSerialize(@javax.annotation.Nullable Boolean body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Boolean>> handler);

    default Future<Boolean> fakeOuterBooleanSerialize(@javax.annotation.Nullable Boolean body, ApiClient.AuthInfo authInfo){
        Promise<Boolean> promise = Promise.promise();
        fakeOuterBooleanSerialize(body, authInfo, promise);
        return promise.future();
    }

    void fakeOuterCompositeSerialize(@javax.annotation.Nullable OuterComposite outerComposite, Handler<AsyncResult<OuterComposite>> handler);

    default Future<OuterComposite> fakeOuterCompositeSerialize(@javax.annotation.Nullable OuterComposite outerComposite){
        Promise<OuterComposite> promise = Promise.promise();
        fakeOuterCompositeSerialize(outerComposite, promise);
        return promise.future();
    }

    void fakeOuterCompositeSerialize(@javax.annotation.Nullable OuterComposite outerComposite, ApiClient.AuthInfo authInfo, Handler<AsyncResult<OuterComposite>> handler);

    default Future<OuterComposite> fakeOuterCompositeSerialize(@javax.annotation.Nullable OuterComposite outerComposite, ApiClient.AuthInfo authInfo){
        Promise<OuterComposite> promise = Promise.promise();
        fakeOuterCompositeSerialize(outerComposite, authInfo, promise);
        return promise.future();
    }

    void fakeOuterNumberSerialize(@javax.annotation.Nullable BigDecimal body, Handler<AsyncResult<BigDecimal>> handler);

    default Future<BigDecimal> fakeOuterNumberSerialize(@javax.annotation.Nullable BigDecimal body){
        Promise<BigDecimal> promise = Promise.promise();
        fakeOuterNumberSerialize(body, promise);
        return promise.future();
    }

    void fakeOuterNumberSerialize(@javax.annotation.Nullable BigDecimal body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<BigDecimal>> handler);

    default Future<BigDecimal> fakeOuterNumberSerialize(@javax.annotation.Nullable BigDecimal body, ApiClient.AuthInfo authInfo){
        Promise<BigDecimal> promise = Promise.promise();
        fakeOuterNumberSerialize(body, authInfo, promise);
        return promise.future();
    }

    void fakeOuterStringSerialize(@javax.annotation.Nullable String body, Handler<AsyncResult<String>> handler);

    default Future<String> fakeOuterStringSerialize(@javax.annotation.Nullable String body){
        Promise<String> promise = Promise.promise();
        fakeOuterStringSerialize(body, promise);
        return promise.future();
    }

    void fakeOuterStringSerialize(@javax.annotation.Nullable String body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<String>> handler);

    default Future<String> fakeOuterStringSerialize(@javax.annotation.Nullable String body, ApiClient.AuthInfo authInfo){
        Promise<String> promise = Promise.promise();
        fakeOuterStringSerialize(body, authInfo, promise);
        return promise.future();
    }

    void fakePropertyEnumIntegerSerialize(@javax.annotation.Nonnull OuterObjectWithEnumProperty outerObjectWithEnumProperty, Handler<AsyncResult<OuterObjectWithEnumProperty>> handler);

    default Future<OuterObjectWithEnumProperty> fakePropertyEnumIntegerSerialize(@javax.annotation.Nonnull OuterObjectWithEnumProperty outerObjectWithEnumProperty){
        Promise<OuterObjectWithEnumProperty> promise = Promise.promise();
        fakePropertyEnumIntegerSerialize(outerObjectWithEnumProperty, promise);
        return promise.future();
    }

    void fakePropertyEnumIntegerSerialize(@javax.annotation.Nonnull OuterObjectWithEnumProperty outerObjectWithEnumProperty, ApiClient.AuthInfo authInfo, Handler<AsyncResult<OuterObjectWithEnumProperty>> handler);

    default Future<OuterObjectWithEnumProperty> fakePropertyEnumIntegerSerialize(@javax.annotation.Nonnull OuterObjectWithEnumProperty outerObjectWithEnumProperty, ApiClient.AuthInfo authInfo){
        Promise<OuterObjectWithEnumProperty> promise = Promise.promise();
        fakePropertyEnumIntegerSerialize(outerObjectWithEnumProperty, authInfo, promise);
        return promise.future();
    }

    void testAdditionalPropertiesReference(@javax.annotation.Nonnull Map<String, Object> requestBody, Handler<AsyncResult<Void>> handler);

    default Future<Void> testAdditionalPropertiesReference(@javax.annotation.Nonnull Map<String, Object> requestBody){
        Promise<Void> promise = Promise.promise();
        testAdditionalPropertiesReference(requestBody, promise);
        return promise.future();
    }

    void testAdditionalPropertiesReference(@javax.annotation.Nonnull Map<String, Object> requestBody, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    default Future<Void> testAdditionalPropertiesReference(@javax.annotation.Nonnull Map<String, Object> requestBody, ApiClient.AuthInfo authInfo){
        Promise<Void> promise = Promise.promise();
        testAdditionalPropertiesReference(requestBody, authInfo, promise);
        return promise.future();
    }

    void testBodyWithBinary(@javax.annotation.Nullable AsyncFile body, Handler<AsyncResult<Void>> handler);

    default Future<Void> testBodyWithBinary(@javax.annotation.Nullable AsyncFile body){
        Promise<Void> promise = Promise.promise();
        testBodyWithBinary(body, promise);
        return promise.future();
    }

    void testBodyWithBinary(@javax.annotation.Nullable AsyncFile body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    default Future<Void> testBodyWithBinary(@javax.annotation.Nullable AsyncFile body, ApiClient.AuthInfo authInfo){
        Promise<Void> promise = Promise.promise();
        testBodyWithBinary(body, authInfo, promise);
        return promise.future();
    }

    void testBodyWithFileSchema(@javax.annotation.Nonnull FileSchemaTestClass fileSchemaTestClass, Handler<AsyncResult<Void>> handler);

    default Future<Void> testBodyWithFileSchema(@javax.annotation.Nonnull FileSchemaTestClass fileSchemaTestClass){
        Promise<Void> promise = Promise.promise();
        testBodyWithFileSchema(fileSchemaTestClass, promise);
        return promise.future();
    }

    void testBodyWithFileSchema(@javax.annotation.Nonnull FileSchemaTestClass fileSchemaTestClass, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    default Future<Void> testBodyWithFileSchema(@javax.annotation.Nonnull FileSchemaTestClass fileSchemaTestClass, ApiClient.AuthInfo authInfo){
        Promise<Void> promise = Promise.promise();
        testBodyWithFileSchema(fileSchemaTestClass, authInfo, promise);
        return promise.future();
    }

    void testBodyWithQueryParams(@javax.annotation.Nonnull String query, @javax.annotation.Nonnull User user, Handler<AsyncResult<Void>> handler);

    default Future<Void> testBodyWithQueryParams(@javax.annotation.Nonnull String query, @javax.annotation.Nonnull User user){
        Promise<Void> promise = Promise.promise();
        testBodyWithQueryParams(query, user, promise);
        return promise.future();
    }

    void testBodyWithQueryParams(@javax.annotation.Nonnull String query, @javax.annotation.Nonnull User user, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    default Future<Void> testBodyWithQueryParams(@javax.annotation.Nonnull String query, @javax.annotation.Nonnull User user, ApiClient.AuthInfo authInfo){
        Promise<Void> promise = Promise.promise();
        testBodyWithQueryParams(query, user, authInfo, promise);
        return promise.future();
    }

    void testClientModel(@javax.annotation.Nonnull Client client, Handler<AsyncResult<Client>> handler);

    default Future<Client> testClientModel(@javax.annotation.Nonnull Client client){
        Promise<Client> promise = Promise.promise();
        testClientModel(client, promise);
        return promise.future();
    }

    void testClientModel(@javax.annotation.Nonnull Client client, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Client>> handler);

    default Future<Client> testClientModel(@javax.annotation.Nonnull Client client, ApiClient.AuthInfo authInfo){
        Promise<Client> promise = Promise.promise();
        testClientModel(client, authInfo, promise);
        return promise.future();
    }

    void testEndpointParameters(@javax.annotation.Nonnull BigDecimal number, @javax.annotation.Nonnull Double _double, @javax.annotation.Nonnull String patternWithoutDelimiter, @javax.annotation.Nonnull byte[] _byte, @javax.annotation.Nullable Integer integer, @javax.annotation.Nullable Integer int32, @javax.annotation.Nullable Long int64, @javax.annotation.Nullable Float _float, @javax.annotation.Nullable String string, @javax.annotation.Nullable AsyncFile binary, @javax.annotation.Nullable LocalDate date, @javax.annotation.Nullable OffsetDateTime dateTime, @javax.annotation.Nullable String password, @javax.annotation.Nullable String paramCallback, Handler<AsyncResult<Void>> handler);

    default Future<Void> testEndpointParameters(@javax.annotation.Nonnull BigDecimal number, @javax.annotation.Nonnull Double _double, @javax.annotation.Nonnull String patternWithoutDelimiter, @javax.annotation.Nonnull byte[] _byte, @javax.annotation.Nullable Integer integer, @javax.annotation.Nullable Integer int32, @javax.annotation.Nullable Long int64, @javax.annotation.Nullable Float _float, @javax.annotation.Nullable String string, @javax.annotation.Nullable AsyncFile binary, @javax.annotation.Nullable LocalDate date, @javax.annotation.Nullable OffsetDateTime dateTime, @javax.annotation.Nullable String password, @javax.annotation.Nullable String paramCallback){
        Promise<Void> promise = Promise.promise();
        testEndpointParameters(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, paramCallback, promise);
        return promise.future();
    }

    void testEndpointParameters(@javax.annotation.Nonnull BigDecimal number, @javax.annotation.Nonnull Double _double, @javax.annotation.Nonnull String patternWithoutDelimiter, @javax.annotation.Nonnull byte[] _byte, @javax.annotation.Nullable Integer integer, @javax.annotation.Nullable Integer int32, @javax.annotation.Nullable Long int64, @javax.annotation.Nullable Float _float, @javax.annotation.Nullable String string, @javax.annotation.Nullable AsyncFile binary, @javax.annotation.Nullable LocalDate date, @javax.annotation.Nullable OffsetDateTime dateTime, @javax.annotation.Nullable String password, @javax.annotation.Nullable String paramCallback, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    default Future<Void> testEndpointParameters(@javax.annotation.Nonnull BigDecimal number, @javax.annotation.Nonnull Double _double, @javax.annotation.Nonnull String patternWithoutDelimiter, @javax.annotation.Nonnull byte[] _byte, @javax.annotation.Nullable Integer integer, @javax.annotation.Nullable Integer int32, @javax.annotation.Nullable Long int64, @javax.annotation.Nullable Float _float, @javax.annotation.Nullable String string, @javax.annotation.Nullable AsyncFile binary, @javax.annotation.Nullable LocalDate date, @javax.annotation.Nullable OffsetDateTime dateTime, @javax.annotation.Nullable String password, @javax.annotation.Nullable String paramCallback, ApiClient.AuthInfo authInfo){
        Promise<Void> promise = Promise.promise();
        testEndpointParameters(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, paramCallback, authInfo, promise);
        return promise.future();
    }

    void testEnumParameters(@javax.annotation.Nullable List<String> enumHeaderStringArray, @javax.annotation.Nullable String enumHeaderString, @javax.annotation.Nullable List<String> enumQueryStringArray, @javax.annotation.Nullable String enumQueryString, @javax.annotation.Nullable Integer enumQueryInteger, @javax.annotation.Nullable Double enumQueryDouble, @javax.annotation.Nullable List<EnumClass> enumQueryModelArray, @javax.annotation.Nullable List<String> enumFormStringArray, @javax.annotation.Nullable String enumFormString, Handler<AsyncResult<Void>> handler);

    default Future<Void> testEnumParameters(@javax.annotation.Nullable List<String> enumHeaderStringArray, @javax.annotation.Nullable String enumHeaderString, @javax.annotation.Nullable List<String> enumQueryStringArray, @javax.annotation.Nullable String enumQueryString, @javax.annotation.Nullable Integer enumQueryInteger, @javax.annotation.Nullable Double enumQueryDouble, @javax.annotation.Nullable List<EnumClass> enumQueryModelArray, @javax.annotation.Nullable List<String> enumFormStringArray, @javax.annotation.Nullable String enumFormString){
        Promise<Void> promise = Promise.promise();
        testEnumParameters(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumQueryModelArray, enumFormStringArray, enumFormString, promise);
        return promise.future();
    }

    void testEnumParameters(@javax.annotation.Nullable List<String> enumHeaderStringArray, @javax.annotation.Nullable String enumHeaderString, @javax.annotation.Nullable List<String> enumQueryStringArray, @javax.annotation.Nullable String enumQueryString, @javax.annotation.Nullable Integer enumQueryInteger, @javax.annotation.Nullable Double enumQueryDouble, @javax.annotation.Nullable List<EnumClass> enumQueryModelArray, @javax.annotation.Nullable List<String> enumFormStringArray, @javax.annotation.Nullable String enumFormString, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    default Future<Void> testEnumParameters(@javax.annotation.Nullable List<String> enumHeaderStringArray, @javax.annotation.Nullable String enumHeaderString, @javax.annotation.Nullable List<String> enumQueryStringArray, @javax.annotation.Nullable String enumQueryString, @javax.annotation.Nullable Integer enumQueryInteger, @javax.annotation.Nullable Double enumQueryDouble, @javax.annotation.Nullable List<EnumClass> enumQueryModelArray, @javax.annotation.Nullable List<String> enumFormStringArray, @javax.annotation.Nullable String enumFormString, ApiClient.AuthInfo authInfo){
        Promise<Void> promise = Promise.promise();
        testEnumParameters(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumQueryModelArray, enumFormStringArray, enumFormString, authInfo, promise);
        return promise.future();
    }

    void testGroupParameters(@javax.annotation.Nonnull Integer requiredStringGroup, @javax.annotation.Nonnull Boolean requiredBooleanGroup, @javax.annotation.Nonnull Long requiredInt64Group, @javax.annotation.Nullable Integer stringGroup, @javax.annotation.Nullable Boolean booleanGroup, @javax.annotation.Nullable Long int64Group, Handler<AsyncResult<Void>> handler);

    default Future<Void> testGroupParameters(@javax.annotation.Nonnull Integer requiredStringGroup, @javax.annotation.Nonnull Boolean requiredBooleanGroup, @javax.annotation.Nonnull Long requiredInt64Group, @javax.annotation.Nullable Integer stringGroup, @javax.annotation.Nullable Boolean booleanGroup, @javax.annotation.Nullable Long int64Group){
        Promise<Void> promise = Promise.promise();
        testGroupParameters(requiredStringGroup, requiredBooleanGroup, requiredInt64Group, stringGroup, booleanGroup, int64Group, promise);
        return promise.future();
    }

    void testGroupParameters(@javax.annotation.Nonnull Integer requiredStringGroup, @javax.annotation.Nonnull Boolean requiredBooleanGroup, @javax.annotation.Nonnull Long requiredInt64Group, @javax.annotation.Nullable Integer stringGroup, @javax.annotation.Nullable Boolean booleanGroup, @javax.annotation.Nullable Long int64Group, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    default Future<Void> testGroupParameters(@javax.annotation.Nonnull Integer requiredStringGroup, @javax.annotation.Nonnull Boolean requiredBooleanGroup, @javax.annotation.Nonnull Long requiredInt64Group, @javax.annotation.Nullable Integer stringGroup, @javax.annotation.Nullable Boolean booleanGroup, @javax.annotation.Nullable Long int64Group, ApiClient.AuthInfo authInfo){
        Promise<Void> promise = Promise.promise();
        testGroupParameters(requiredStringGroup, requiredBooleanGroup, requiredInt64Group, stringGroup, booleanGroup, int64Group, authInfo, promise);
        return promise.future();
    }

    void testInlineAdditionalProperties(@javax.annotation.Nonnull Map<String, String> requestBody, Handler<AsyncResult<Void>> handler);

    default Future<Void> testInlineAdditionalProperties(@javax.annotation.Nonnull Map<String, String> requestBody){
        Promise<Void> promise = Promise.promise();
        testInlineAdditionalProperties(requestBody, promise);
        return promise.future();
    }

    void testInlineAdditionalProperties(@javax.annotation.Nonnull Map<String, String> requestBody, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    default Future<Void> testInlineAdditionalProperties(@javax.annotation.Nonnull Map<String, String> requestBody, ApiClient.AuthInfo authInfo){
        Promise<Void> promise = Promise.promise();
        testInlineAdditionalProperties(requestBody, authInfo, promise);
        return promise.future();
    }

    void testInlineFreeformAdditionalProperties(@javax.annotation.Nonnull TestInlineFreeformAdditionalPropertiesRequest testInlineFreeformAdditionalPropertiesRequest, Handler<AsyncResult<Void>> handler);

    default Future<Void> testInlineFreeformAdditionalProperties(@javax.annotation.Nonnull TestInlineFreeformAdditionalPropertiesRequest testInlineFreeformAdditionalPropertiesRequest){
        Promise<Void> promise = Promise.promise();
        testInlineFreeformAdditionalProperties(testInlineFreeformAdditionalPropertiesRequest, promise);
        return promise.future();
    }

    void testInlineFreeformAdditionalProperties(@javax.annotation.Nonnull TestInlineFreeformAdditionalPropertiesRequest testInlineFreeformAdditionalPropertiesRequest, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    default Future<Void> testInlineFreeformAdditionalProperties(@javax.annotation.Nonnull TestInlineFreeformAdditionalPropertiesRequest testInlineFreeformAdditionalPropertiesRequest, ApiClient.AuthInfo authInfo){
        Promise<Void> promise = Promise.promise();
        testInlineFreeformAdditionalProperties(testInlineFreeformAdditionalPropertiesRequest, authInfo, promise);
        return promise.future();
    }

    void testJsonFormData(@javax.annotation.Nonnull String param, @javax.annotation.Nonnull String param2, Handler<AsyncResult<Void>> handler);

    default Future<Void> testJsonFormData(@javax.annotation.Nonnull String param, @javax.annotation.Nonnull String param2){
        Promise<Void> promise = Promise.promise();
        testJsonFormData(param, param2, promise);
        return promise.future();
    }

    void testJsonFormData(@javax.annotation.Nonnull String param, @javax.annotation.Nonnull String param2, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    default Future<Void> testJsonFormData(@javax.annotation.Nonnull String param, @javax.annotation.Nonnull String param2, ApiClient.AuthInfo authInfo){
        Promise<Void> promise = Promise.promise();
        testJsonFormData(param, param2, authInfo, promise);
        return promise.future();
    }

    void testNullable(@javax.annotation.Nonnull ChildWithNullable childWithNullable, Handler<AsyncResult<Void>> handler);

    default Future<Void> testNullable(@javax.annotation.Nonnull ChildWithNullable childWithNullable){
        Promise<Void> promise = Promise.promise();
        testNullable(childWithNullable, promise);
        return promise.future();
    }

    void testNullable(@javax.annotation.Nonnull ChildWithNullable childWithNullable, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    default Future<Void> testNullable(@javax.annotation.Nonnull ChildWithNullable childWithNullable, ApiClient.AuthInfo authInfo){
        Promise<Void> promise = Promise.promise();
        testNullable(childWithNullable, authInfo, promise);
        return promise.future();
    }

    void testQueryParameterCollectionFormat(@javax.annotation.Nonnull List<String> pipe, @javax.annotation.Nonnull List<String> ioutil, @javax.annotation.Nonnull List<String> http, @javax.annotation.Nonnull List<String> url, @javax.annotation.Nonnull List<String> context, @javax.annotation.Nonnull String allowEmpty, @javax.annotation.Nullable Map<String, String> language, Handler<AsyncResult<Void>> handler);

    default Future<Void> testQueryParameterCollectionFormat(@javax.annotation.Nonnull List<String> pipe, @javax.annotation.Nonnull List<String> ioutil, @javax.annotation.Nonnull List<String> http, @javax.annotation.Nonnull List<String> url, @javax.annotation.Nonnull List<String> context, @javax.annotation.Nonnull String allowEmpty, @javax.annotation.Nullable Map<String, String> language){
        Promise<Void> promise = Promise.promise();
        testQueryParameterCollectionFormat(pipe, ioutil, http, url, context, allowEmpty, language, promise);
        return promise.future();
    }

    void testQueryParameterCollectionFormat(@javax.annotation.Nonnull List<String> pipe, @javax.annotation.Nonnull List<String> ioutil, @javax.annotation.Nonnull List<String> http, @javax.annotation.Nonnull List<String> url, @javax.annotation.Nonnull List<String> context, @javax.annotation.Nonnull String allowEmpty, @javax.annotation.Nullable Map<String, String> language, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    default Future<Void> testQueryParameterCollectionFormat(@javax.annotation.Nonnull List<String> pipe, @javax.annotation.Nonnull List<String> ioutil, @javax.annotation.Nonnull List<String> http, @javax.annotation.Nonnull List<String> url, @javax.annotation.Nonnull List<String> context, @javax.annotation.Nonnull String allowEmpty, @javax.annotation.Nullable Map<String, String> language, ApiClient.AuthInfo authInfo){
        Promise<Void> promise = Promise.promise();
        testQueryParameterCollectionFormat(pipe, ioutil, http, url, context, allowEmpty, language, authInfo, promise);
        return promise.future();
    }

    void testStringMapReference(@javax.annotation.Nonnull Map<String, String> requestBody, Handler<AsyncResult<Void>> handler);

    default Future<Void> testStringMapReference(@javax.annotation.Nonnull Map<String, String> requestBody){
        Promise<Void> promise = Promise.promise();
        testStringMapReference(requestBody, promise);
        return promise.future();
    }

    void testStringMapReference(@javax.annotation.Nonnull Map<String, String> requestBody, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    default Future<Void> testStringMapReference(@javax.annotation.Nonnull Map<String, String> requestBody, ApiClient.AuthInfo authInfo){
        Promise<Void> promise = Promise.promise();
        testStringMapReference(requestBody, authInfo, promise);
        return promise.future();
    }

}
