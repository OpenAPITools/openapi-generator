package org.openapitools.client.api.rxjava;

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
import org.openapitools.client.ApiClient;

import java.util.*;

import rx.Single;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen")
public class FakeApi {

    private final org.openapitools.client.api.FakeApi delegate;

    public FakeApi(org.openapitools.client.api.FakeApi delegate) {
        this.delegate = delegate;
    }

    public org.openapitools.client.api.FakeApi getDelegate() {
        return delegate;
    }

    /**
    * 
    * for Java apache and Java native, test toUrlQueryString for maps with BegDecimal keys
    * @param resultHandler Asynchronous result handler
    */
    public void fakeBigDecimalMap(Handler<AsyncResult<FakeBigDecimalMap200Response>> resultHandler) {
        delegate.fakeBigDecimalMap(resultHandler);
    }

    /**
    * 
    * for Java apache and Java native, test toUrlQueryString for maps with BegDecimal keys
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void fakeBigDecimalMap(ApiClient.AuthInfo authInfo, Handler<AsyncResult<FakeBigDecimalMap200Response>> resultHandler) {
        delegate.fakeBigDecimalMap(authInfo, resultHandler);
    }

    /**
    * 
    * for Java apache and Java native, test toUrlQueryString for maps with BegDecimal keys
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<FakeBigDecimalMap200Response> rxFakeBigDecimalMap() {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.fakeBigDecimalMap(fut)
        ));
    }

    /**
    * 
    * for Java apache and Java native, test toUrlQueryString for maps with BegDecimal keys
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<FakeBigDecimalMap200Response> rxFakeBigDecimalMap(ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.fakeBigDecimalMap(authInfo, fut)
        ));
    }
    /**
    * Health check endpoint
    * 
    * @param resultHandler Asynchronous result handler
    */
    public void fakeHealthGet(Handler<AsyncResult<HealthCheckResult>> resultHandler) {
        delegate.fakeHealthGet(resultHandler);
    }

    /**
    * Health check endpoint
    * 
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void fakeHealthGet(ApiClient.AuthInfo authInfo, Handler<AsyncResult<HealthCheckResult>> resultHandler) {
        delegate.fakeHealthGet(authInfo, resultHandler);
    }

    /**
    * Health check endpoint
    * 
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<HealthCheckResult> rxFakeHealthGet() {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.fakeHealthGet(fut)
        ));
    }

    /**
    * Health check endpoint
    * 
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<HealthCheckResult> rxFakeHealthGet(ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.fakeHealthGet(authInfo, fut)
        ));
    }
    /**
    * test http signature authentication
    * 
    * @param pet Pet object that needs to be added to the store (required)
    * @param query1 query parameter (optional)
    * @param header1 header parameter (optional)
    * @param resultHandler Asynchronous result handler
    */
    public void fakeHttpSignatureTest(Pet pet, String query1, String header1, Handler<AsyncResult<Void>> resultHandler) {
        delegate.fakeHttpSignatureTest(pet, query1, header1, resultHandler);
    }

    /**
    * test http signature authentication
    * 
    * @param pet Pet object that needs to be added to the store (required)
    * @param query1 query parameter (optional)
    * @param header1 header parameter (optional)
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void fakeHttpSignatureTest(Pet pet, String query1, String header1, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
        delegate.fakeHttpSignatureTest(pet, query1, header1, authInfo, resultHandler);
    }

    /**
    * test http signature authentication
    * 
    * @param pet Pet object that needs to be added to the store (required)
    * @param query1 query parameter (optional)
    * @param header1 header parameter (optional)
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxFakeHttpSignatureTest(Pet pet, String query1, String header1) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.fakeHttpSignatureTest(pet, query1, header1, fut)
        ));
    }

    /**
    * test http signature authentication
    * 
    * @param pet Pet object that needs to be added to the store (required)
    * @param query1 query parameter (optional)
    * @param header1 header parameter (optional)
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxFakeHttpSignatureTest(Pet pet, String query1, String header1, ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.fakeHttpSignatureTest(pet, query1, header1, authInfo, fut)
        ));
    }
    /**
    * 
    * Test serialization of outer boolean types
    * @param body Input boolean as post body (optional)
    * @param resultHandler Asynchronous result handler
    */
    public void fakeOuterBooleanSerialize(Boolean body, Handler<AsyncResult<Boolean>> resultHandler) {
        delegate.fakeOuterBooleanSerialize(body, resultHandler);
    }

    /**
    * 
    * Test serialization of outer boolean types
    * @param body Input boolean as post body (optional)
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void fakeOuterBooleanSerialize(Boolean body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Boolean>> resultHandler) {
        delegate.fakeOuterBooleanSerialize(body, authInfo, resultHandler);
    }

    /**
    * 
    * Test serialization of outer boolean types
    * @param body Input boolean as post body (optional)
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Boolean> rxFakeOuterBooleanSerialize(Boolean body) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.fakeOuterBooleanSerialize(body, fut)
        ));
    }

    /**
    * 
    * Test serialization of outer boolean types
    * @param body Input boolean as post body (optional)
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Boolean> rxFakeOuterBooleanSerialize(Boolean body, ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.fakeOuterBooleanSerialize(body, authInfo, fut)
        ));
    }
    /**
    * 
    * Test serialization of object with outer number type
    * @param outerComposite Input composite as post body (optional)
    * @param resultHandler Asynchronous result handler
    */
    public void fakeOuterCompositeSerialize(OuterComposite outerComposite, Handler<AsyncResult<OuterComposite>> resultHandler) {
        delegate.fakeOuterCompositeSerialize(outerComposite, resultHandler);
    }

    /**
    * 
    * Test serialization of object with outer number type
    * @param outerComposite Input composite as post body (optional)
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void fakeOuterCompositeSerialize(OuterComposite outerComposite, ApiClient.AuthInfo authInfo, Handler<AsyncResult<OuterComposite>> resultHandler) {
        delegate.fakeOuterCompositeSerialize(outerComposite, authInfo, resultHandler);
    }

    /**
    * 
    * Test serialization of object with outer number type
    * @param outerComposite Input composite as post body (optional)
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<OuterComposite> rxFakeOuterCompositeSerialize(OuterComposite outerComposite) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.fakeOuterCompositeSerialize(outerComposite, fut)
        ));
    }

    /**
    * 
    * Test serialization of object with outer number type
    * @param outerComposite Input composite as post body (optional)
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<OuterComposite> rxFakeOuterCompositeSerialize(OuterComposite outerComposite, ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.fakeOuterCompositeSerialize(outerComposite, authInfo, fut)
        ));
    }
    /**
    * 
    * Test serialization of outer number types
    * @param body Input number as post body (optional)
    * @param resultHandler Asynchronous result handler
    */
    public void fakeOuterNumberSerialize(BigDecimal body, Handler<AsyncResult<BigDecimal>> resultHandler) {
        delegate.fakeOuterNumberSerialize(body, resultHandler);
    }

    /**
    * 
    * Test serialization of outer number types
    * @param body Input number as post body (optional)
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void fakeOuterNumberSerialize(BigDecimal body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<BigDecimal>> resultHandler) {
        delegate.fakeOuterNumberSerialize(body, authInfo, resultHandler);
    }

    /**
    * 
    * Test serialization of outer number types
    * @param body Input number as post body (optional)
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<BigDecimal> rxFakeOuterNumberSerialize(BigDecimal body) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.fakeOuterNumberSerialize(body, fut)
        ));
    }

    /**
    * 
    * Test serialization of outer number types
    * @param body Input number as post body (optional)
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<BigDecimal> rxFakeOuterNumberSerialize(BigDecimal body, ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.fakeOuterNumberSerialize(body, authInfo, fut)
        ));
    }
    /**
    * 
    * Test serialization of outer string types
    * @param body Input string as post body (optional)
    * @param resultHandler Asynchronous result handler
    */
    public void fakeOuterStringSerialize(String body, Handler<AsyncResult<String>> resultHandler) {
        delegate.fakeOuterStringSerialize(body, resultHandler);
    }

    /**
    * 
    * Test serialization of outer string types
    * @param body Input string as post body (optional)
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void fakeOuterStringSerialize(String body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<String>> resultHandler) {
        delegate.fakeOuterStringSerialize(body, authInfo, resultHandler);
    }

    /**
    * 
    * Test serialization of outer string types
    * @param body Input string as post body (optional)
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<String> rxFakeOuterStringSerialize(String body) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.fakeOuterStringSerialize(body, fut)
        ));
    }

    /**
    * 
    * Test serialization of outer string types
    * @param body Input string as post body (optional)
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<String> rxFakeOuterStringSerialize(String body, ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.fakeOuterStringSerialize(body, authInfo, fut)
        ));
    }
    /**
    * 
    * Test serialization of enum (int) properties with examples
    * @param outerObjectWithEnumProperty Input enum (int) as post body (required)
    * @param resultHandler Asynchronous result handler
    */
    public void fakePropertyEnumIntegerSerialize(OuterObjectWithEnumProperty outerObjectWithEnumProperty, Handler<AsyncResult<OuterObjectWithEnumProperty>> resultHandler) {
        delegate.fakePropertyEnumIntegerSerialize(outerObjectWithEnumProperty, resultHandler);
    }

    /**
    * 
    * Test serialization of enum (int) properties with examples
    * @param outerObjectWithEnumProperty Input enum (int) as post body (required)
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void fakePropertyEnumIntegerSerialize(OuterObjectWithEnumProperty outerObjectWithEnumProperty, ApiClient.AuthInfo authInfo, Handler<AsyncResult<OuterObjectWithEnumProperty>> resultHandler) {
        delegate.fakePropertyEnumIntegerSerialize(outerObjectWithEnumProperty, authInfo, resultHandler);
    }

    /**
    * 
    * Test serialization of enum (int) properties with examples
    * @param outerObjectWithEnumProperty Input enum (int) as post body (required)
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<OuterObjectWithEnumProperty> rxFakePropertyEnumIntegerSerialize(OuterObjectWithEnumProperty outerObjectWithEnumProperty) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.fakePropertyEnumIntegerSerialize(outerObjectWithEnumProperty, fut)
        ));
    }

    /**
    * 
    * Test serialization of enum (int) properties with examples
    * @param outerObjectWithEnumProperty Input enum (int) as post body (required)
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<OuterObjectWithEnumProperty> rxFakePropertyEnumIntegerSerialize(OuterObjectWithEnumProperty outerObjectWithEnumProperty, ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.fakePropertyEnumIntegerSerialize(outerObjectWithEnumProperty, authInfo, fut)
        ));
    }
    /**
    * 
    * For this test, the body has to be a binary file.
    * @param body image to upload (required)
    * @param resultHandler Asynchronous result handler
    */
    public void testBodyWithBinary(AsyncFile body, Handler<AsyncResult<Void>> resultHandler) {
        delegate.testBodyWithBinary(body, resultHandler);
    }

    /**
    * 
    * For this test, the body has to be a binary file.
    * @param body image to upload (required)
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void testBodyWithBinary(AsyncFile body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
        delegate.testBodyWithBinary(body, authInfo, resultHandler);
    }

    /**
    * 
    * For this test, the body has to be a binary file.
    * @param body image to upload (required)
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxTestBodyWithBinary(AsyncFile body) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.testBodyWithBinary(body, fut)
        ));
    }

    /**
    * 
    * For this test, the body has to be a binary file.
    * @param body image to upload (required)
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxTestBodyWithBinary(AsyncFile body, ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.testBodyWithBinary(body, authInfo, fut)
        ));
    }
    /**
    * 
    * For this test, the body for this request must reference a schema named &#x60;File&#x60;.
    * @param fileSchemaTestClass  (required)
    * @param resultHandler Asynchronous result handler
    */
    public void testBodyWithFileSchema(FileSchemaTestClass fileSchemaTestClass, Handler<AsyncResult<Void>> resultHandler) {
        delegate.testBodyWithFileSchema(fileSchemaTestClass, resultHandler);
    }

    /**
    * 
    * For this test, the body for this request must reference a schema named &#x60;File&#x60;.
    * @param fileSchemaTestClass  (required)
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void testBodyWithFileSchema(FileSchemaTestClass fileSchemaTestClass, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
        delegate.testBodyWithFileSchema(fileSchemaTestClass, authInfo, resultHandler);
    }

    /**
    * 
    * For this test, the body for this request must reference a schema named &#x60;File&#x60;.
    * @param fileSchemaTestClass  (required)
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxTestBodyWithFileSchema(FileSchemaTestClass fileSchemaTestClass) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.testBodyWithFileSchema(fileSchemaTestClass, fut)
        ));
    }

    /**
    * 
    * For this test, the body for this request must reference a schema named &#x60;File&#x60;.
    * @param fileSchemaTestClass  (required)
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxTestBodyWithFileSchema(FileSchemaTestClass fileSchemaTestClass, ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.testBodyWithFileSchema(fileSchemaTestClass, authInfo, fut)
        ));
    }
    /**
    * 
    * 
    * @param query  (required)
    * @param user  (required)
    * @param resultHandler Asynchronous result handler
    */
    public void testBodyWithQueryParams(String query, User user, Handler<AsyncResult<Void>> resultHandler) {
        delegate.testBodyWithQueryParams(query, user, resultHandler);
    }

    /**
    * 
    * 
    * @param query  (required)
    * @param user  (required)
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void testBodyWithQueryParams(String query, User user, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
        delegate.testBodyWithQueryParams(query, user, authInfo, resultHandler);
    }

    /**
    * 
    * 
    * @param query  (required)
    * @param user  (required)
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxTestBodyWithQueryParams(String query, User user) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.testBodyWithQueryParams(query, user, fut)
        ));
    }

    /**
    * 
    * 
    * @param query  (required)
    * @param user  (required)
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxTestBodyWithQueryParams(String query, User user, ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.testBodyWithQueryParams(query, user, authInfo, fut)
        ));
    }
    /**
    * To test \&quot;client\&quot; model
    * To test \&quot;client\&quot; model
    * @param client client model (required)
    * @param resultHandler Asynchronous result handler
    */
    public void testClientModel(Client client, Handler<AsyncResult<Client>> resultHandler) {
        delegate.testClientModel(client, resultHandler);
    }

    /**
    * To test \&quot;client\&quot; model
    * To test \&quot;client\&quot; model
    * @param client client model (required)
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void testClientModel(Client client, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Client>> resultHandler) {
        delegate.testClientModel(client, authInfo, resultHandler);
    }

    /**
    * To test \&quot;client\&quot; model
    * To test \&quot;client\&quot; model
    * @param client client model (required)
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Client> rxTestClientModel(Client client) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.testClientModel(client, fut)
        ));
    }

    /**
    * To test \&quot;client\&quot; model
    * To test \&quot;client\&quot; model
    * @param client client model (required)
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Client> rxTestClientModel(Client client, ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.testClientModel(client, authInfo, fut)
        ));
    }
    /**
    * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
    * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
    * @param number None (required)
    * @param _double None (required)
    * @param patternWithoutDelimiter None (required)
    * @param _byte None (required)
    * @param integer None (optional)
    * @param int32 None (optional)
    * @param int64 None (optional)
    * @param _float None (optional)
    * @param string None (optional)
    * @param binary None (optional)
    * @param date None (optional)
    * @param dateTime None (optional)
    * @param password None (optional)
    * @param paramCallback None (optional)
    * @param resultHandler Asynchronous result handler
    */
    public void testEndpointParameters(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string, AsyncFile binary, LocalDate date, OffsetDateTime dateTime, String password, String paramCallback, Handler<AsyncResult<Void>> resultHandler) {
        delegate.testEndpointParameters(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, paramCallback, resultHandler);
    }

    /**
    * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
    * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
    * @param number None (required)
    * @param _double None (required)
    * @param patternWithoutDelimiter None (required)
    * @param _byte None (required)
    * @param integer None (optional)
    * @param int32 None (optional)
    * @param int64 None (optional)
    * @param _float None (optional)
    * @param string None (optional)
    * @param binary None (optional)
    * @param date None (optional)
    * @param dateTime None (optional)
    * @param password None (optional)
    * @param paramCallback None (optional)
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void testEndpointParameters(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string, AsyncFile binary, LocalDate date, OffsetDateTime dateTime, String password, String paramCallback, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
        delegate.testEndpointParameters(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, paramCallback, authInfo, resultHandler);
    }

    /**
    * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
    * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
    * @param number None (required)
    * @param _double None (required)
    * @param patternWithoutDelimiter None (required)
    * @param _byte None (required)
    * @param integer None (optional)
    * @param int32 None (optional)
    * @param int64 None (optional)
    * @param _float None (optional)
    * @param string None (optional)
    * @param binary None (optional)
    * @param date None (optional)
    * @param dateTime None (optional)
    * @param password None (optional)
    * @param paramCallback None (optional)
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxTestEndpointParameters(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string, AsyncFile binary, LocalDate date, OffsetDateTime dateTime, String password, String paramCallback) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.testEndpointParameters(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, paramCallback, fut)
        ));
    }

    /**
    * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
    * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
    * @param number None (required)
    * @param _double None (required)
    * @param patternWithoutDelimiter None (required)
    * @param _byte None (required)
    * @param integer None (optional)
    * @param int32 None (optional)
    * @param int64 None (optional)
    * @param _float None (optional)
    * @param string None (optional)
    * @param binary None (optional)
    * @param date None (optional)
    * @param dateTime None (optional)
    * @param password None (optional)
    * @param paramCallback None (optional)
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxTestEndpointParameters(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string, AsyncFile binary, LocalDate date, OffsetDateTime dateTime, String password, String paramCallback, ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.testEndpointParameters(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, paramCallback, authInfo, fut)
        ));
    }
    /**
    * To test enum parameters
    * To test enum parameters
    * @param enumHeaderStringArray Header parameter enum test (string array) (optional)
    * @param enumHeaderString Header parameter enum test (string) (optional, default to -efg)
    * @param enumQueryStringArray Query parameter enum test (string array) (optional)
    * @param enumQueryString Query parameter enum test (string) (optional, default to -efg)
    * @param enumQueryInteger Query parameter enum test (double) (optional)
    * @param enumQueryDouble Query parameter enum test (double) (optional)
    * @param enumQueryModelArray  (optional)
    * @param enumFormStringArray Form parameter enum test (string array) (optional, default to $)
    * @param enumFormString Form parameter enum test (string) (optional, default to -efg)
    * @param resultHandler Asynchronous result handler
    */
    public void testEnumParameters(List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble, List<EnumClass> enumQueryModelArray, List<String> enumFormStringArray, String enumFormString, Handler<AsyncResult<Void>> resultHandler) {
        delegate.testEnumParameters(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumQueryModelArray, enumFormStringArray, enumFormString, resultHandler);
    }

    /**
    * To test enum parameters
    * To test enum parameters
    * @param enumHeaderStringArray Header parameter enum test (string array) (optional)
    * @param enumHeaderString Header parameter enum test (string) (optional, default to -efg)
    * @param enumQueryStringArray Query parameter enum test (string array) (optional)
    * @param enumQueryString Query parameter enum test (string) (optional, default to -efg)
    * @param enumQueryInteger Query parameter enum test (double) (optional)
    * @param enumQueryDouble Query parameter enum test (double) (optional)
    * @param enumQueryModelArray  (optional)
    * @param enumFormStringArray Form parameter enum test (string array) (optional, default to $)
    * @param enumFormString Form parameter enum test (string) (optional, default to -efg)
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void testEnumParameters(List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble, List<EnumClass> enumQueryModelArray, List<String> enumFormStringArray, String enumFormString, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
        delegate.testEnumParameters(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumQueryModelArray, enumFormStringArray, enumFormString, authInfo, resultHandler);
    }

    /**
    * To test enum parameters
    * To test enum parameters
    * @param enumHeaderStringArray Header parameter enum test (string array) (optional)
    * @param enumHeaderString Header parameter enum test (string) (optional, default to -efg)
    * @param enumQueryStringArray Query parameter enum test (string array) (optional)
    * @param enumQueryString Query parameter enum test (string) (optional, default to -efg)
    * @param enumQueryInteger Query parameter enum test (double) (optional)
    * @param enumQueryDouble Query parameter enum test (double) (optional)
    * @param enumQueryModelArray  (optional)
    * @param enumFormStringArray Form parameter enum test (string array) (optional, default to $)
    * @param enumFormString Form parameter enum test (string) (optional, default to -efg)
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxTestEnumParameters(List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble, List<EnumClass> enumQueryModelArray, List<String> enumFormStringArray, String enumFormString) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.testEnumParameters(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumQueryModelArray, enumFormStringArray, enumFormString, fut)
        ));
    }

    /**
    * To test enum parameters
    * To test enum parameters
    * @param enumHeaderStringArray Header parameter enum test (string array) (optional)
    * @param enumHeaderString Header parameter enum test (string) (optional, default to -efg)
    * @param enumQueryStringArray Query parameter enum test (string array) (optional)
    * @param enumQueryString Query parameter enum test (string) (optional, default to -efg)
    * @param enumQueryInteger Query parameter enum test (double) (optional)
    * @param enumQueryDouble Query parameter enum test (double) (optional)
    * @param enumQueryModelArray  (optional)
    * @param enumFormStringArray Form parameter enum test (string array) (optional, default to $)
    * @param enumFormString Form parameter enum test (string) (optional, default to -efg)
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxTestEnumParameters(List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble, List<EnumClass> enumQueryModelArray, List<String> enumFormStringArray, String enumFormString, ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.testEnumParameters(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumQueryModelArray, enumFormStringArray, enumFormString, authInfo, fut)
        ));
    }
    /**
    * Fake endpoint to test group parameters (optional)
    * Fake endpoint to test group parameters (optional)
    * @param requiredStringGroup Required String in group parameters (required)
    * @param requiredBooleanGroup Required Boolean in group parameters (required)
    * @param requiredInt64Group Required Integer in group parameters (required)
    * @param stringGroup String in group parameters (optional)
    * @param booleanGroup Boolean in group parameters (optional)
    * @param int64Group Integer in group parameters (optional)
    * @param resultHandler Asynchronous result handler
    */
    public void testGroupParameters(Integer requiredStringGroup, Boolean requiredBooleanGroup, Long requiredInt64Group, Integer stringGroup, Boolean booleanGroup, Long int64Group, Handler<AsyncResult<Void>> resultHandler) {
        delegate.testGroupParameters(requiredStringGroup, requiredBooleanGroup, requiredInt64Group, stringGroup, booleanGroup, int64Group, resultHandler);
    }

    /**
    * Fake endpoint to test group parameters (optional)
    * Fake endpoint to test group parameters (optional)
    * @param requiredStringGroup Required String in group parameters (required)
    * @param requiredBooleanGroup Required Boolean in group parameters (required)
    * @param requiredInt64Group Required Integer in group parameters (required)
    * @param stringGroup String in group parameters (optional)
    * @param booleanGroup Boolean in group parameters (optional)
    * @param int64Group Integer in group parameters (optional)
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void testGroupParameters(Integer requiredStringGroup, Boolean requiredBooleanGroup, Long requiredInt64Group, Integer stringGroup, Boolean booleanGroup, Long int64Group, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
        delegate.testGroupParameters(requiredStringGroup, requiredBooleanGroup, requiredInt64Group, stringGroup, booleanGroup, int64Group, authInfo, resultHandler);
    }

    /**
    * Fake endpoint to test group parameters (optional)
    * Fake endpoint to test group parameters (optional)
    * @param requiredStringGroup Required String in group parameters (required)
    * @param requiredBooleanGroup Required Boolean in group parameters (required)
    * @param requiredInt64Group Required Integer in group parameters (required)
    * @param stringGroup String in group parameters (optional)
    * @param booleanGroup Boolean in group parameters (optional)
    * @param int64Group Integer in group parameters (optional)
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxTestGroupParameters(Integer requiredStringGroup, Boolean requiredBooleanGroup, Long requiredInt64Group, Integer stringGroup, Boolean booleanGroup, Long int64Group) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.testGroupParameters(requiredStringGroup, requiredBooleanGroup, requiredInt64Group, stringGroup, booleanGroup, int64Group, fut)
        ));
    }

    /**
    * Fake endpoint to test group parameters (optional)
    * Fake endpoint to test group parameters (optional)
    * @param requiredStringGroup Required String in group parameters (required)
    * @param requiredBooleanGroup Required Boolean in group parameters (required)
    * @param requiredInt64Group Required Integer in group parameters (required)
    * @param stringGroup String in group parameters (optional)
    * @param booleanGroup Boolean in group parameters (optional)
    * @param int64Group Integer in group parameters (optional)
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxTestGroupParameters(Integer requiredStringGroup, Boolean requiredBooleanGroup, Long requiredInt64Group, Integer stringGroup, Boolean booleanGroup, Long int64Group, ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.testGroupParameters(requiredStringGroup, requiredBooleanGroup, requiredInt64Group, stringGroup, booleanGroup, int64Group, authInfo, fut)
        ));
    }
    /**
    * test inline additionalProperties
    * 
    * @param requestBody request body (required)
    * @param resultHandler Asynchronous result handler
    */
    public void testInlineAdditionalProperties(Map<String, String> requestBody, Handler<AsyncResult<Void>> resultHandler) {
        delegate.testInlineAdditionalProperties(requestBody, resultHandler);
    }

    /**
    * test inline additionalProperties
    * 
    * @param requestBody request body (required)
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void testInlineAdditionalProperties(Map<String, String> requestBody, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
        delegate.testInlineAdditionalProperties(requestBody, authInfo, resultHandler);
    }

    /**
    * test inline additionalProperties
    * 
    * @param requestBody request body (required)
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxTestInlineAdditionalProperties(Map<String, String> requestBody) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.testInlineAdditionalProperties(requestBody, fut)
        ));
    }

    /**
    * test inline additionalProperties
    * 
    * @param requestBody request body (required)
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxTestInlineAdditionalProperties(Map<String, String> requestBody, ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.testInlineAdditionalProperties(requestBody, authInfo, fut)
        ));
    }
    /**
    * test inline free-form additionalProperties
    * 
    * @param testInlineFreeformAdditionalPropertiesRequest request body (required)
    * @param resultHandler Asynchronous result handler
    */
    public void testInlineFreeformAdditionalProperties(TestInlineFreeformAdditionalPropertiesRequest testInlineFreeformAdditionalPropertiesRequest, Handler<AsyncResult<Void>> resultHandler) {
        delegate.testInlineFreeformAdditionalProperties(testInlineFreeformAdditionalPropertiesRequest, resultHandler);
    }

    /**
    * test inline free-form additionalProperties
    * 
    * @param testInlineFreeformAdditionalPropertiesRequest request body (required)
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void testInlineFreeformAdditionalProperties(TestInlineFreeformAdditionalPropertiesRequest testInlineFreeformAdditionalPropertiesRequest, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
        delegate.testInlineFreeformAdditionalProperties(testInlineFreeformAdditionalPropertiesRequest, authInfo, resultHandler);
    }

    /**
    * test inline free-form additionalProperties
    * 
    * @param testInlineFreeformAdditionalPropertiesRequest request body (required)
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxTestInlineFreeformAdditionalProperties(TestInlineFreeformAdditionalPropertiesRequest testInlineFreeformAdditionalPropertiesRequest) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.testInlineFreeformAdditionalProperties(testInlineFreeformAdditionalPropertiesRequest, fut)
        ));
    }

    /**
    * test inline free-form additionalProperties
    * 
    * @param testInlineFreeformAdditionalPropertiesRequest request body (required)
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxTestInlineFreeformAdditionalProperties(TestInlineFreeformAdditionalPropertiesRequest testInlineFreeformAdditionalPropertiesRequest, ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.testInlineFreeformAdditionalProperties(testInlineFreeformAdditionalPropertiesRequest, authInfo, fut)
        ));
    }
    /**
    * test json serialization of form data
    * 
    * @param param field1 (required)
    * @param param2 field2 (required)
    * @param resultHandler Asynchronous result handler
    */
    public void testJsonFormData(String param, String param2, Handler<AsyncResult<Void>> resultHandler) {
        delegate.testJsonFormData(param, param2, resultHandler);
    }

    /**
    * test json serialization of form data
    * 
    * @param param field1 (required)
    * @param param2 field2 (required)
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void testJsonFormData(String param, String param2, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
        delegate.testJsonFormData(param, param2, authInfo, resultHandler);
    }

    /**
    * test json serialization of form data
    * 
    * @param param field1 (required)
    * @param param2 field2 (required)
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxTestJsonFormData(String param, String param2) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.testJsonFormData(param, param2, fut)
        ));
    }

    /**
    * test json serialization of form data
    * 
    * @param param field1 (required)
    * @param param2 field2 (required)
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxTestJsonFormData(String param, String param2, ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.testJsonFormData(param, param2, authInfo, fut)
        ));
    }
    /**
    * test nullable parent property
    * 
    * @param childWithNullable request body (required)
    * @param resultHandler Asynchronous result handler
    */
    public void testNullable(ChildWithNullable childWithNullable, Handler<AsyncResult<Void>> resultHandler) {
        delegate.testNullable(childWithNullable, resultHandler);
    }

    /**
    * test nullable parent property
    * 
    * @param childWithNullable request body (required)
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void testNullable(ChildWithNullable childWithNullable, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
        delegate.testNullable(childWithNullable, authInfo, resultHandler);
    }

    /**
    * test nullable parent property
    * 
    * @param childWithNullable request body (required)
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxTestNullable(ChildWithNullable childWithNullable) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.testNullable(childWithNullable, fut)
        ));
    }

    /**
    * test nullable parent property
    * 
    * @param childWithNullable request body (required)
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxTestNullable(ChildWithNullable childWithNullable, ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.testNullable(childWithNullable, authInfo, fut)
        ));
    }
    /**
    * 
    * To test the collection format in query parameters
    * @param pipe  (required)
    * @param ioutil  (required)
    * @param http  (required)
    * @param url  (required)
    * @param context  (required)
    * @param allowEmpty  (required)
    * @param language  (optional)
    * @param resultHandler Asynchronous result handler
    */
    public void testQueryParameterCollectionFormat(List<String> pipe, List<String> ioutil, List<String> http, List<String> url, List<String> context, String allowEmpty, Map<String, String> language, Handler<AsyncResult<Void>> resultHandler) {
        delegate.testQueryParameterCollectionFormat(pipe, ioutil, http, url, context, allowEmpty, language, resultHandler);
    }

    /**
    * 
    * To test the collection format in query parameters
    * @param pipe  (required)
    * @param ioutil  (required)
    * @param http  (required)
    * @param url  (required)
    * @param context  (required)
    * @param allowEmpty  (required)
    * @param language  (optional)
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void testQueryParameterCollectionFormat(List<String> pipe, List<String> ioutil, List<String> http, List<String> url, List<String> context, String allowEmpty, Map<String, String> language, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
        delegate.testQueryParameterCollectionFormat(pipe, ioutil, http, url, context, allowEmpty, language, authInfo, resultHandler);
    }

    /**
    * 
    * To test the collection format in query parameters
    * @param pipe  (required)
    * @param ioutil  (required)
    * @param http  (required)
    * @param url  (required)
    * @param context  (required)
    * @param allowEmpty  (required)
    * @param language  (optional)
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxTestQueryParameterCollectionFormat(List<String> pipe, List<String> ioutil, List<String> http, List<String> url, List<String> context, String allowEmpty, Map<String, String> language) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.testQueryParameterCollectionFormat(pipe, ioutil, http, url, context, allowEmpty, language, fut)
        ));
    }

    /**
    * 
    * To test the collection format in query parameters
    * @param pipe  (required)
    * @param ioutil  (required)
    * @param http  (required)
    * @param url  (required)
    * @param context  (required)
    * @param allowEmpty  (required)
    * @param language  (optional)
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxTestQueryParameterCollectionFormat(List<String> pipe, List<String> ioutil, List<String> http, List<String> url, List<String> context, String allowEmpty, Map<String, String> language, ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.testQueryParameterCollectionFormat(pipe, ioutil, http, url, context, allowEmpty, language, authInfo, fut)
        ));
    }

    public static FakeApi newInstance(org.openapitools.client.api.FakeApi arg) {
        return arg != null ? new FakeApi(arg) : null;
    }
}
