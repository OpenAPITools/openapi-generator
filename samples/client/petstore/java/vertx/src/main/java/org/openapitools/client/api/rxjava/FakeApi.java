package org.openapitools.client.api.rxjava;

import io.vertx.core.file.AsyncFile;
import java.math.BigDecimal;
import org.openapitools.client.model.Client;
import org.openapitools.client.model.FileSchemaTestClass;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import org.openapitools.client.model.OuterComposite;
import org.openapitools.client.model.User;
import org.openapitools.client.model.XmlItem;
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
    * creates an XmlItem
    * this route creates an XmlItem
    * @param xmlItem XmlItem Body (required)
    * @param resultHandler Asynchronous result handler
    */
    public void createXmlItem(XmlItem xmlItem, Handler<AsyncResult<Void>> resultHandler) {
        delegate.createXmlItem(xmlItem, resultHandler);
    }

    /**
    * creates an XmlItem
    * this route creates an XmlItem
    * @param xmlItem XmlItem Body (required)
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void createXmlItem(XmlItem xmlItem, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
        delegate.createXmlItem(xmlItem, authInfo, resultHandler);
    }

    /**
    * creates an XmlItem
    * this route creates an XmlItem
    * @param xmlItem XmlItem Body (required)
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxCreateXmlItem(XmlItem xmlItem) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.createXmlItem(xmlItem, fut)
        ));
    }

    /**
    * creates an XmlItem
    * this route creates an XmlItem
    * @param xmlItem XmlItem Body (required)
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxCreateXmlItem(XmlItem xmlItem, ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.createXmlItem(xmlItem, authInfo, fut)
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
    * @param body Input composite as post body (optional)
    * @param resultHandler Asynchronous result handler
    */
    public void fakeOuterCompositeSerialize(OuterComposite body, Handler<AsyncResult<OuterComposite>> resultHandler) {
        delegate.fakeOuterCompositeSerialize(body, resultHandler);
    }

    /**
    * 
    * Test serialization of object with outer number type
    * @param body Input composite as post body (optional)
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void fakeOuterCompositeSerialize(OuterComposite body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<OuterComposite>> resultHandler) {
        delegate.fakeOuterCompositeSerialize(body, authInfo, resultHandler);
    }

    /**
    * 
    * Test serialization of object with outer number type
    * @param body Input composite as post body (optional)
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<OuterComposite> rxFakeOuterCompositeSerialize(OuterComposite body) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.fakeOuterCompositeSerialize(body, fut)
        ));
    }

    /**
    * 
    * Test serialization of object with outer number type
    * @param body Input composite as post body (optional)
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<OuterComposite> rxFakeOuterCompositeSerialize(OuterComposite body, ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.fakeOuterCompositeSerialize(body, authInfo, fut)
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
    * For this test, the body for this request much reference a schema named &#x60;File&#x60;.
    * @param body  (required)
    * @param resultHandler Asynchronous result handler
    */
    public void testBodyWithFileSchema(FileSchemaTestClass body, Handler<AsyncResult<Void>> resultHandler) {
        delegate.testBodyWithFileSchema(body, resultHandler);
    }

    /**
    * 
    * For this test, the body for this request much reference a schema named &#x60;File&#x60;.
    * @param body  (required)
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void testBodyWithFileSchema(FileSchemaTestClass body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
        delegate.testBodyWithFileSchema(body, authInfo, resultHandler);
    }

    /**
    * 
    * For this test, the body for this request much reference a schema named &#x60;File&#x60;.
    * @param body  (required)
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxTestBodyWithFileSchema(FileSchemaTestClass body) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.testBodyWithFileSchema(body, fut)
        ));
    }

    /**
    * 
    * For this test, the body for this request much reference a schema named &#x60;File&#x60;.
    * @param body  (required)
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxTestBodyWithFileSchema(FileSchemaTestClass body, ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.testBodyWithFileSchema(body, authInfo, fut)
        ));
    }
    /**
    * 
    * 
    * @param query  (required)
    * @param body  (required)
    * @param resultHandler Asynchronous result handler
    */
    public void testBodyWithQueryParams(String query, User body, Handler<AsyncResult<Void>> resultHandler) {
        delegate.testBodyWithQueryParams(query, body, resultHandler);
    }

    /**
    * 
    * 
    * @param query  (required)
    * @param body  (required)
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void testBodyWithQueryParams(String query, User body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
        delegate.testBodyWithQueryParams(query, body, authInfo, resultHandler);
    }

    /**
    * 
    * 
    * @param query  (required)
    * @param body  (required)
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxTestBodyWithQueryParams(String query, User body) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.testBodyWithQueryParams(query, body, fut)
        ));
    }

    /**
    * 
    * 
    * @param query  (required)
    * @param body  (required)
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxTestBodyWithQueryParams(String query, User body, ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.testBodyWithQueryParams(query, body, authInfo, fut)
        ));
    }
    /**
    * To test \&quot;client\&quot; model
    * To test \&quot;client\&quot; model
    * @param body client model (required)
    * @param resultHandler Asynchronous result handler
    */
    public void testClientModel(Client body, Handler<AsyncResult<Client>> resultHandler) {
        delegate.testClientModel(body, resultHandler);
    }

    /**
    * To test \&quot;client\&quot; model
    * To test \&quot;client\&quot; model
    * @param body client model (required)
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void testClientModel(Client body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Client>> resultHandler) {
        delegate.testClientModel(body, authInfo, resultHandler);
    }

    /**
    * To test \&quot;client\&quot; model
    * To test \&quot;client\&quot; model
    * @param body client model (required)
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Client> rxTestClientModel(Client body) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.testClientModel(body, fut)
        ));
    }

    /**
    * To test \&quot;client\&quot; model
    * To test \&quot;client\&quot; model
    * @param body client model (required)
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Client> rxTestClientModel(Client body, ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.testClientModel(body, authInfo, fut)
        ));
    }
    /**
    * Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
    * Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
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
    * Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
    * Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
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
    * Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
    * Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
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
    * Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
    * Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
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
    * @param enumFormStringArray Form parameter enum test (string array) (optional, default to $)
    * @param enumFormString Form parameter enum test (string) (optional, default to -efg)
    * @param resultHandler Asynchronous result handler
    */
    public void testEnumParameters(List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble, List<String> enumFormStringArray, String enumFormString, Handler<AsyncResult<Void>> resultHandler) {
        delegate.testEnumParameters(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumFormStringArray, enumFormString, resultHandler);
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
    * @param enumFormStringArray Form parameter enum test (string array) (optional, default to $)
    * @param enumFormString Form parameter enum test (string) (optional, default to -efg)
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void testEnumParameters(List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble, List<String> enumFormStringArray, String enumFormString, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
        delegate.testEnumParameters(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumFormStringArray, enumFormString, authInfo, resultHandler);
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
    * @param enumFormStringArray Form parameter enum test (string array) (optional, default to $)
    * @param enumFormString Form parameter enum test (string) (optional, default to -efg)
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxTestEnumParameters(List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble, List<String> enumFormStringArray, String enumFormString) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.testEnumParameters(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumFormStringArray, enumFormString, fut)
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
    * @param enumFormStringArray Form parameter enum test (string array) (optional, default to $)
    * @param enumFormString Form parameter enum test (string) (optional, default to -efg)
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxTestEnumParameters(List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble, List<String> enumFormStringArray, String enumFormString, ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.testEnumParameters(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumFormStringArray, enumFormString, authInfo, fut)
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
    * @param param request body (required)
    * @param resultHandler Asynchronous result handler
    */
    public void testInlineAdditionalProperties(Map<String, String> param, Handler<AsyncResult<Void>> resultHandler) {
        delegate.testInlineAdditionalProperties(param, resultHandler);
    }

    /**
    * test inline additionalProperties
    * 
    * @param param request body (required)
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void testInlineAdditionalProperties(Map<String, String> param, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
        delegate.testInlineAdditionalProperties(param, authInfo, resultHandler);
    }

    /**
    * test inline additionalProperties
    * 
    * @param param request body (required)
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxTestInlineAdditionalProperties(Map<String, String> param) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.testInlineAdditionalProperties(param, fut)
        ));
    }

    /**
    * test inline additionalProperties
    * 
    * @param param request body (required)
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxTestInlineAdditionalProperties(Map<String, String> param, ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.testInlineAdditionalProperties(param, authInfo, fut)
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
    * 
    * To test the collection format in query parameters
    * @param pipe  (required)
    * @param ioutil  (required)
    * @param http  (required)
    * @param url  (required)
    * @param context  (required)
    * @param resultHandler Asynchronous result handler
    */
    public void testQueryParameterCollectionFormat(List<String> pipe, List<String> ioutil, List<String> http, List<String> url, List<String> context, Handler<AsyncResult<Void>> resultHandler) {
        delegate.testQueryParameterCollectionFormat(pipe, ioutil, http, url, context, resultHandler);
    }

    /**
    * 
    * To test the collection format in query parameters
    * @param pipe  (required)
    * @param ioutil  (required)
    * @param http  (required)
    * @param url  (required)
    * @param context  (required)
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void testQueryParameterCollectionFormat(List<String> pipe, List<String> ioutil, List<String> http, List<String> url, List<String> context, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
        delegate.testQueryParameterCollectionFormat(pipe, ioutil, http, url, context, authInfo, resultHandler);
    }

    /**
    * 
    * To test the collection format in query parameters
    * @param pipe  (required)
    * @param ioutil  (required)
    * @param http  (required)
    * @param url  (required)
    * @param context  (required)
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxTestQueryParameterCollectionFormat(List<String> pipe, List<String> ioutil, List<String> http, List<String> url, List<String> context) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.testQueryParameterCollectionFormat(pipe, ioutil, http, url, context, fut)
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
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxTestQueryParameterCollectionFormat(List<String> pipe, List<String> ioutil, List<String> http, List<String> url, List<String> context, ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.testQueryParameterCollectionFormat(pipe, ioutil, http, url, context, authInfo, fut)
        ));
    }

    public static FakeApi newInstance(org.openapitools.client.api.FakeApi arg) {
        return arg != null ? new FakeApi(arg) : null;
    }
}
