package io.swagger.client.api.rxjava;

import java.math.BigDecimal;
import io.swagger.client.model.Client;
import org.threeten.bp.LocalDate;
import org.threeten.bp.OffsetDateTime;
import io.swagger.client.model.OuterComposite;

import java.util.*;

import rx.Single;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;


public class FakeApi {

	private final io.swagger.client.api.FakeApi delegate;

	public FakeApi(io.swagger.client.api.FakeApi delegate) {
	    this.delegate = delegate;
    }

	public io.swagger.client.api.FakeApi getDelegate() {
	    return delegate;
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
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<Boolean> rxFakeOuterBooleanSerialize(Boolean body) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.fakeOuterBooleanSerialize(body, fut);
        }));
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
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<OuterComposite> rxFakeOuterCompositeSerialize(OuterComposite body) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.fakeOuterCompositeSerialize(body, fut);
        }));
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
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<BigDecimal> rxFakeOuterNumberSerialize(BigDecimal body) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.fakeOuterNumberSerialize(body, fut);
        }));
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
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<String> rxFakeOuterStringSerialize(String body) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.fakeOuterStringSerialize(body, fut);
        }));
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
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<Client> rxTestClientModel(Client body) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.testClientModel(body, fut);
        }));
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
    public void testEndpointParameters(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string, byte[] binary, LocalDate date, OffsetDateTime dateTime, String password, String paramCallback, Handler<AsyncResult<Void>> resultHandler) {
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
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<Void> rxTestEndpointParameters(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string, byte[] binary, LocalDate date, OffsetDateTime dateTime, String password, String paramCallback) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.testEndpointParameters(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, paramCallback, fut);
        }));
    }
    /**
     * To test enum parameters
     * To test enum parameters
     * @param enumFormStringArray Form parameter enum test (string array) (optional)
     * @param enumFormString Form parameter enum test (string) (optional, default to -efg)
     * @param enumHeaderStringArray Header parameter enum test (string array) (optional)
     * @param enumHeaderString Header parameter enum test (string) (optional, default to -efg)
     * @param enumQueryStringArray Query parameter enum test (string array) (optional)
     * @param enumQueryString Query parameter enum test (string) (optional, default to -efg)
     * @param enumQueryInteger Query parameter enum test (double) (optional)
     * @param enumQueryDouble Query parameter enum test (double) (optional)
     * @param resultHandler Asynchronous result handler
     */
    public void testEnumParameters(List<String> enumFormStringArray, String enumFormString, List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble, Handler<AsyncResult<Void>> resultHandler) {
        delegate.testEnumParameters(enumFormStringArray, enumFormString, enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, resultHandler);
    }

    /**
     * To test enum parameters
     * To test enum parameters
     * @param enumFormStringArray Form parameter enum test (string array) (optional)
     * @param enumFormString Form parameter enum test (string) (optional, default to -efg)
     * @param enumHeaderStringArray Header parameter enum test (string array) (optional)
     * @param enumHeaderString Header parameter enum test (string) (optional, default to -efg)
     * @param enumQueryStringArray Query parameter enum test (string array) (optional)
     * @param enumQueryString Query parameter enum test (string) (optional, default to -efg)
     * @param enumQueryInteger Query parameter enum test (double) (optional)
     * @param enumQueryDouble Query parameter enum test (double) (optional)
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<Void> rxTestEnumParameters(List<String> enumFormStringArray, String enumFormString, List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.testEnumParameters(enumFormStringArray, enumFormString, enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, fut);
        }));
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
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<Void> rxTestJsonFormData(String param, String param2) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.testJsonFormData(param, param2, fut);
        }));
    }

    public static FakeApi newInstance(io.swagger.client.api.FakeApi arg) {
        return arg != null ? new FakeApi(arg) : null;
    }
}
