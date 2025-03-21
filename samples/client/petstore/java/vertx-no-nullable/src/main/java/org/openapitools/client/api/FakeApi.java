package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import io.vertx.core.file.AsyncFile;
import java.math.BigDecimal;
import org.openapitools.client.model.Client;
import org.openapitools.client.model.FileSchemaTestClass;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import org.openapitools.client.model.OuterComposite;
import org.openapitools.client.model.User;
import org.openapitools.client.model.XmlItem;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;

import java.util.*;

public interface FakeApi {

    void createXmlItem(@javax.annotation.Nonnull XmlItem xmlItem, Handler<AsyncResult<Void>> handler);

    void createXmlItem(@javax.annotation.Nonnull XmlItem xmlItem, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void fakeOuterBooleanSerialize(@javax.annotation.Nullable Boolean body, Handler<AsyncResult<Boolean>> handler);

    void fakeOuterBooleanSerialize(@javax.annotation.Nullable Boolean body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Boolean>> handler);

    void fakeOuterCompositeSerialize(@javax.annotation.Nullable OuterComposite body, Handler<AsyncResult<OuterComposite>> handler);

    void fakeOuterCompositeSerialize(@javax.annotation.Nullable OuterComposite body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<OuterComposite>> handler);

    void fakeOuterNumberSerialize(@javax.annotation.Nullable BigDecimal body, Handler<AsyncResult<BigDecimal>> handler);

    void fakeOuterNumberSerialize(@javax.annotation.Nullable BigDecimal body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<BigDecimal>> handler);

    void fakeOuterStringSerialize(@javax.annotation.Nullable String body, Handler<AsyncResult<String>> handler);

    void fakeOuterStringSerialize(@javax.annotation.Nullable String body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<String>> handler);

    void testBodyWithFileSchema(@javax.annotation.Nonnull FileSchemaTestClass body, Handler<AsyncResult<Void>> handler);

    void testBodyWithFileSchema(@javax.annotation.Nonnull FileSchemaTestClass body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void testBodyWithQueryParams(@javax.annotation.Nonnull String query, @javax.annotation.Nonnull User body, Handler<AsyncResult<Void>> handler);

    void testBodyWithQueryParams(@javax.annotation.Nonnull String query, @javax.annotation.Nonnull User body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void testClientModel(@javax.annotation.Nonnull Client body, Handler<AsyncResult<Client>> handler);

    void testClientModel(@javax.annotation.Nonnull Client body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Client>> handler);

    void testEndpointParameters(@javax.annotation.Nonnull BigDecimal number, @javax.annotation.Nonnull Double _double, @javax.annotation.Nonnull String patternWithoutDelimiter, @javax.annotation.Nonnull byte[] _byte, @javax.annotation.Nullable Integer integer, @javax.annotation.Nullable Integer int32, @javax.annotation.Nullable Long int64, @javax.annotation.Nullable Float _float, @javax.annotation.Nullable String string, @javax.annotation.Nullable AsyncFile binary, @javax.annotation.Nullable LocalDate date, @javax.annotation.Nullable OffsetDateTime dateTime, @javax.annotation.Nullable String password, @javax.annotation.Nullable String paramCallback, Handler<AsyncResult<Void>> handler);

    void testEndpointParameters(@javax.annotation.Nonnull BigDecimal number, @javax.annotation.Nonnull Double _double, @javax.annotation.Nonnull String patternWithoutDelimiter, @javax.annotation.Nonnull byte[] _byte, @javax.annotation.Nullable Integer integer, @javax.annotation.Nullable Integer int32, @javax.annotation.Nullable Long int64, @javax.annotation.Nullable Float _float, @javax.annotation.Nullable String string, @javax.annotation.Nullable AsyncFile binary, @javax.annotation.Nullable LocalDate date, @javax.annotation.Nullable OffsetDateTime dateTime, @javax.annotation.Nullable String password, @javax.annotation.Nullable String paramCallback, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void testEnumParameters(@javax.annotation.Nullable List<String> enumHeaderStringArray, @javax.annotation.Nullable String enumHeaderString, @javax.annotation.Nullable List<String> enumQueryStringArray, @javax.annotation.Nullable String enumQueryString, @javax.annotation.Nullable Integer enumQueryInteger, @javax.annotation.Nullable Double enumQueryDouble, @javax.annotation.Nullable List<String> enumFormStringArray, @javax.annotation.Nullable String enumFormString, Handler<AsyncResult<Void>> handler);

    void testEnumParameters(@javax.annotation.Nullable List<String> enumHeaderStringArray, @javax.annotation.Nullable String enumHeaderString, @javax.annotation.Nullable List<String> enumQueryStringArray, @javax.annotation.Nullable String enumQueryString, @javax.annotation.Nullable Integer enumQueryInteger, @javax.annotation.Nullable Double enumQueryDouble, @javax.annotation.Nullable List<String> enumFormStringArray, @javax.annotation.Nullable String enumFormString, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void testGroupParameters(@javax.annotation.Nonnull Integer requiredStringGroup, @javax.annotation.Nonnull Boolean requiredBooleanGroup, @javax.annotation.Nonnull Long requiredInt64Group, @javax.annotation.Nullable Integer stringGroup, @javax.annotation.Nullable Boolean booleanGroup, @javax.annotation.Nullable Long int64Group, Handler<AsyncResult<Void>> handler);

    void testGroupParameters(@javax.annotation.Nonnull Integer requiredStringGroup, @javax.annotation.Nonnull Boolean requiredBooleanGroup, @javax.annotation.Nonnull Long requiredInt64Group, @javax.annotation.Nullable Integer stringGroup, @javax.annotation.Nullable Boolean booleanGroup, @javax.annotation.Nullable Long int64Group, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void testInlineAdditionalProperties(@javax.annotation.Nonnull Map<String, String> param, Handler<AsyncResult<Void>> handler);

    void testInlineAdditionalProperties(@javax.annotation.Nonnull Map<String, String> param, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void testJsonFormData(@javax.annotation.Nonnull String param, @javax.annotation.Nonnull String param2, Handler<AsyncResult<Void>> handler);

    void testJsonFormData(@javax.annotation.Nonnull String param, @javax.annotation.Nonnull String param2, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void testQueryParameterCollectionFormat(@javax.annotation.Nonnull List<String> pipe, @javax.annotation.Nonnull List<String> ioutil, @javax.annotation.Nonnull List<String> http, @javax.annotation.Nonnull List<String> url, @javax.annotation.Nonnull List<String> context, Handler<AsyncResult<Void>> handler);

    void testQueryParameterCollectionFormat(@javax.annotation.Nonnull List<String> pipe, @javax.annotation.Nonnull List<String> ioutil, @javax.annotation.Nonnull List<String> http, @javax.annotation.Nonnull List<String> url, @javax.annotation.Nonnull List<String> context, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

}
