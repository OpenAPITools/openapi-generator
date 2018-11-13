package org.openapitools.client.api;

import io.vertx.core.file.AsyncFile;
import java.math.BigDecimal;
import org.openapitools.client.model.Client;
import org.openapitools.client.model.FileSchemaTestClass;
import org.threeten.bp.LocalDate;
import org.threeten.bp.OffsetDateTime;
import org.openapitools.client.model.OuterComposite;
import org.openapitools.client.model.User;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;

import java.util.*;

public interface FakeApi {

    void fakeOuterBooleanSerialize(Boolean body, Handler<AsyncResult<Boolean>> handler);

    void fakeOuterCompositeSerialize(OuterComposite outerComposite, Handler<AsyncResult<OuterComposite>> handler);

    void fakeOuterNumberSerialize(BigDecimal body, Handler<AsyncResult<BigDecimal>> handler);

    void fakeOuterStringSerialize(String body, Handler<AsyncResult<String>> handler);

    void testBodyWithFileSchema(FileSchemaTestClass fileSchemaTestClass, Handler<AsyncResult<Void>> handler);

    void testBodyWithQueryParams(String query, User user, Handler<AsyncResult<Void>> handler);

    void testClientModel(Client client, Handler<AsyncResult<Client>> handler);

    void testEndpointParameters(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string, AsyncFile binary, LocalDate date, OffsetDateTime dateTime, String password, String paramCallback, Handler<AsyncResult<Void>> handler);

    void testEnumParameters(List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble, List<String> enumFormStringArray, String enumFormString, Handler<AsyncResult<Void>> handler);

    void testGroupParameters(Integer requiredStringGroup, Boolean requiredBooleanGroup, Long requiredInt64Group, Integer stringGroup, Boolean booleanGroup, Long int64Group, Handler<AsyncResult<Void>> handler);

    void testInlineAdditionalProperties(Map<String, String> requestBody, Handler<AsyncResult<Void>> handler);

    void testJsonFormData(String param, String param2, Handler<AsyncResult<Void>> handler);

}
