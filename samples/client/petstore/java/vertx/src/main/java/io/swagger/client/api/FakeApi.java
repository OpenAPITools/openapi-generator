package io.swagger.client.api;

import java.math.BigDecimal;
import io.swagger.client.model.Client;
import org.threeten.bp.LocalDate;
import org.threeten.bp.OffsetDateTime;
import io.swagger.client.model.OuterComposite;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;

import java.util.*;

public interface FakeApi {

    void fakeOuterBooleanSerialize(Boolean body, Handler<AsyncResult<Boolean>> handler);

    void fakeOuterCompositeSerialize(OuterComposite body, Handler<AsyncResult<OuterComposite>> handler);

    void fakeOuterNumberSerialize(BigDecimal body, Handler<AsyncResult<BigDecimal>> handler);

    void fakeOuterStringSerialize(String body, Handler<AsyncResult<String>> handler);

    void testClientModel(Client body, Handler<AsyncResult<Client>> handler);

    void testEndpointParameters(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string, byte[] binary, LocalDate date, OffsetDateTime dateTime, String password, String paramCallback, Handler<AsyncResult<Void>> handler);

    void testEnumParameters(List<String> enumFormStringArray, String enumFormString, List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble, Handler<AsyncResult<Void>> handler);

    void testJsonFormData(String param, String param2, Handler<AsyncResult<Void>> handler);

}
