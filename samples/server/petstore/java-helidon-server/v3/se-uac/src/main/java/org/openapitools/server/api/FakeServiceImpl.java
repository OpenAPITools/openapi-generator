package org.openapitools.server.api;

import java.util.ArrayList;
import java.math.BigDecimal;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import org.openapitools.server.model.ChildWithNullable;
import org.openapitools.server.model.Client;
import io.helidon.common.http.DataChunk;
import org.openapitools.server.model.EnumClass;
import org.openapitools.server.model.FakeBigDecimalMap200Response;
import java.io.File;
import org.openapitools.server.model.FileSchemaTestClass;
import io.helidon.webserver.Handler;
import java.util.HashMap;
import org.openapitools.server.model.HealthCheckResult;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.time.LocalDate;
import java.util.Map;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.time.OffsetDateTime;
import org.openapitools.server.model.OuterComposite;
import org.openapitools.server.model.OuterObjectWithEnumProperty;
import org.openapitools.server.model.Pet;
import io.helidon.media.multipart.ReadableBodyPart;
import org.openapitools.server.model.TestInlineFreeformAdditionalPropertiesRequest;
import java.io.UncheckedIOException;
import org.openapitools.server.model.User;
import io.helidon.webserver.ServerRequest;
import io.helidon.webserver.ServerResponse;

public class FakeServiceImpl extends FakeService {

    private static final int HTTP_CODE_NOT_IMPLEMENTED = 501;

    public void handleFakeBigDecimalMap(ServerRequest request, ServerResponse response) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void handleFakeHealthGet(ServerRequest request, ServerResponse response) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void handleFakeHttpSignatureTest(ServerRequest request, ServerResponse response, Pet pet, String query1, String header1) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void handleFakeOuterBooleanSerialize(ServerRequest request, ServerResponse response, Boolean body) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void handleFakeOuterCompositeSerialize(ServerRequest request, ServerResponse response, OuterComposite outerComposite) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void handleFakeOuterNumberSerialize(ServerRequest request, ServerResponse response, BigDecimal body) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void handleFakeOuterStringSerialize(ServerRequest request, ServerResponse response, String body) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void handleFakePropertyEnumIntegerSerialize(ServerRequest request, ServerResponse response, OuterObjectWithEnumProperty outerObjectWithEnumProperty) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void handleTestAdditionalPropertiesReference(ServerRequest request, ServerResponse response, String requestBody) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void handleTestBodyWithBinary(ServerRequest request, ServerResponse response, InputStream body) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void handleTestBodyWithFileSchema(ServerRequest request, ServerResponse response, FileSchemaTestClass fileSchemaTestClass) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void handleTestBodyWithQueryParams(ServerRequest request, ServerResponse response, String query, User user) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void handleTestClientModel(ServerRequest request, ServerResponse response, Client client) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void handleTestEndpointParameters(ServerRequest request, ServerResponse response, BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string, InputStream binary, LocalDate date, OffsetDateTime dateTime, String password, String paramCallback) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void handleTestEnumParameters(ServerRequest request, ServerResponse response, List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble, List<String> enumQueryModelArray, List<String> enumFormStringArray, String enumFormString) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void handleTestGroupParameters(ServerRequest request, ServerResponse response, Integer requiredStringGroup, Boolean requiredBooleanGroup, Long requiredInt64Group, Integer stringGroup, Boolean booleanGroup, Long int64Group) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void handleTestInlineAdditionalProperties(ServerRequest request, ServerResponse response, String requestBody) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void handleTestInlineFreeformAdditionalProperties(ServerRequest request, ServerResponse response, TestInlineFreeformAdditionalPropertiesRequest testInlineFreeformAdditionalPropertiesRequest) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void handleTestJsonFormData(ServerRequest request, ServerResponse response, String param, String param2) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void handleTestNullable(ServerRequest request, ServerResponse response, ChildWithNullable childWithNullable) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void handleTestQueryParameterCollectionFormat(ServerRequest request, ServerResponse response, List<String> pipe, List<String> ioutil, List<String> http, List<String> url, List<String> context, String allowEmpty, String language) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void handleTestStringMapReference(ServerRequest request, ServerResponse response, String requestBody) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }


    public Void handleError(ServerRequest request, ServerResponse response, Throwable throwable) {
        return response.send(throwable);
    }
}
