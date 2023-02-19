package org.openapitools.server.api;

import java.math.BigDecimal;
import org.openapitools.server.model.Client;
import org.openapitools.server.model.EnumClass;
import java.io.File;
import org.openapitools.server.model.FileSchemaTestClass;
import io.helidon.webserver.Handler;
import org.openapitools.server.model.HealthCheckResult;
import java.time.LocalDate;
import java.util.Map;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.time.OffsetDateTime;
import org.openapitools.server.model.OuterComposite;
import org.openapitools.server.model.OuterObjectWithEnumProperty;
import org.openapitools.server.model.Pet;
import org.openapitools.server.model.User;
import java.util.logging.Logger;

import io.helidon.webserver.ServerRequest;
import io.helidon.webserver.ServerResponse;

public class FakeServiceImpl implements FakeService {

    private static final int HTTP_CODE_NOT_IMPLEMENTED = 501;
    private static final Logger LOGGER = Logger.getLogger(FakeService.class.getName());
    private static final ObjectMapper MAPPER = JsonProvider.objectMapper();


    public void fakeHealthGet(ServerRequest request, ServerResponse response) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void fakeHttpSignatureTest(ServerRequest request, ServerResponse response, Pet pet) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void fakeOuterBooleanSerialize(ServerRequest request, ServerResponse response) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void fakeOuterCompositeSerialize(ServerRequest request, ServerResponse response, OuterComposite outerComposite) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void fakeOuterNumberSerialize(ServerRequest request, ServerResponse response) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void fakeOuterStringSerialize(ServerRequest request, ServerResponse response) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void fakePropertyEnumIntegerSerialize(ServerRequest request, ServerResponse response, OuterObjectWithEnumProperty outerObjectWithEnumProperty) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testBodyWithBinary(ServerRequest request, ServerResponse response) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testBodyWithFileSchema(ServerRequest request, ServerResponse response, FileSchemaTestClass fileSchemaTestClass) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testBodyWithQueryParams(ServerRequest request, ServerResponse response, User user) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testClientModel(ServerRequest request, ServerResponse response, Client client) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testEndpointParameters(ServerRequest request, ServerResponse response) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testEnumParameters(ServerRequest request, ServerResponse response) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testGroupParameters(ServerRequest request, ServerResponse response) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testInlineAdditionalProperties(ServerRequest request, ServerResponse response) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testJsonFormData(ServerRequest request, ServerResponse response) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testQueryParameterCollectionFormat(ServerRequest request, ServerResponse response) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

}
