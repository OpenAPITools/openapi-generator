package org.openapitools.server.api;

import java.math.BigDecimal;
import org.openapitools.server.model.ChildWithNullable;
import org.openapitools.server.model.Client;
import java.util.stream.Collectors;
import org.openapitools.server.model.EnumClass;
import org.openapitools.server.model.FakeBigDecimalMap200Response;
import java.io.File;
import org.openapitools.server.model.FileSchemaTestClass;
import java.nio.file.Files;
import org.openapitools.server.model.GenericTypes;
import io.helidon.http.HeaderNames;
import io.helidon.http.Headers;
import org.openapitools.server.model.HealthCheckResult;
import java.util.HexFormat;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.time.LocalDate;
import java.util.Map;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.Objects;
import java.time.OffsetDateTime;
import java.util.Optional;
import org.openapitools.server.model.OuterComposite;
import org.openapitools.server.model.OuterObjectWithEnumProperty;
import io.helidon.common.parameters.Parameters;
import java.nio.file.Path;
import org.openapitools.server.model.Pet;
import io.helidon.http.media.multipart.ReadablePart;
import io.helidon.http.Status;
import org.openapitools.server.model.TestInlineFreeformAdditionalPropertiesRequest;
import java.io.UncheckedIOException;
import org.openapitools.server.model.User;
import io.helidon.common.mapper.Value;
import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;

public class FakeServiceImpl implements FakeService {
    private static final ObjectMapper MAPPER = JsonProvider.objectMapper();


    @Override
    public void fakeBigDecimalMap(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    public void fakeHealthGet(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    public void fakeHttpSignatureTest(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    public void fakeOuterBooleanSerialize(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    public void fakeOuterCompositeSerialize(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    public void fakeOuterNumberSerialize(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    public void fakeOuterStringSerialize(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    public void fakePropertyEnumIntegerSerialize(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    public void testAdditionalPropertiesReference(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    public void testBodyWithBinary(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    public void testBodyWithFileSchema(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    public void testBodyWithQueryParams(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    public void testClientModel(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    public void testEndpointParameters(ServerRequest request, ServerResponse response) {
        Parameters formParams = request.content().as(Parameters.class);
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    public void testEnumParameters(ServerRequest request, ServerResponse response) {
        Parameters formParams = request.content().as(Parameters.class);
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    public void testGroupParameters(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    public void testInlineAdditionalProperties(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    public void testInlineFreeformAdditionalProperties(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    public void testJsonFormData(ServerRequest request, ServerResponse response) {
        Parameters formParams = request.content().as(Parameters.class);
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    public void testNullable(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    public void testQueryParameterCollectionFormat(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    public void testStringMapReference(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }


    @Override
    public void afterStop() {
        System.out.println("Service FakeService is down. Goodbye!");
    }

}
