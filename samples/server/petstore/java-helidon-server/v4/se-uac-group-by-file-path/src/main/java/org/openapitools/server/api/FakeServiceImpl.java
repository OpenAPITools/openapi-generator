package org.openapitools.server.api;

import java.util.ArrayList;
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
import java.util.HashMap;
import io.helidon.http.HeaderNames;
import io.helidon.http.Headers;
import org.openapitools.server.model.HealthCheckResult;
import java.util.HexFormat;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.time.LocalDate;
import java.util.Map;
import org.openapitools.server.model.ModelApiResponse;
import io.helidon.http.media.multipart.MultiPart;
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

public class FakeServiceImpl extends FakeService {

    @Override
    protected void handleFakeBigDecimalMap(ServerRequest request, ServerResponse response) {

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    protected void handleFakeHealthGet(ServerRequest request, ServerResponse response) {

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    protected void handleFakeHttpSignatureTest(ServerRequest request, ServerResponse response, 
                Pet pet, 
                Optional<String> query1, 
                Optional<String> header1) {

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    protected void handleFakeOuterBooleanSerialize(ServerRequest request, ServerResponse response, 
                Optional<Boolean> body) {

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    protected void handleFakeOuterCompositeSerialize(ServerRequest request, ServerResponse response, 
                Optional<OuterComposite> outerComposite) {

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    protected void handleFakeOuterNumberSerialize(ServerRequest request, ServerResponse response, 
                Optional<BigDecimal> body) {

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    protected void handleFakeOuterStringSerialize(ServerRequest request, ServerResponse response, 
                Optional<String> body) {

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    protected void handleFakePropertyEnumIntegerSerialize(ServerRequest request, ServerResponse response, 
                OuterObjectWithEnumProperty outerObjectWithEnumProperty) {

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    protected void handleTestAdditionalPropertiesReference(ServerRequest request, ServerResponse response, 
                Map<String, Object> requestBody) {

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    protected void handleTestBodyWithBinary(ServerRequest request, ServerResponse response, 
                InputStream body) {

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    protected void handleTestBodyWithFileSchema(ServerRequest request, ServerResponse response, 
                FileSchemaTestClass fileSchemaTestClass) {

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    protected void handleTestBodyWithQueryParams(ServerRequest request, ServerResponse response, 
                String query, 
                User user) {

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    protected void handleTestClientModel(ServerRequest request, ServerResponse response, 
                Client client) {

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    protected void handleTestEndpointParameters(ServerRequest request, ServerResponse response, 
                BigDecimal number, 
                Double _double, 
                String patternWithoutDelimiter, 
                byte[] _byte, 
                Optional<Integer> integer, 
                Optional<Integer> int32, 
                Optional<Long> int64, 
                Optional<Float> _float, 
                Optional<String> string, 
                Optional<byte[]> binary, 
                Optional<LocalDate> date, 
                Optional<OffsetDateTime> dateTime, 
                Optional<String> password, 
                Optional<String> paramCallback) {

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    protected void handleTestEnumParameters(ServerRequest request, ServerResponse response, 
                List<String> enumHeaderStringArray, 
                Optional<String> enumHeaderString, 
                List<String> enumQueryStringArray, 
                Optional<String> enumQueryString, 
                Optional<Integer> enumQueryInteger, 
                Optional<Double> enumQueryDouble, 
                List<EnumClass> enumQueryModelArray, 
                List<String> enumFormStringArray, 
                Optional<String> enumFormString) {

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    protected void handleTestGroupParameters(ServerRequest request, ServerResponse response, 
                Integer requiredStringGroup, 
                Boolean requiredBooleanGroup, 
                Long requiredInt64Group, 
                Optional<Integer> stringGroup, 
                Optional<Boolean> booleanGroup, 
                Optional<Long> int64Group) {

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    protected void handleTestInlineAdditionalProperties(ServerRequest request, ServerResponse response, 
                Map<String, String> requestBody) {

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    protected void handleTestInlineFreeformAdditionalProperties(ServerRequest request, ServerResponse response, 
                TestInlineFreeformAdditionalPropertiesRequest testInlineFreeformAdditionalPropertiesRequest) {

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    protected void handleTestJsonFormData(ServerRequest request, ServerResponse response, 
                String param, 
                String param2) {

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    protected void handleTestNullable(ServerRequest request, ServerResponse response, 
                ChildWithNullable childWithNullable) {

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    protected void handleTestQueryParameterCollectionFormat(ServerRequest request, ServerResponse response, 
                List<String> pipe, 
                List<String> ioutil, 
                List<String> http, 
                List<String> url, 
                List<String> context, 
                String allowEmpty, 
                Map<String, String> language) {

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    protected void handleTestStringMapReference(ServerRequest request, ServerResponse response, 
                Map<String, String> requestBody) {

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    protected void handleUploadFileWithRequiredFile(ServerRequest request, ServerResponse response, 
                Long petId, 
                ReadablePart requiredFile, 
                Optional<ReadablePart> additionalMetadata) {

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

}
