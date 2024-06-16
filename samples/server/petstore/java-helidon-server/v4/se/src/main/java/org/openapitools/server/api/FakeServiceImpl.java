package org.openapitools.server.api;

import java.math.BigDecimal;
import org.openapitools.server.model.ChildWithNullable;
import org.openapitools.server.model.Client;
import org.openapitools.server.model.EnumClass;
import org.openapitools.server.model.FakeBigDecimalMap200Response;
import java.io.File;
import org.openapitools.server.model.FileSchemaTestClass;
import io.helidon.common.GenericType;
import io.helidon.http.HeaderNames;
import io.helidon.http.Headers;
import org.openapitools.server.model.HealthCheckResult;
import java.util.List;
import java.time.LocalDate;
import java.util.Map;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.time.OffsetDateTime;
import java.util.Optional;
import org.openapitools.server.model.OuterComposite;
import org.openapitools.server.model.OuterObjectWithEnumProperty;
import io.helidon.common.parameters.Parameters;
import org.openapitools.server.model.Pet;
import org.openapitools.server.model.TestInlineFreeformAdditionalPropertiesRequest;
import org.openapitools.server.model.User;
import io.helidon.common.mapper.Value;
import java.util.logging.Logger;

import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;
import org.openapitools.server.model.GenericTypes;

public class FakeServiceImpl implements FakeService {

    private static final int HTTP_CODE_NOT_IMPLEMENTED = 501;
    private static final Logger LOGGER = Logger.getLogger(FakeService.class.getName());
    private static final ObjectMapper MAPPER = JsonProvider.objectMapper();


    public void fakeBigDecimalMap(ServerRequest request, ServerResponse response) {

        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void fakeHealthGet(ServerRequest request, ServerResponse response) {

        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void fakeHttpSignatureTest(ServerRequest request, ServerResponse response) {
        Pet pet = request.content().as(Pet.class);

        Optional<String> query1 = request.query()
                .first("query_1");

        Optional<String> header1 = request.headers()
                .first(HeaderNames.create("header_1"));


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void fakeOuterBooleanSerialize(ServerRequest request, ServerResponse response) {
        Optional<Boolean> body = Optional.ofNullable(request.content().hasEntity()
                ? request.content().as(Boolean.class)
                : null);


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void fakeOuterCompositeSerialize(ServerRequest request, ServerResponse response) {
        Optional<OuterComposite> outerComposite = Optional.ofNullable(request.content().hasEntity()
                ? request.content().as(OuterComposite.class)
                : null);


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void fakeOuterNumberSerialize(ServerRequest request, ServerResponse response) {
        Optional<BigDecimal> body = Optional.ofNullable(request.content().hasEntity()
                ? request.content().as(BigDecimal.class)
                : null);


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void fakeOuterStringSerialize(ServerRequest request, ServerResponse response) {
        Optional<String> body = Optional.ofNullable(request.content().hasEntity()
                ? request.content().as(String.class)
                : null);


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void fakePropertyEnumIntegerSerialize(ServerRequest request, ServerResponse response) {
        OuterObjectWithEnumProperty outerObjectWithEnumProperty = request.content().as(OuterObjectWithEnumProperty.class);


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testAdditionalPropertiesReference(ServerRequest request, ServerResponse response) {
        Map<String, Object> requestBody = request.content().as(GenericTypes.TYPE__Map_Object);


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testBodyWithBinary(ServerRequest request, ServerResponse response) {
        File body = request.content().as(File.class);


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testBodyWithFileSchema(ServerRequest request, ServerResponse response) {
        FileSchemaTestClass fileSchemaTestClass = request.content().as(FileSchemaTestClass.class);


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testBodyWithQueryParams(ServerRequest request, ServerResponse response) {
        String query = ValidatorUtils.nonEmpty(request.query()
                .first("query"));

        User user = request.content().as(User.class);


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testClientModel(ServerRequest request, ServerResponse response) {
        Client client = request.content().as(Client.class);


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testEndpointParameters(ServerRequest request, ServerResponse response) {
        Parameters formParams = request.content().as(Parameters.class);
        Optional<Integer> integer = formParams.first("integer")
                .map(Integer::valueOf)
                .asOptional();
        Optional<Integer> int32 = formParams.first("int32")
                .map(Integer::valueOf)
                .asOptional();
        Optional<Long> int64 = formParams.first("int64")
                .map(Long::valueOf)
                .asOptional();
        BigDecimal number = ValidatorUtils.nonEmpty(formParams.first("number")
                .map(BigDecimal::new)
                .asOptional());
        Optional<Float> _float = formParams.first("float")
                .map(Float::valueOf)
                .asOptional();
        Double _double = ValidatorUtils.nonEmpty(formParams.first("double")
                .map(Double::valueOf)
                .asOptional());
        Optional<String> string = formParams.first("string")
                .asOptional();
        String patternWithoutDelimiter = ValidatorUtils.nonEmpty(formParams.first("pattern_without_delimiter")
                .asOptional());
        byte[] _byte = ValidatorUtils.nonEmpty(formParams.first("byte")
                .map(byte[]::valueOf)
                .asOptional());
        Optional<File> binary = formParams.first("binary")
                .asOptional();
        Optional<LocalDate> date = formParams.first("date")
                .map(LocalDate::parse)
                .asOptional();
        Optional<OffsetDateTime> dateTime = formParams.first("dateTime")
                .map(OffsetDateTime::parse)
                .asOptional();
        Optional<String> password = formParams.first("password")
                .asOptional();
        Optional<String> paramCallback = formParams.first("callback")
                .asOptional();

        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testEnumParameters(ServerRequest request, ServerResponse response) {
        Parameters formParams = request.content().as(Parameters.class);
        List<String> enumHeaderStringArray = request.headers()
                .values(HeaderNames.create("enum_header_string_array"))
                .stream()
                .toList();

        Optional<String> enumHeaderString = request.headers()
                .first(HeaderNames.create("enum_header_string"));

        List<String> enumQueryStringArray = request.query()
                .all("enum_query_string_array")
                .stream()
                .toList();

        Optional<String> enumQueryString = request.query()
                .first("enum_query_string");

        Optional<Integer> enumQueryInteger = request.query()
                .first("enum_query_integer")
                .map(Integer::valueOf);

        Optional<Double> enumQueryDouble = request.query()
                .first("enum_query_double")
                .map(Double::valueOf);

        List<EnumClass> enumQueryModelArray = request.query()
                .all("enum_query_model_array")
                .stream()
                .toList();

        List<String> enumFormStringArray = formParams                .all("enum_form_string_array")
                .stream()
                .toList();
        Optional<String> enumFormString = formParams.first("enum_form_string")
                .asOptional();

        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testGroupParameters(ServerRequest request, ServerResponse response) {
        Integer requiredStringGroup = ValidatorUtils.nonEmpty(request.query()
                .first("required_string_group")
                .map(Integer::valueOf));

        Boolean requiredBooleanGroup = ValidatorUtils.nonEmpty(request.headers()
                .first(HeaderNames.create("required_boolean_group"))
                .map(Boolean::valueOf));

        Long requiredInt64Group = ValidatorUtils.nonEmpty(request.query()
                .first("required_int64_group")
                .map(Long::valueOf));

        Optional<Integer> stringGroup = request.query()
                .first("string_group")
                .map(Integer::valueOf);

        Optional<Boolean> booleanGroup = request.headers()
                .first(HeaderNames.create("boolean_group"))
                .map(Boolean::valueOf);

        Optional<Long> int64Group = request.query()
                .first("int64_group")
                .map(Long::valueOf);


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testInlineAdditionalProperties(ServerRequest request, ServerResponse response) {
        Map<String, String> requestBody = request.content().as(GenericTypes.TYPE__Map_String);


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testInlineFreeformAdditionalProperties(ServerRequest request, ServerResponse response) {
        TestInlineFreeformAdditionalPropertiesRequest testInlineFreeformAdditionalPropertiesRequest = request.content().as(TestInlineFreeformAdditionalPropertiesRequest.class);


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testJsonFormData(ServerRequest request, ServerResponse response) {
        Parameters formParams = request.content().as(Parameters.class);
        String param = ValidatorUtils.nonEmpty(formParams.first("param")
                .asOptional());
        String param2 = ValidatorUtils.nonEmpty(formParams.first("param2")
                .asOptional());

        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testNullable(ServerRequest request, ServerResponse response) {
        ChildWithNullable childWithNullable = request.content().as(ChildWithNullable.class);


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testQueryParameterCollectionFormat(ServerRequest request, ServerResponse response) {
        List<String> pipe = ValidatorUtils.nonEmpty(request.query()
                .all("pipe")
                .stream()
                .toList());

        List<String> ioutil = ValidatorUtils.nonEmpty(request.query()
                .all("ioutil")
                .stream()
                .toList());

        List<String> http = ValidatorUtils.nonEmpty(request.query()
                .all("http")
                .stream()
                .toList());

        List<String> url = ValidatorUtils.nonEmpty(request.query()
                .all("url")
                .stream()
                .toList());

        List<String> context = ValidatorUtils.nonEmpty(request.query()
                .all("context")
                .stream()
                .toList());

        String allowEmpty = ValidatorUtils.nonEmpty(request.query()
                .first("allowEmpty"));

        Map<String, String> language = request.query()
                .all("language")
                .stream()
                .toMap();


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testStringMapReference(ServerRequest request, ServerResponse response) {
        Map<String, String> requestBody = request.content().as(GenericTypes.TYPE__Map_String);


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    @Override
    public void afterStop() {
        System.out.println("Service FakeService is down. Goodbye!");
    }
}
