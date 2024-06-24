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
import 
import io.helidon.common.Errors;
import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;
import jakarta.validation.ValidationException;
import org.openapitools.server.model.GenericTypes;

public class FakeServiceImpl implements FakeService {

    private static final int HTTP_CODE_NOT_IMPLEMENTED = 501;
    private static final System.Logger LOGGER = System.getLogger(FakeService.class.getName());
    private static final ObjectMapper MAPPER = JsonProvider.objectMapper();


    public void fakeBigDecimalMap(ServerRequest request, ServerResponse response) {
        Errors.Collector errorsCollector = Errors.collector();
        ValidatorUtils.Validator validator = ValidatorUtils.validator();


        Errors errors = errorsCollector.collect();
        errors.log(LOGGER);
        if (errors.hasFatal()) {
            throw new ValidationException("Validation errors: " + errors);
        }
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void fakeHealthGet(ServerRequest request, ServerResponse response) {
        Errors.Collector errorsCollector = Errors.collector();
        ValidatorUtils.Validator validator = ValidatorUtils.validator();


        Errors errors = errorsCollector.collect();
        errors.log(LOGGER);
        if (errors.hasFatal()) {
            throw new ValidationException("Validation errors: " + errors);
        }
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void fakeHttpSignatureTest(ServerRequest request, ServerResponse response) {
        Errors.Collector errorsCollector = Errors.collector();
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        Pet pet = request.content().as(Pet.class);

        Optional<String> query1 = request.query()
                .first("query_1")
                .asOptional()
                .or(Optional::empty);

        Optional<String> header1 = request.headers()
                .first(HeaderNames.create("header_1"))
                .or(Optional::empty);


        Errors errors = errorsCollector.collect();
        errors.log(LOGGER);
        if (errors.hasFatal()) {
            throw new ValidationException("Validation errors: " + errors);
        }
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void fakeOuterBooleanSerialize(ServerRequest request, ServerResponse response) {
        Errors.Collector errorsCollector = Errors.collector();
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        Optional<Boolean> body = Optional.ofNullable(request.content().hasEntity()
                ? request.content().as(Boolean.class)
                : null);


        Errors errors = errorsCollector.collect();
        errors.log(LOGGER);
        if (errors.hasFatal()) {
            throw new ValidationException("Validation errors: " + errors);
        }
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void fakeOuterCompositeSerialize(ServerRequest request, ServerResponse response) {
        Errors.Collector errorsCollector = Errors.collector();
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        Optional<OuterComposite> outerComposite = Optional.ofNullable(request.content().hasEntity()
                ? request.content().as(OuterComposite.class)
                : null);


        Errors errors = errorsCollector.collect();
        errors.log(LOGGER);
        if (errors.hasFatal()) {
            throw new ValidationException("Validation errors: " + errors);
        }
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void fakeOuterNumberSerialize(ServerRequest request, ServerResponse response) {
        Errors.Collector errorsCollector = Errors.collector();
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        Optional<BigDecimal> body = Optional.ofNullable(request.content().hasEntity()
                ? request.content().as(BigDecimal.class)
                : null);


        Errors errors = errorsCollector.collect();
        errors.log(LOGGER);
        if (errors.hasFatal()) {
            throw new ValidationException("Validation errors: " + errors);
        }
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void fakeOuterStringSerialize(ServerRequest request, ServerResponse response) {
        Errors.Collector errorsCollector = Errors.collector();
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        Optional<String> body = Optional.ofNullable(request.content().hasEntity()
                ? request.content().as(String.class)
                : null);


        Errors errors = errorsCollector.collect();
        errors.log(LOGGER);
        if (errors.hasFatal()) {
            throw new ValidationException("Validation errors: " + errors);
        }
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void fakePropertyEnumIntegerSerialize(ServerRequest request, ServerResponse response) {
        Errors.Collector errorsCollector = Errors.collector();
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        OuterObjectWithEnumProperty outerObjectWithEnumProperty = request.content().as(OuterObjectWithEnumProperty.class);


        Errors errors = errorsCollector.collect();
        errors.log(LOGGER);
        if (errors.hasFatal()) {
            throw new ValidationException("Validation errors: " + errors);
        }
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testAdditionalPropertiesReference(ServerRequest request, ServerResponse response) {
        Errors.Collector errorsCollector = Errors.collector();
        ValidatorUtils.Validator validator = ValidatorUtils.validator();


        // TODO - user code needed to handle Map for parameter requestBody.
        Errors errors = errorsCollector.collect();
        errors.log(LOGGER);
        if (errors.hasFatal()) {
            throw new ValidationException("Validation errors: " + errors);
        }
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testBodyWithBinary(ServerRequest request, ServerResponse response) {
        Errors.Collector errorsCollector = Errors.collector();
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        File body = request.content().as(File.class);


        Errors errors = errorsCollector.collect();
        errors.log(LOGGER);
        if (errors.hasFatal()) {
            throw new ValidationException("Validation errors: " + errors);
        }
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testBodyWithFileSchema(ServerRequest request, ServerResponse response) {
        Errors.Collector errorsCollector = Errors.collector();
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        FileSchemaTestClass fileSchemaTestClass = request.content().as(FileSchemaTestClass.class);


        Errors errors = errorsCollector.collect();
        errors.log(LOGGER);
        if (errors.hasFatal()) {
            throw new ValidationException("Validation errors: " + errors);
        }
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testBodyWithQueryParams(ServerRequest request, ServerResponse response) {
        Errors.Collector errorsCollector = Errors.collector();
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        String query = request.query()
                .first("query")
                .asOptional()
                .map(v -> validator.require("query", v))
                .orElse(null);

        User user = request.content().as(User.class);


        Errors errors = errorsCollector.collect();
        errors.log(LOGGER);
        if (errors.hasFatal()) {
            throw new ValidationException("Validation errors: " + errors);
        }
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testClientModel(ServerRequest request, ServerResponse response) {
        Errors.Collector errorsCollector = Errors.collector();
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        Client client = request.content().as(Client.class);


        Errors errors = errorsCollector.collect();
        errors.log(LOGGER);
        if (errors.hasFatal()) {
            throw new ValidationException("Validation errors: " + errors);
        }
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testEndpointParameters(ServerRequest request, ServerResponse response) {
        Parameters formParams = request.content().as(Parameters.class);

        Errors.Collector errorsCollector = Errors.collector();
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        BigDecimal number = formParams
                .first("number")
                .map(v -> validator.require("number", v))
                .map(BigDecimal::new)
                .orElse(null);

        Double _double = formParams
                .first("double")
                .map(v -> validator.require("double", v))
                .map(Double::valueOf)
                .orElse(null);

        String patternWithoutDelimiter = formParams
                .first("pattern_without_delimiter")
                .asOptional()
                .map(v -> validator.require("pattern_without_delimiter", v))
                .orElse(null);

        byte[] _byte = formParams
                .first("byte")
                .map(v -> validator.require("byte", v))
                .map(byte[]::valueOf)
                .orElse(null);

        Optional<Integer> integer = formParams
                .first("integer")
                .map(Integer::valueOf)
                .or(Optional::empty);

        Optional<Integer> int32 = formParams
                .first("int32")
                .map(Integer::valueOf)
                .or(Optional::empty);

        Optional<Long> int64 = formParams
                .first("int64")
                .map(Long::valueOf)
                .or(Optional::empty);

        Optional<Float> _float = formParams
                .first("float")
                .map(Float::valueOf)
                .or(Optional::empty);

        Optional<String> string = formParams
                .first("string")
                .asOptional()
                .or(Optional::empty);

        Optional<File> binary = formParams
                .first("binary")
                .or(Optional::empty);

        Optional<LocalDate> date = formParams
                .first("date")
                .map(LocalDate::parse)
                .or(Optional::empty);

        Optional<OffsetDateTime> dateTime = formParams
                .first("dateTime")
                .map(OffsetDateTime::parse)
                .or(Optional::empty);

        Optional<String> password = formParams
                .first("password")
                .asOptional()
                .or(Optional::empty);

        Optional<String> paramCallback = formParams
                .first("callback")
                .asOptional()
                .or(Optional::empty);


        Errors errors = errorsCollector.collect();
        errors.log(LOGGER);
        if (errors.hasFatal()) {
            throw new ValidationException("Validation errors: " + errors);
        }
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testEnumParameters(ServerRequest request, ServerResponse response) {
        Parameters formParams = request.content().as(Parameters.class);

        Errors.Collector errorsCollector = Errors.collector();
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        List<String> enumHeaderStringArray = request.headers()
                .values(HeaderNames.create("enum_header_string_array"))
                .stream()
                .map(String::valueOf)
                .map(v -> validator.check("enum_header_string_array",
                     v,
                     List.of(">",
                             "$")))
                .toList();

        Optional<String> enumHeaderString = request.headers()
                .first(HeaderNames.create("enum_header_string"))
                .or(() -> Optional.of("-efg"))
                .map(v -> validator.check("enum_header_string",
                     v,
                     List.of("_abc",
                             "-efg",
                             "(xyz)")));

        List<String> enumQueryStringArray = request.query()
                .all("enum_query_string_array")
                .stream()
                .map(String::valueOf)
                .map(v -> validator.check("enum_query_string_array",
                     v,
                     List.of(">",
                             "$")))
                .toList();

        Optional<String> enumQueryString = request.query()
                .first("enum_query_string")
                .asOptional()
                .or(() -> Optional.of("-efg"))
                .map(v -> validator.check("enum_query_string",
                     v,
                     List.of("_abc",
                             "-efg",
                             "(xyz)")));

        Optional<Integer> enumQueryInteger = request.query()
                .first("enum_query_integer")
                .map(Integer::valueOf)
                .map(v -> validator.check("enum_query_integer",
                     v,
                     List.of(1,
                             -2)))
                .or(Optional::empty);

        Optional<Double> enumQueryDouble = request.query()
                .first("enum_query_double")
                .map(Double::valueOf)
                .map(v -> validator.check("enum_query_double",
                     v,
                     List.of(1.1,
                             -1.2)))
                .or(Optional::empty);

        List<EnumClass> enumQueryModelArray = request.query()
                .all("enum_query_model_array")
                .stream()
                .map(EnumClass::fromValue)
                .toList();

        List<String> enumFormStringArray = formParams
                .all("enum_form_string_array")
                .stream()
                .map(String::valueOf)
                .map(v -> validator.check("enum_form_string_array",
                     v,
                     List.of(">",
                             "$")))
                .collect(HCollectors.toDefaultedList("$",
                                                    String::valueOf);

        Optional<String> enumFormString = formParams
                .first("enum_form_string")
                .asOptional()
                .or(() -> Optional.of("-efg"))
                .map(v -> validator.check("enum_form_string",
                     v,
                     List.of("_abc",
                             "-efg",
                             "(xyz)")));


        Errors errors = errorsCollector.collect();
        errors.log(LOGGER);
        if (errors.hasFatal()) {
            throw new ValidationException("Validation errors: " + errors);
        }
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testGroupParameters(ServerRequest request, ServerResponse response) {
        Errors.Collector errorsCollector = Errors.collector();
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        Integer requiredStringGroup = request.query()
                .first("required_string_group")
                .map(v -> validator.require("required_string_group", v))
                .map(Integer::valueOf)
                .orElse(null);

        Boolean requiredBooleanGroup = request.headers()
                .first(HeaderNames.create("required_boolean_group"))
                .map(v -> validator.require("required_boolean_group", v))
                .map(Boolean::valueOf)
                .orElse(null);

        Long requiredInt64Group = request.query()
                .first("required_int64_group")
                .map(v -> validator.require("required_int64_group", v))
                .map(Long::valueOf)
                .orElse(null);

        Optional<Integer> stringGroup = request.query()
                .first("string_group")
                .map(Integer::valueOf)
                .or(Optional::empty);

        Optional<Boolean> booleanGroup = request.headers()
                .first(HeaderNames.create("boolean_group"))
                .map(Boolean::valueOf)
                .or(Optional::empty);

        Optional<Long> int64Group = request.query()
                .first("int64_group")
                .map(Long::valueOf)
                .or(Optional::empty);


        Errors errors = errorsCollector.collect();
        errors.log(LOGGER);
        if (errors.hasFatal()) {
            throw new ValidationException("Validation errors: " + errors);
        }
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testInlineAdditionalProperties(ServerRequest request, ServerResponse response) {
        Errors.Collector errorsCollector = Errors.collector();
        ValidatorUtils.Validator validator = ValidatorUtils.validator();


        // TODO - user code needed to handle Map for parameter requestBody.
        Errors errors = errorsCollector.collect();
        errors.log(LOGGER);
        if (errors.hasFatal()) {
            throw new ValidationException("Validation errors: " + errors);
        }
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testInlineFreeformAdditionalProperties(ServerRequest request, ServerResponse response) {
        Errors.Collector errorsCollector = Errors.collector();
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        TestInlineFreeformAdditionalPropertiesRequest testInlineFreeformAdditionalPropertiesRequest = request.content().as(TestInlineFreeformAdditionalPropertiesRequest.class);


        Errors errors = errorsCollector.collect();
        errors.log(LOGGER);
        if (errors.hasFatal()) {
            throw new ValidationException("Validation errors: " + errors);
        }
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testJsonFormData(ServerRequest request, ServerResponse response) {
        Parameters formParams = request.content().as(Parameters.class);

        Errors.Collector errorsCollector = Errors.collector();
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        String param = formParams
                .first("param")
                .asOptional()
                .map(v -> validator.require("param", v))
                .orElse(null);

        String param2 = formParams
                .first("param2")
                .asOptional()
                .map(v -> validator.require("param2", v))
                .orElse(null);


        Errors errors = errorsCollector.collect();
        errors.log(LOGGER);
        if (errors.hasFatal()) {
            throw new ValidationException("Validation errors: " + errors);
        }
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testNullable(ServerRequest request, ServerResponse response) {
        Errors.Collector errorsCollector = Errors.collector();
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        ChildWithNullable childWithNullable = request.content().as(ChildWithNullable.class);


        Errors errors = errorsCollector.collect();
        errors.log(LOGGER);
        if (errors.hasFatal()) {
            throw new ValidationException("Validation errors: " + errors);
        }
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testQueryParameterCollectionFormat(ServerRequest request, ServerResponse response) {
        Errors.Collector errorsCollector = Errors.collector();
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        List<String> pipe = request.query()
                .all("pipe")
                .stream()
                .map(String::valueOf)
                .collect(HCollectors.toRequiredList("pipe",
                                                    validator);

        List<String> ioutil = request.query()
                .all("ioutil")
                .stream()
                .map(String::valueOf)
                .collect(HCollectors.toRequiredList("ioutil",
                                                    validator);

        List<String> http = request.query()
                .all("http")
                .stream()
                .map(String::valueOf)
                .collect(HCollectors.toRequiredList("http",
                                                    validator);

        List<String> url = request.query()
                .all("url")
                .stream()
                .map(String::valueOf)
                .collect(HCollectors.toRequiredList("url",
                                                    validator);

        List<String> context = request.query()
                .all("context")
                .stream()
                .map(String::valueOf)
                .collect(HCollectors.toRequiredList("context",
                                                    validator);

        String allowEmpty = request.query()
                .first("allowEmpty")
                .asOptional()
                .map(v -> validator.require("allowEmpty", v))
                .orElse(null);


        // TODO - user code needed to handle Map for parameter language.
        Errors errors = errorsCollector.collect();
        errors.log(LOGGER);
        if (errors.hasFatal()) {
            throw new ValidationException("Validation errors: " + errors);
        }
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testStringMapReference(ServerRequest request, ServerResponse response) {
        Errors.Collector errorsCollector = Errors.collector();
        ValidatorUtils.Validator validator = ValidatorUtils.validator();


        // TODO - user code needed to handle Map for parameter requestBody.
        Errors errors = errorsCollector.collect();
        errors.log(LOGGER);
        if (errors.hasFatal()) {
            throw new ValidationException("Validation errors: " + errors);
        }
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    @Override
    public void afterStop() {
        System.out.println("Service FakeService is down. Goodbye!");
    }
}
