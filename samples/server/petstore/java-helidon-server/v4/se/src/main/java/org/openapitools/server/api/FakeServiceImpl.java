package org.openapitools.server.api;

import java.math.BigDecimal;
import org.openapitools.server.model.ChildWithNullable;
import org.openapitools.server.model.Client;
import org.openapitools.server.model.EnumClass;
import org.openapitools.server.model.FakeBigDecimalMap200Response;
import java.io.File;
import org.openapitools.server.model.FileSchemaTestClass;
import java.nio.file.Files;
import io.helidon.common.GenericType;
import io.helidon.http.HeaderNames;
import io.helidon.http.Headers;
import org.openapitools.server.model.HealthCheckResult;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.time.LocalDate;
import java.util.Map;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.time.OffsetDateTime;
import java.util.Optional;
import org.openapitools.server.model.OuterComposite;
import org.openapitools.server.model.OuterObjectWithEnumProperty;
import io.helidon.common.parameters.Parameters;
import java.nio.file.Path;
import org.openapitools.server.model.Pet;
import io.helidon.http.Status;
import java.util.function.Supplier;
import org.openapitools.server.model.TestInlineFreeformAdditionalPropertiesRequest;
import java.io.UncheckedIOException;
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
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        validator.execute();
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void fakeHealthGet(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        validator.execute();
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void fakeHttpSignatureTest(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        Pet pet = request.content().as(Pet.class);

        Optional<String> query1 = request.query()
                .first("query_1")
                .asOptional()
                .or(Optional::empty);
        Optional<String> header1 = request.headers()
                .first(HeaderNames.create("header_1"))
                .or(Optional::empty);
        validator.execute();
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void fakeOuterBooleanSerialize(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        Optional<Boolean> body = Optional.ofNullable(request.content().hasEntity()
                ? request.content().as(Boolean.class)
                : null);

        validator.execute();
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void fakeOuterCompositeSerialize(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        Optional<OuterComposite> outerComposite = Optional.ofNullable(request.content().hasEntity()
                ? request.content().as(OuterComposite.class)
                : null);

        validator.execute();
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void fakeOuterNumberSerialize(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        Optional<BigDecimal> body = Optional.ofNullable(request.content().hasEntity()
                ? request.content().as(BigDecimal.class)
                : null);

        validator.execute();
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void fakeOuterStringSerialize(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        Optional<String> body = Optional.ofNullable(request.content().hasEntity()
                ? request.content().as(String.class)
                : null);

        validator.execute();
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void fakePropertyEnumIntegerSerialize(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        OuterObjectWithEnumProperty outerObjectWithEnumProperty = request.content().as(OuterObjectWithEnumProperty.class);

        validator.execute();
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testAdditionalPropertiesReference(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        // TODO - user code needed to handle Map for parameter requestBody.
        validator.execute();
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testBodyWithBinary(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        Supplier<InputStream> body = request.content()::inputStream;

        validator.execute();
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testBodyWithFileSchema(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        FileSchemaTestClass fileSchemaTestClass = request.content().as(FileSchemaTestClass.class);

        validator.execute();
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testBodyWithQueryParams(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        String query = request.query()
                .first("query")
                .asOptional()
                .map(v -> validator.require("query", v))
                .orElse(null);
        User user = request.content().as(User.class);

        validator.execute();
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testClientModel(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        Client client = request.content().as(Client.class);

        validator.execute();
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testEndpointParameters(ServerRequest request, ServerResponse response) {
        Parameters formParams = request.content().as(Parameters.class);

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


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
        validator.execute();
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testEnumParameters(ServerRequest request, ServerResponse response) {
        Parameters formParams = request.content().as(Parameters.class);

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


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
        validator.execute();
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testGroupParameters(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


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
        validator.execute();
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testInlineAdditionalProperties(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        // TODO - user code needed to handle Map for parameter requestBody.
        validator.execute();
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testInlineFreeformAdditionalProperties(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        TestInlineFreeformAdditionalPropertiesRequest testInlineFreeformAdditionalPropertiesRequest = request.content().as(TestInlineFreeformAdditionalPropertiesRequest.class);

        validator.execute();
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testJsonFormData(ServerRequest request, ServerResponse response) {
        Parameters formParams = request.content().as(Parameters.class);

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


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
        validator.execute();
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testNullable(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        ChildWithNullable childWithNullable = request.content().as(ChildWithNullable.class);

        validator.execute();
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testQueryParameterCollectionFormat(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


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
        validator.execute();
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void testStringMapReference(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        // TODO - user code needed to handle Map for parameter requestBody.
        validator.execute();
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    @Override
    public void afterStop() {
        System.out.println("Service FakeService is down. Goodbye!");
    }




    /**
     * Responses for operation {@code fakeBigDecimalMap } organized by response status.
     * <p>
     *     Once your code determines which (if any) response to send, it can use the static {@code create} method and pass
     *     the required elements of the response, then assign any optional response elements using the record fields.
     * <p>
     *     Finally, invoke the {@code apply} method, passing the original {@link ServerResponse}; the method sets any headers
     *     you have assigned, sets the correct status in the response, and sends the response including any appropriate
     *     entity.
     * </p>
     */
    interface FakeBigDecimalMapResult {

        /**
         * Result for HTTP status 200.
         *
         * @param response 
         */
        record $200(FakeBigDecimalMap200Response response) {

            /**
             * Creates a result for the status 200 result
             * for the fakeBigDecimalMap operation, accepting all the required result values.
             *
             * @return new result data for status 200
             */
            static $200 create() {
                return new $200(null);
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(200));
                if (response != null) { 
                    serverResponse.send(response);
                } else {
                    serverResponse.send();
                }
                return serverResponse;
            }
        }
    }

    /**
     * Responses for operation {@code fakeHealthGet } organized by response status.
     * <p>
     *     Once your code determines which (if any) response to send, it can use the static {@code create} method and pass
     *     the required elements of the response, then assign any optional response elements using the record fields.
     * <p>
     *     Finally, invoke the {@code apply} method, passing the original {@link ServerResponse}; the method sets any headers
     *     you have assigned, sets the correct status in the response, and sends the response including any appropriate
     *     entity.
     * </p>
     */
    interface FakeHealthGetResult {

        /**
         * Result for HTTP status 200.
         *
         * @param response 
         */
        record $200(HealthCheckResult response) {

            /**
             * Creates a result for the status 200 result
             * for the fakeHealthGet operation, accepting all the required result values.
             *
             * @return new result data for status 200
             */
            static $200 create() {
                return new $200(null);
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(200));
                if (response != null) { 
                    serverResponse.send(response);
                } else {
                    serverResponse.send();
                }
                return serverResponse;
            }
        }
    }

    /**
     * Responses for operation {@code fakeHttpSignatureTest } organized by response status.
     * <p>
     *     Once your code determines which (if any) response to send, it can use the static {@code create} method and pass
     *     the required elements of the response, then assign any optional response elements using the record fields.
     * <p>
     *     Finally, invoke the {@code apply} method, passing the original {@link ServerResponse}; the method sets any headers
     *     you have assigned, sets the correct status in the response, and sends the response including any appropriate
     *     entity.
     * </p>
     */
    interface FakeHttpSignatureTestResult {

        /**
         * Result for HTTP status 200.
         */
        record $200() {

            /**
             * Creates a result for the status 200 result
             * for the fakeHttpSignatureTest operation, accepting all the required result values.
             *
             * @return new result data for status 200
             */
            static $200 create() {
                return new $200();
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(200));
                serverResponse.send();
                return serverResponse;
            }
        }
    }

    /**
     * Responses for operation {@code fakeOuterBooleanSerialize } organized by response status.
     * <p>
     *     Once your code determines which (if any) response to send, it can use the static {@code create} method and pass
     *     the required elements of the response, then assign any optional response elements using the record fields.
     * <p>
     *     Finally, invoke the {@code apply} method, passing the original {@link ServerResponse}; the method sets any headers
     *     you have assigned, sets the correct status in the response, and sends the response including any appropriate
     *     entity.
     * </p>
     */
    interface FakeOuterBooleanSerializeResult {

        /**
         * Result for HTTP status 200.
         *
         * @param response 
         */
        record $200(Boolean response) {

            /**
             * Creates a result for the status 200 result
             * for the fakeOuterBooleanSerialize operation, accepting all the required result values.
             *
             * @return new result data for status 200
             */
            static $200 create() {
                return new $200(null);
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(200));
                if (response != null) { 
                    serverResponse.send(response);
                } else {
                    serverResponse.send();
                }
                return serverResponse;
            }
        }
    }

    /**
     * Responses for operation {@code fakeOuterCompositeSerialize } organized by response status.
     * <p>
     *     Once your code determines which (if any) response to send, it can use the static {@code create} method and pass
     *     the required elements of the response, then assign any optional response elements using the record fields.
     * <p>
     *     Finally, invoke the {@code apply} method, passing the original {@link ServerResponse}; the method sets any headers
     *     you have assigned, sets the correct status in the response, and sends the response including any appropriate
     *     entity.
     * </p>
     */
    interface FakeOuterCompositeSerializeResult {

        /**
         * Result for HTTP status 200.
         *
         * @param response 
         */
        record $200(OuterComposite response) {

            /**
             * Creates a result for the status 200 result
             * for the fakeOuterCompositeSerialize operation, accepting all the required result values.
             *
             * @return new result data for status 200
             */
            static $200 create() {
                return new $200(null);
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(200));
                if (response != null) { 
                    serverResponse.send(response);
                } else {
                    serverResponse.send();
                }
                return serverResponse;
            }
        }
    }

    /**
     * Responses for operation {@code fakeOuterNumberSerialize } organized by response status.
     * <p>
     *     Once your code determines which (if any) response to send, it can use the static {@code create} method and pass
     *     the required elements of the response, then assign any optional response elements using the record fields.
     * <p>
     *     Finally, invoke the {@code apply} method, passing the original {@link ServerResponse}; the method sets any headers
     *     you have assigned, sets the correct status in the response, and sends the response including any appropriate
     *     entity.
     * </p>
     */
    interface FakeOuterNumberSerializeResult {

        /**
         * Result for HTTP status 200.
         *
         * @param response 
         */
        record $200(BigDecimal response) {

            /**
             * Creates a result for the status 200 result
             * for the fakeOuterNumberSerialize operation, accepting all the required result values.
             *
             * @return new result data for status 200
             */
            static $200 create() {
                return new $200(null);
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(200));
                if (response != null) { 
                    serverResponse.send(response);
                } else {
                    serverResponse.send();
                }
                return serverResponse;
            }
        }
    }

    /**
     * Responses for operation {@code fakeOuterStringSerialize } organized by response status.
     * <p>
     *     Once your code determines which (if any) response to send, it can use the static {@code create} method and pass
     *     the required elements of the response, then assign any optional response elements using the record fields.
     * <p>
     *     Finally, invoke the {@code apply} method, passing the original {@link ServerResponse}; the method sets any headers
     *     you have assigned, sets the correct status in the response, and sends the response including any appropriate
     *     entity.
     * </p>
     */
    interface FakeOuterStringSerializeResult {

        /**
         * Result for HTTP status 200.
         *
         * @param response 
         */
        record $200(String response) {

            /**
             * Creates a result for the status 200 result
             * for the fakeOuterStringSerialize operation, accepting all the required result values.
             *
             * @return new result data for status 200
             */
            static $200 create() {
                return new $200(null);
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(200));
                if (response != null) { 
                    serverResponse.send(response);
                } else {
                    serverResponse.send();
                }
                return serverResponse;
            }
        }
    }

    /**
     * Responses for operation {@code fakePropertyEnumIntegerSerialize } organized by response status.
     * <p>
     *     Once your code determines which (if any) response to send, it can use the static {@code create} method and pass
     *     the required elements of the response, then assign any optional response elements using the record fields.
     * <p>
     *     Finally, invoke the {@code apply} method, passing the original {@link ServerResponse}; the method sets any headers
     *     you have assigned, sets the correct status in the response, and sends the response including any appropriate
     *     entity.
     * </p>
     */
    interface FakePropertyEnumIntegerSerializeResult {

        /**
         * Result for HTTP status 200.
         *
         * @param response 
         */
        record $200(OuterObjectWithEnumProperty response) {

            /**
             * Creates a result for the status 200 result
             * for the fakePropertyEnumIntegerSerialize operation, accepting all the required result values.
             *
             * @return new result data for status 200
             */
            static $200 create() {
                return new $200(null);
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(200));
                if (response != null) { 
                    serverResponse.send(response);
                } else {
                    serverResponse.send();
                }
                return serverResponse;
            }
        }
    }

    /**
     * Responses for operation {@code testAdditionalPropertiesReference } organized by response status.
     * <p>
     *     Once your code determines which (if any) response to send, it can use the static {@code create} method and pass
     *     the required elements of the response, then assign any optional response elements using the record fields.
     * <p>
     *     Finally, invoke the {@code apply} method, passing the original {@link ServerResponse}; the method sets any headers
     *     you have assigned, sets the correct status in the response, and sends the response including any appropriate
     *     entity.
     * </p>
     */
    interface TestAdditionalPropertiesReferenceResult {

        /**
         * Result for HTTP status 200.
         */
        record $200() {

            /**
             * Creates a result for the status 200 result
             * for the testAdditionalPropertiesReference operation, accepting all the required result values.
             *
             * @return new result data for status 200
             */
            static $200 create() {
                return new $200();
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(200));
                serverResponse.send();
                return serverResponse;
            }
        }
    }

    /**
     * Responses for operation {@code testBodyWithBinary } organized by response status.
     * <p>
     *     Once your code determines which (if any) response to send, it can use the static {@code create} method and pass
     *     the required elements of the response, then assign any optional response elements using the record fields.
     * <p>
     *     Finally, invoke the {@code apply} method, passing the original {@link ServerResponse}; the method sets any headers
     *     you have assigned, sets the correct status in the response, and sends the response including any appropriate
     *     entity.
     * </p>
     */
    interface TestBodyWithBinaryResult {

        /**
         * Result for HTTP status 200.
         */
        record $200() {

            /**
             * Creates a result for the status 200 result
             * for the testBodyWithBinary operation, accepting all the required result values.
             *
             * @return new result data for status 200
             */
            static $200 create() {
                return new $200();
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(200));
                serverResponse.send();
                return serverResponse;
            }
        }
    }

    /**
     * Responses for operation {@code testBodyWithFileSchema } organized by response status.
     * <p>
     *     Once your code determines which (if any) response to send, it can use the static {@code create} method and pass
     *     the required elements of the response, then assign any optional response elements using the record fields.
     * <p>
     *     Finally, invoke the {@code apply} method, passing the original {@link ServerResponse}; the method sets any headers
     *     you have assigned, sets the correct status in the response, and sends the response including any appropriate
     *     entity.
     * </p>
     */
    interface TestBodyWithFileSchemaResult {

        /**
         * Result for HTTP status 200.
         */
        record $200() {

            /**
             * Creates a result for the status 200 result
             * for the testBodyWithFileSchema operation, accepting all the required result values.
             *
             * @return new result data for status 200
             */
            static $200 create() {
                return new $200();
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(200));
                serverResponse.send();
                return serverResponse;
            }
        }
    }

    /**
     * Responses for operation {@code testBodyWithQueryParams } organized by response status.
     * <p>
     *     Once your code determines which (if any) response to send, it can use the static {@code create} method and pass
     *     the required elements of the response, then assign any optional response elements using the record fields.
     * <p>
     *     Finally, invoke the {@code apply} method, passing the original {@link ServerResponse}; the method sets any headers
     *     you have assigned, sets the correct status in the response, and sends the response including any appropriate
     *     entity.
     * </p>
     */
    interface TestBodyWithQueryParamsResult {

        /**
         * Result for HTTP status 200.
         */
        record $200() {

            /**
             * Creates a result for the status 200 result
             * for the testBodyWithQueryParams operation, accepting all the required result values.
             *
             * @return new result data for status 200
             */
            static $200 create() {
                return new $200();
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(200));
                serverResponse.send();
                return serverResponse;
            }
        }
    }

    /**
     * Responses for operation {@code testClientModel } organized by response status.
     * <p>
     *     Once your code determines which (if any) response to send, it can use the static {@code create} method and pass
     *     the required elements of the response, then assign any optional response elements using the record fields.
     * <p>
     *     Finally, invoke the {@code apply} method, passing the original {@link ServerResponse}; the method sets any headers
     *     you have assigned, sets the correct status in the response, and sends the response including any appropriate
     *     entity.
     * </p>
     */
    interface TestClientModelResult {

        /**
         * Result for HTTP status 200.
         *
         * @param response 
         */
        record $200(Client response) {

            /**
             * Creates a result for the status 200 result
             * for the testClientModel operation, accepting all the required result values.
             *
             * @return new result data for status 200
             */
            static $200 create() {
                return new $200(null);
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(200));
                if (response != null) { 
                    serverResponse.send(response);
                } else {
                    serverResponse.send();
                }
                return serverResponse;
            }
        }
    }

    /**
     * Responses for operation {@code testEndpointParameters } organized by response status.
     * <p>
     *     Once your code determines which (if any) response to send, it can use the static {@code create} method and pass
     *     the required elements of the response, then assign any optional response elements using the record fields.
     * <p>
     *     Finally, invoke the {@code apply} method, passing the original {@link ServerResponse}; the method sets any headers
     *     you have assigned, sets the correct status in the response, and sends the response including any appropriate
     *     entity.
     * </p>
     */
    interface TestEndpointParametersResult {

        /**
         * Result for HTTP status 400.
         */
        record $400() {

            /**
             * Creates a result for the status 400 result
             * for the testEndpointParameters operation, accepting all the required result values.
             *
             * @return new result data for status 400
             */
            static $400 create() {
                return new $400();
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(400));
                serverResponse.send();
                return serverResponse;
            }
        }

        /**
         * Result for HTTP status 404.
         */
        record $404() {

            /**
             * Creates a result for the status 404 result
             * for the testEndpointParameters operation, accepting all the required result values.
             *
             * @return new result data for status 404
             */
            static $404 create() {
                return new $404();
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(404));
                serverResponse.send();
                return serverResponse;
            }
        }
    }

    /**
     * Responses for operation {@code testEnumParameters } organized by response status.
     * <p>
     *     Once your code determines which (if any) response to send, it can use the static {@code create} method and pass
     *     the required elements of the response, then assign any optional response elements using the record fields.
     * <p>
     *     Finally, invoke the {@code apply} method, passing the original {@link ServerResponse}; the method sets any headers
     *     you have assigned, sets the correct status in the response, and sends the response including any appropriate
     *     entity.
     * </p>
     */
    interface TestEnumParametersResult {

        /**
         * Result for HTTP status 400.
         */
        record $400() {

            /**
             * Creates a result for the status 400 result
             * for the testEnumParameters operation, accepting all the required result values.
             *
             * @return new result data for status 400
             */
            static $400 create() {
                return new $400();
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(400));
                serverResponse.send();
                return serverResponse;
            }
        }

        /**
         * Result for HTTP status 404.
         */
        record $404() {

            /**
             * Creates a result for the status 404 result
             * for the testEnumParameters operation, accepting all the required result values.
             *
             * @return new result data for status 404
             */
            static $404 create() {
                return new $404();
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(404));
                serverResponse.send();
                return serverResponse;
            }
        }
    }

    /**
     * Responses for operation {@code testGroupParameters } organized by response status.
     * <p>
     *     Once your code determines which (if any) response to send, it can use the static {@code create} method and pass
     *     the required elements of the response, then assign any optional response elements using the record fields.
     * <p>
     *     Finally, invoke the {@code apply} method, passing the original {@link ServerResponse}; the method sets any headers
     *     you have assigned, sets the correct status in the response, and sends the response including any appropriate
     *     entity.
     * </p>
     */
    interface TestGroupParametersResult {

        /**
         * Result for HTTP status 400.
         */
        record $400() {

            /**
             * Creates a result for the status 400 result
             * for the testGroupParameters operation, accepting all the required result values.
             *
             * @return new result data for status 400
             */
            static $400 create() {
                return new $400();
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(400));
                serverResponse.send();
                return serverResponse;
            }
        }
    }

    /**
     * Responses for operation {@code testInlineAdditionalProperties } organized by response status.
     * <p>
     *     Once your code determines which (if any) response to send, it can use the static {@code create} method and pass
     *     the required elements of the response, then assign any optional response elements using the record fields.
     * <p>
     *     Finally, invoke the {@code apply} method, passing the original {@link ServerResponse}; the method sets any headers
     *     you have assigned, sets the correct status in the response, and sends the response including any appropriate
     *     entity.
     * </p>
     */
    interface TestInlineAdditionalPropertiesResult {

        /**
         * Result for HTTP status 200.
         */
        record $200() {

            /**
             * Creates a result for the status 200 result
             * for the testInlineAdditionalProperties operation, accepting all the required result values.
             *
             * @return new result data for status 200
             */
            static $200 create() {
                return new $200();
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(200));
                serverResponse.send();
                return serverResponse;
            }
        }
    }

    /**
     * Responses for operation {@code testInlineFreeformAdditionalProperties } organized by response status.
     * <p>
     *     Once your code determines which (if any) response to send, it can use the static {@code create} method and pass
     *     the required elements of the response, then assign any optional response elements using the record fields.
     * <p>
     *     Finally, invoke the {@code apply} method, passing the original {@link ServerResponse}; the method sets any headers
     *     you have assigned, sets the correct status in the response, and sends the response including any appropriate
     *     entity.
     * </p>
     */
    interface TestInlineFreeformAdditionalPropertiesResult {

        /**
         * Result for HTTP status 200.
         */
        record $200() {

            /**
             * Creates a result for the status 200 result
             * for the testInlineFreeformAdditionalProperties operation, accepting all the required result values.
             *
             * @return new result data for status 200
             */
            static $200 create() {
                return new $200();
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(200));
                serverResponse.send();
                return serverResponse;
            }
        }
    }

    /**
     * Responses for operation {@code testJsonFormData } organized by response status.
     * <p>
     *     Once your code determines which (if any) response to send, it can use the static {@code create} method and pass
     *     the required elements of the response, then assign any optional response elements using the record fields.
     * <p>
     *     Finally, invoke the {@code apply} method, passing the original {@link ServerResponse}; the method sets any headers
     *     you have assigned, sets the correct status in the response, and sends the response including any appropriate
     *     entity.
     * </p>
     */
    interface TestJsonFormDataResult {

        /**
         * Result for HTTP status 200.
         */
        record $200() {

            /**
             * Creates a result for the status 200 result
             * for the testJsonFormData operation, accepting all the required result values.
             *
             * @return new result data for status 200
             */
            static $200 create() {
                return new $200();
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(200));
                serverResponse.send();
                return serverResponse;
            }
        }
    }

    /**
     * Responses for operation {@code testNullable } organized by response status.
     * <p>
     *     Once your code determines which (if any) response to send, it can use the static {@code create} method and pass
     *     the required elements of the response, then assign any optional response elements using the record fields.
     * <p>
     *     Finally, invoke the {@code apply} method, passing the original {@link ServerResponse}; the method sets any headers
     *     you have assigned, sets the correct status in the response, and sends the response including any appropriate
     *     entity.
     * </p>
     */
    interface TestNullableResult {

        /**
         * Result for HTTP status 200.
         */
        record $200() {

            /**
             * Creates a result for the status 200 result
             * for the testNullable operation, accepting all the required result values.
             *
             * @return new result data for status 200
             */
            static $200 create() {
                return new $200();
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(200));
                serverResponse.send();
                return serverResponse;
            }
        }
    }

    /**
     * Responses for operation {@code testQueryParameterCollectionFormat } organized by response status.
     * <p>
     *     Once your code determines which (if any) response to send, it can use the static {@code create} method and pass
     *     the required elements of the response, then assign any optional response elements using the record fields.
     * <p>
     *     Finally, invoke the {@code apply} method, passing the original {@link ServerResponse}; the method sets any headers
     *     you have assigned, sets the correct status in the response, and sends the response including any appropriate
     *     entity.
     * </p>
     */
    interface TestQueryParameterCollectionFormatResult {

        /**
         * Result for HTTP status 200.
         */
        record $200() {

            /**
             * Creates a result for the status 200 result
             * for the testQueryParameterCollectionFormat operation, accepting all the required result values.
             *
             * @return new result data for status 200
             */
            static $200 create() {
                return new $200();
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(200));
                serverResponse.send();
                return serverResponse;
            }
        }
    }

    /**
     * Responses for operation {@code testStringMapReference } organized by response status.
     * <p>
     *     Once your code determines which (if any) response to send, it can use the static {@code create} method and pass
     *     the required elements of the response, then assign any optional response elements using the record fields.
     * <p>
     *     Finally, invoke the {@code apply} method, passing the original {@link ServerResponse}; the method sets any headers
     *     you have assigned, sets the correct status in the response, and sends the response including any appropriate
     *     entity.
     * </p>
     */
    interface TestStringMapReferenceResult {

        /**
         * Result for HTTP status 200.
         */
        record $200() {

            /**
             * Creates a result for the status 200 result
             * for the testStringMapReference operation, accepting all the required result values.
             *
             * @return new result data for status 200
             */
            static $200 create() {
                return new $200();
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(200));
                serverResponse.send();
                return serverResponse;
            }
        }
    }

}
