package org.openapitools.server.api;

import java.util.ArrayList;
import java.math.BigDecimal;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import org.openapitools.server.model.Client;
import io.helidon.common.http.DataChunk;
import org.openapitools.server.model.EnumClass;
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
import java.io.UncheckedIOException;
import org.openapitools.server.model.User;

import java.util.Optional;
import java.util.logging.Logger;

import io.helidon.common.GenericType;
import io.helidon.common.reactive.Single;
import io.helidon.config.Config;
import io.helidon.webserver.Routing;
import io.helidon.webserver.ServerRequest;
import io.helidon.webserver.ServerResponse;
import io.helidon.webserver.Service;


public abstract class FakeService implements Service { 

    protected static final Logger LOGGER = Logger.getLogger(FakeService.class.getName());
    private static final ObjectMapper MAPPER = JsonProvider.objectMapper();

    protected final Config config;

    public FakeService(Config config) {
        this.config = config;
    }

    /**
     * A service registers itself by updating the routing rules.
     * @param rules the routing rules.
     */
    @Override
    public void update(Routing.Rules rules) {
        rules.get("/fake/health", this::fakeHealthGet);
        rules.get("/fake/http-signature-test", Handler.create(Pet.class, this::fakeHttpSignatureTest));
        rules.post("/fake/outer/boolean", this::fakeOuterBooleanSerialize);
        rules.post("/fake/outer/composite", Handler.create(OuterComposite.class, this::fakeOuterCompositeSerialize));
        rules.post("/fake/outer/number", this::fakeOuterNumberSerialize);
        rules.post("/fake/outer/string", this::fakeOuterStringSerialize);
        rules.post("/fake/property/enum-int", Handler.create(OuterObjectWithEnumProperty.class, this::fakePropertyEnumIntegerSerialize));
        rules.put("/fake/body-with-binary", this::testBodyWithBinary);
        rules.put("/fake/body-with-file-schema", Handler.create(FileSchemaTestClass.class, this::testBodyWithFileSchema));
        rules.put("/fake/body-with-query-params", Handler.create(User.class, this::testBodyWithQueryParams));
        rules.patch("/fake", Handler.create(Client.class, this::testClientModel));
        rules.post("/fake", this::testEndpointParameters);
        rules.get("/fake", this::testEnumParameters);
        rules.delete("/fake", this::testGroupParameters);
        rules.post("/fake/inline-additionalProperties", this::testInlineAdditionalProperties);
        rules.get("/fake/jsonFormData", this::testJsonFormData);
        rules.put("/fake/test-query-parameters", this::testQueryParameterCollectionFormat);
    }


    private void processNonFileFormField(String name, Map<String, List<String>> nonFileFormContent, ReadableBodyPart part) {
        List<String> content = nonFileFormContent.computeIfAbsent(name, key -> new ArrayList<>());
        part.content().as(String.class).thenAccept(content::add);
    }

    private void processFileFormField(String name, Map<String, List<InputStream>> fileFormContent, ReadableBodyPart part) {
        List<InputStream> content = fileFormContent.computeIfAbsent(name, key -> new ArrayList<>());
        part.content().map(DataChunk::bytes)
            .collect(ByteArrayOutputStream::new, (stream, bytes) -> {
                try {
                    stream.write(bytes);
                } catch (IOException e) {
                    throw new UncheckedIOException(e);
                }
        })
        .thenAccept(byteStream -> content.add(new ByteArrayInputStream(byteStream.toByteArray())));
    }


    /**
     * GET /fake/health : Health check endpoint.
     * @param request the server request
     * @param response the server response
     */
    void fakeHealthGet(ServerRequest request, ServerResponse response) { 
        Single.create(Single.empty())
            .thenAccept(val -> {
                
                handleFakeHealthGet(request, response);
            })
            .exceptionally(throwable -> handleError(request, response, throwable));
    }
    /**
     * Handle GET /fake/health : Health check endpoint.
     * @param request the server request
     * @param response the server response
     */
    abstract void handleFakeHealthGet(ServerRequest request, ServerResponse response);


    /**
     * GET /fake/http-signature-test : test http signature authentication.
     * @param request the server request
     * @param response the server response
     * @param pet Pet object that needs to be added to the store 
     */
    void fakeHttpSignatureTest(ServerRequest request, ServerResponse response, Pet pet) { 
        Single.create(Single.empty())
            .thenAccept(val -> {
                ValidatorUtils.checkNonNull(pet);
                String query1 = request.queryParams().toMap().getOrDefault("query_1", List.of()).stream().findFirst().orElse(null);
                String header1 = request.headers().value("header_1").orElse(null);
                
                handleFakeHttpSignatureTest(request, response, pet, query1, header1);
            })
            .exceptionally(throwable -> handleError(request, response, throwable));
    }
    /**
     * Handle GET /fake/http-signature-test : test http signature authentication.
     * @param request the server request
     * @param response the server response
     * @param pet Pet object that needs to be added to the store 
     * @param query1 query parameter 
     * @param header1 header parameter 
     */
    abstract void handleFakeHttpSignatureTest(ServerRequest request, ServerResponse response, Pet pet, String query1, String header1);


    /**
     * POST /fake/outer/boolean.
     * @param request the server request
     * @param response the server response
     */
    void fakeOuterBooleanSerialize(ServerRequest request, ServerResponse response) { 
        Single.create(request.content().as(new GenericType<Boolean>() { }))
            .thenAccept(body -> {
                ValidatorUtils.checkNonNull(body);
                
                handleFakeOuterBooleanSerialize(request, response, body);
            })
            .exceptionally(throwable -> handleError(request, response, throwable));
    }
    /**
     * Handle POST /fake/outer/boolean.
     * @param request the server request
     * @param response the server response
     * @param body Input boolean as post body 
     */
    abstract void handleFakeOuterBooleanSerialize(ServerRequest request, ServerResponse response, Boolean body);


    /**
     * POST /fake/outer/composite.
     * @param request the server request
     * @param response the server response
     * @param outerComposite Input composite as post body 
     */
    void fakeOuterCompositeSerialize(ServerRequest request, ServerResponse response, OuterComposite outerComposite) { 
        Single.create(Single.empty())
            .thenAccept(val -> {
                ValidatorUtils.checkNonNull(outerComposite);
                
                handleFakeOuterCompositeSerialize(request, response, outerComposite);
            })
            .exceptionally(throwable -> handleError(request, response, throwable));
    }
    /**
     * Handle POST /fake/outer/composite.
     * @param request the server request
     * @param response the server response
     * @param outerComposite Input composite as post body 
     */
    abstract void handleFakeOuterCompositeSerialize(ServerRequest request, ServerResponse response, OuterComposite outerComposite);


    /**
     * POST /fake/outer/number.
     * @param request the server request
     * @param response the server response
     */
    void fakeOuterNumberSerialize(ServerRequest request, ServerResponse response) { 
        Single.create(request.content().as(new GenericType<BigDecimal>() { }))
            .thenAccept(body -> {
                ValidatorUtils.checkNonNull(body);
                
                handleFakeOuterNumberSerialize(request, response, body);
            })
            .exceptionally(throwable -> handleError(request, response, throwable));
    }
    /**
     * Handle POST /fake/outer/number.
     * @param request the server request
     * @param response the server response
     * @param body Input number as post body 
     */
    abstract void handleFakeOuterNumberSerialize(ServerRequest request, ServerResponse response, BigDecimal body);


    /**
     * POST /fake/outer/string.
     * @param request the server request
     * @param response the server response
     */
    void fakeOuterStringSerialize(ServerRequest request, ServerResponse response) { 
        Single.create(request.content().as(new GenericType<String>() { }))
            .thenAccept(body -> {
                ValidatorUtils.checkNonNull(body);
                
                handleFakeOuterStringSerialize(request, response, body);
            })
            .exceptionally(throwable -> handleError(request, response, throwable));
    }
    /**
     * Handle POST /fake/outer/string.
     * @param request the server request
     * @param response the server response
     * @param body Input string as post body 
     */
    abstract void handleFakeOuterStringSerialize(ServerRequest request, ServerResponse response, String body);


    /**
     * POST /fake/property/enum-int.
     * @param request the server request
     * @param response the server response
     * @param outerObjectWithEnumProperty Input enum (int) as post body 
     */
    void fakePropertyEnumIntegerSerialize(ServerRequest request, ServerResponse response, OuterObjectWithEnumProperty outerObjectWithEnumProperty) { 
        Single.create(Single.empty())
            .thenAccept(val -> {
                ValidatorUtils.checkNonNull(outerObjectWithEnumProperty);
                
                handleFakePropertyEnumIntegerSerialize(request, response, outerObjectWithEnumProperty);
            })
            .exceptionally(throwable -> handleError(request, response, throwable));
    }
    /**
     * Handle POST /fake/property/enum-int.
     * @param request the server request
     * @param response the server response
     * @param outerObjectWithEnumProperty Input enum (int) as post body 
     */
    abstract void handleFakePropertyEnumIntegerSerialize(ServerRequest request, ServerResponse response, OuterObjectWithEnumProperty outerObjectWithEnumProperty);


    /**
     * PUT /fake/body-with-binary.
     * @param request the server request
     * @param response the server response
     */
    void testBodyWithBinary(ServerRequest request, ServerResponse response) { 
        Single.create(request.content().as(new GenericType<File>() { }))
            .thenAccept(body -> {
                ValidatorUtils.checkNonNull(body);
                
                handleTestBodyWithBinary(request, response, body);
            })
            .exceptionally(throwable -> handleError(request, response, throwable));
    }
    /**
     * Handle PUT /fake/body-with-binary.
     * @param request the server request
     * @param response the server response
     * @param body image to upload 
     */
    abstract void handleTestBodyWithBinary(ServerRequest request, ServerResponse response, InputStream body);


    /**
     * PUT /fake/body-with-file-schema.
     * @param request the server request
     * @param response the server response
     * @param fileSchemaTestClass fileSchemaTestClass 
     */
    void testBodyWithFileSchema(ServerRequest request, ServerResponse response, FileSchemaTestClass fileSchemaTestClass) { 
        Single.create(Single.empty())
            .thenAccept(val -> {
                ValidatorUtils.checkNonNull(fileSchemaTestClass);
                
                handleTestBodyWithFileSchema(request, response, fileSchemaTestClass);
            })
            .exceptionally(throwable -> handleError(request, response, throwable));
    }
    /**
     * Handle PUT /fake/body-with-file-schema.
     * @param request the server request
     * @param response the server response
     * @param fileSchemaTestClass fileSchemaTestClass 
     */
    abstract void handleTestBodyWithFileSchema(ServerRequest request, ServerResponse response, FileSchemaTestClass fileSchemaTestClass);


    /**
     * PUT /fake/body-with-query-params.
     * @param request the server request
     * @param response the server response
     * @param user user 
     */
    void testBodyWithQueryParams(ServerRequest request, ServerResponse response, User user) { 
        Single.create(Single.empty())
            .thenAccept(val -> {
                String query = request.queryParams().toMap().getOrDefault("query", List.of()).stream().findFirst().orElse(null);
                ValidatorUtils.checkNonNull(query);
                ValidatorUtils.checkNonNull(user);
                
                handleTestBodyWithQueryParams(request, response, query, user);
            })
            .exceptionally(throwable -> handleError(request, response, throwable));
    }
    /**
     * Handle PUT /fake/body-with-query-params.
     * @param request the server request
     * @param response the server response
     * @param query query 
     * @param user user 
     */
    abstract void handleTestBodyWithQueryParams(ServerRequest request, ServerResponse response, String query, User user);


    /**
     * PATCH /fake : To test \&quot;client\&quot; model.
     * @param request the server request
     * @param response the server response
     * @param client client model 
     */
    void testClientModel(ServerRequest request, ServerResponse response, Client client) { 
        Single.create(Single.empty())
            .thenAccept(val -> {
                ValidatorUtils.checkNonNull(client);
                
                handleTestClientModel(request, response, client);
            })
            .exceptionally(throwable -> handleError(request, response, throwable));
    }
    /**
     * Handle PATCH /fake : To test \&quot;client\&quot; model.
     * @param request the server request
     * @param response the server response
     * @param client client model 
     */
    abstract void handleTestClientModel(ServerRequest request, ServerResponse response, Client client);


    /**
     * POST /fake : Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 .
     * @param request the server request
     * @param response the server response
     */
    void testEndpointParameters(ServerRequest request, ServerResponse response) { 
        Map<String, List<String>> nonFileFormContent = new HashMap<>();
        Map<String, List<InputStream>> fileFormContent = new HashMap<>();
        Single<Void> formSingle = request.content().asStream(ReadableBodyPart.class)
                                         .forEach(part -> {
                                            String name = part.name();
                                            if ("integer".equals(name)) {
                                                processNonFileFormField(name, nonFileFormContent, part);
                                            }
                                            if ("int32".equals(name)) {
                                                processNonFileFormField(name, nonFileFormContent, part);
                                            }
                                            if ("int64".equals(name)) {
                                                processNonFileFormField(name, nonFileFormContent, part);
                                            }
                                            if ("number".equals(name)) {
                                                processNonFileFormField(name, nonFileFormContent, part);
                                            }
                                            if ("float".equals(name)) {
                                                processNonFileFormField(name, nonFileFormContent, part);
                                            }
                                            if ("double".equals(name)) {
                                                processNonFileFormField(name, nonFileFormContent, part);
                                            }
                                            if ("string".equals(name)) {
                                                processNonFileFormField(name, nonFileFormContent, part);
                                            }
                                            if ("pattern_without_delimiter".equals(name)) {
                                                processNonFileFormField(name, nonFileFormContent, part);
                                            }
                                            if ("byte".equals(name)) {
                                                processNonFileFormField(name, nonFileFormContent, part);
                                            }
                                            if ("binary".equals(name)) {
                                                processFileFormField(name, fileFormContent, part);
                                            }
                                            if ("date".equals(name)) {
                                                processNonFileFormField(name, nonFileFormContent, part);
                                            }
                                            if ("dateTime".equals(name)) {
                                                processNonFileFormField(name, nonFileFormContent, part);
                                            }
                                            if ("password".equals(name)) {
                                                processNonFileFormField(name, nonFileFormContent, part);
                                            }
                                            if ("callback".equals(name)) {
                                                processNonFileFormField(name, nonFileFormContent, part);
                                            }
                                            part.drain();
                                         });
        Single.create(formSingle)
            .thenAccept(val -> {
                BigDecimal number = Optional.ofNullable(nonFileFormContent.get("number")).flatMap(list->list.stream().findFirst()).map(BigDecimal::new).orElse(null);
                ValidatorUtils.checkNonNull(number);
                ValidatorUtils.validateMin(number, "32.1", true);
                ValidatorUtils.validateMax(number, "543.2", true);
                Double _double = Optional.ofNullable(nonFileFormContent.get("double")).flatMap(list->list.stream().findFirst()).map(Double::valueOf).orElse(null);
                ValidatorUtils.checkNonNull(_double);
                ValidatorUtils.validateMin(_double, "67.8", true);
                ValidatorUtils.validateMax(_double, "123.4", true);
                String patternWithoutDelimiter = Optional.ofNullable(nonFileFormContent.get("pattern_without_delimiter")).flatMap(list->list.stream().findFirst()).orElse(null);
                ValidatorUtils.checkNonNull(patternWithoutDelimiter);
                ValidatorUtils.validatePattern(patternWithoutDelimiter, "^[A-Z].*");
                byte[] _byte = Optional.ofNullable(nonFileFormContent.get("byte")).flatMap(list->list.stream().findFirst()).map(byte[]::valueOf).orElse(null);
                ValidatorUtils.checkNonNull(_byte);
                Integer integer = Optional.ofNullable(nonFileFormContent.get("integer")).flatMap(list->list.stream().findFirst()).map(Integer::valueOf).orElse(null);
                ValidatorUtils.validateMin(integer, 10);
                ValidatorUtils.validateMax(integer, 100);
                Integer int32 = Optional.ofNullable(nonFileFormContent.get("int32")).flatMap(list->list.stream().findFirst()).map(Integer::valueOf).orElse(null);
                ValidatorUtils.validateMin(int32, 20);
                ValidatorUtils.validateMax(int32, 200);
                Long int64 = Optional.ofNullable(nonFileFormContent.get("int64")).flatMap(list->list.stream().findFirst()).map(Long::valueOf).orElse(null);
                Float _float = Optional.ofNullable(nonFileFormContent.get("float")).flatMap(list->list.stream().findFirst()).map(Float::valueOf).orElse(null);
                ValidatorUtils.validateMax(_float, "987.6", true);
                String string = Optional.ofNullable(nonFileFormContent.get("string")).flatMap(list->list.stream().findFirst()).orElse(null);
                ValidatorUtils.validatePattern(string, "/[a-z]/i");
                InputStream binary = Optional.ofNullable(fileFormContent.get("binary")).flatMap(list->list.stream().findFirst()).orElse(null);
                LocalDate date = Optional.ofNullable(nonFileFormContent.get("date")).flatMap(list->list.stream().findFirst()).map(LocalDate::parse).orElse(null);
                OffsetDateTime dateTime = Optional.ofNullable(nonFileFormContent.get("dateTime")).flatMap(list->list.stream().findFirst()).map(OffsetDateTime::parse).orElse(null);
                String password = Optional.ofNullable(nonFileFormContent.get("password")).flatMap(list->list.stream().findFirst()).orElse(null);
                ValidatorUtils.validateSize(password, 10, 64);
                String paramCallback = Optional.ofNullable(nonFileFormContent.get("callback")).flatMap(list->list.stream().findFirst()).orElse(null);
                
                handleTestEndpointParameters(request, response, number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, paramCallback);
            })
            .exceptionally(throwable -> handleError(request, response, throwable));
    }
    /**
     * Handle POST /fake : Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 .
     * @param request the server request
     * @param response the server response
     * @param number None 
     * @param _double None 
     * @param patternWithoutDelimiter None 
     * @param _byte None 
     * @param integer None 
     * @param int32 None 
     * @param int64 None 
     * @param _float None 
     * @param string None 
     * @param binary None 
     * @param date None 
     * @param dateTime None 
     * @param password None 
     * @param paramCallback None 
     */
    abstract void handleTestEndpointParameters(ServerRequest request, ServerResponse response, BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string, InputStream binary, LocalDate date, OffsetDateTime dateTime, String password, String paramCallback);


    /**
     * GET /fake : To test enum parameters.
     * @param request the server request
     * @param response the server response
     */
    void testEnumParameters(ServerRequest request, ServerResponse response) { 
        Map<String, List<String>> nonFileFormContent = new HashMap<>();
        Map<String, List<InputStream>> fileFormContent = new HashMap<>();
        Single<Void> formSingle = request.content().asStream(ReadableBodyPart.class)
                                         .forEach(part -> {
                                            String name = part.name();
                                            if ("enum_form_string_array".equals(name)) {
                                                processNonFileFormField(name, nonFileFormContent, part);
                                            }
                                            if ("enum_form_string".equals(name)) {
                                                processNonFileFormField(name, nonFileFormContent, part);
                                            }
                                            part.drain();
                                         });
        Single.create(formSingle)
            .thenAccept(val -> {
                List<String> enumHeaderStringArray = request.headers().value("enum_header_string_array").orElse(null);
                String enumHeaderString = request.headers().value("enum_header_string").orElse(null);
                List<String> enumQueryStringArray = Optional.ofNullable(request.queryParams().toMap().get("enum_query_string_array")).orElse(null);
                String enumQueryString = request.queryParams().toMap().getOrDefault("enum_query_string", List.of()).stream().findFirst().orElse(null);
                Integer enumQueryInteger = request.queryParams().toMap().getOrDefault("enum_query_integer", List.of()).stream().findFirst().map(Integer::valueOf).orElse(null);
                Double enumQueryDouble = request.queryParams().toMap().getOrDefault("enum_query_double", List.of()).stream().findFirst().map(Double::valueOf).orElse(null);
                List<String> enumQueryModelArray = Optional.ofNullable(request.queryParams().toMap().get("enum_query_model_array")).orElse(null);
                List<String> enumFormStringArray = Optional.ofNullable(nonFileFormContent.get("enum_form_string_array")).orElse(null);
                String enumFormString = Optional.ofNullable(nonFileFormContent.get("enum_form_string")).flatMap(list->list.stream().findFirst()).orElse(null);
                
                handleTestEnumParameters(request, response, enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumQueryModelArray, enumFormStringArray, enumFormString);
            })
            .exceptionally(throwable -> handleError(request, response, throwable));
    }
    /**
     * Handle GET /fake : To test enum parameters.
     * @param request the server request
     * @param response the server response
     * @param enumHeaderStringArray Header parameter enum test (string array) 
     * @param enumHeaderString Header parameter enum test (string) 
     * @param enumQueryStringArray Query parameter enum test (string array) 
     * @param enumQueryString Query parameter enum test (string) 
     * @param enumQueryInteger Query parameter enum test (double) 
     * @param enumQueryDouble Query parameter enum test (double) 
     * @param enumQueryModelArray enumQueryModelArray 
     * @param enumFormStringArray Form parameter enum test (string array) 
     * @param enumFormString Form parameter enum test (string) 
     */
    abstract void handleTestEnumParameters(ServerRequest request, ServerResponse response, List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble, List<String> enumQueryModelArray, List<String> enumFormStringArray, String enumFormString);


    /**
     * DELETE /fake : Fake endpoint to test group parameters (optional).
     * @param request the server request
     * @param response the server response
     */
    void testGroupParameters(ServerRequest request, ServerResponse response) { 
        Single.create(Single.empty())
            .thenAccept(val -> {
                Integer requiredStringGroup = request.queryParams().toMap().getOrDefault("required_string_group", List.of()).stream().findFirst().map(Integer::valueOf).orElse(null);
                ValidatorUtils.checkNonNull(requiredStringGroup);
                Boolean requiredBooleanGroup = request.headers().value("required_boolean_group").map(Boolean::valueOf).orElse(null);
                ValidatorUtils.checkNonNull(requiredBooleanGroup);
                Long requiredInt64Group = request.queryParams().toMap().getOrDefault("required_int64_group", List.of()).stream().findFirst().map(Long::valueOf).orElse(null);
                ValidatorUtils.checkNonNull(requiredInt64Group);
                Integer stringGroup = request.queryParams().toMap().getOrDefault("string_group", List.of()).stream().findFirst().map(Integer::valueOf).orElse(null);
                Boolean booleanGroup = request.headers().value("boolean_group").map(Boolean::valueOf).orElse(null);
                Long int64Group = request.queryParams().toMap().getOrDefault("int64_group", List.of()).stream().findFirst().map(Long::valueOf).orElse(null);
                
                handleTestGroupParameters(request, response, requiredStringGroup, requiredBooleanGroup, requiredInt64Group, stringGroup, booleanGroup, int64Group);
            })
            .exceptionally(throwable -> handleError(request, response, throwable));
    }
    /**
     * Handle DELETE /fake : Fake endpoint to test group parameters (optional).
     * @param request the server request
     * @param response the server response
     * @param requiredStringGroup Required String in group parameters 
     * @param requiredBooleanGroup Required Boolean in group parameters 
     * @param requiredInt64Group Required Integer in group parameters 
     * @param stringGroup String in group parameters 
     * @param booleanGroup Boolean in group parameters 
     * @param int64Group Integer in group parameters 
     */
    abstract void handleTestGroupParameters(ServerRequest request, ServerResponse response, Integer requiredStringGroup, Boolean requiredBooleanGroup, Long requiredInt64Group, Integer stringGroup, Boolean booleanGroup, Long int64Group);


    /**
     * POST /fake/inline-additionalProperties : test inline additionalProperties.
     * @param request the server request
     * @param response the server response
     */
    void testInlineAdditionalProperties(ServerRequest request, ServerResponse response) { 
        Single.create(request.content().as(new GenericType<Map<String, String>>() { }))
            .thenAccept(requestBody -> {
                ValidatorUtils.checkNonNull(requestBody);
                
                handleTestInlineAdditionalProperties(request, response, requestBody);
            })
            .exceptionally(throwable -> handleError(request, response, throwable));
    }
    /**
     * Handle POST /fake/inline-additionalProperties : test inline additionalProperties.
     * @param request the server request
     * @param response the server response
     * @param requestBody request body 
     */
    abstract void handleTestInlineAdditionalProperties(ServerRequest request, ServerResponse response, String requestBody);


    /**
     * GET /fake/jsonFormData : test json serialization of form data.
     * @param request the server request
     * @param response the server response
     */
    void testJsonFormData(ServerRequest request, ServerResponse response) { 
        Map<String, List<String>> nonFileFormContent = new HashMap<>();
        Map<String, List<InputStream>> fileFormContent = new HashMap<>();
        Single<Void> formSingle = request.content().asStream(ReadableBodyPart.class)
                                         .forEach(part -> {
                                            String name = part.name();
                                            if ("param".equals(name)) {
                                                processNonFileFormField(name, nonFileFormContent, part);
                                            }
                                            if ("param2".equals(name)) {
                                                processNonFileFormField(name, nonFileFormContent, part);
                                            }
                                            part.drain();
                                         });
        Single.create(formSingle)
            .thenAccept(val -> {
                String param = Optional.ofNullable(nonFileFormContent.get("param")).flatMap(list->list.stream().findFirst()).orElse(null);
                ValidatorUtils.checkNonNull(param);
                String param2 = Optional.ofNullable(nonFileFormContent.get("param2")).flatMap(list->list.stream().findFirst()).orElse(null);
                ValidatorUtils.checkNonNull(param2);
                
                handleTestJsonFormData(request, response, param, param2);
            })
            .exceptionally(throwable -> handleError(request, response, throwable));
    }
    /**
     * Handle GET /fake/jsonFormData : test json serialization of form data.
     * @param request the server request
     * @param response the server response
     * @param param field1 
     * @param param2 field2 
     */
    abstract void handleTestJsonFormData(ServerRequest request, ServerResponse response, String param, String param2);


    /**
     * PUT /fake/test-query-parameters.
     * @param request the server request
     * @param response the server response
     */
    void testQueryParameterCollectionFormat(ServerRequest request, ServerResponse response) { 
        Single.create(Single.empty())
            .thenAccept(val -> {
                List<String> pipe = Optional.ofNullable(request.queryParams().toMap().get("pipe")).orElse(null);
                ValidatorUtils.checkNonNull(pipe);
                List<String> ioutil = Optional.ofNullable(request.queryParams().toMap().get("ioutil")).orElse(null);
                ValidatorUtils.checkNonNull(ioutil);
                List<String> http = Optional.ofNullable(request.queryParams().toMap().get("http")).orElse(null);
                ValidatorUtils.checkNonNull(http);
                List<String> url = Optional.ofNullable(request.queryParams().toMap().get("url")).orElse(null);
                ValidatorUtils.checkNonNull(url);
                List<String> context = Optional.ofNullable(request.queryParams().toMap().get("context")).orElse(null);
                ValidatorUtils.checkNonNull(context);
                String allowEmpty = request.queryParams().toMap().getOrDefault("allowEmpty", List.of()).stream().findFirst().orElse(null);
                ValidatorUtils.checkNonNull(allowEmpty);
                String language = Optional.ofNullable(request.queryParams().toMap().get("language")).orElse(null);
                
                handleTestQueryParameterCollectionFormat(request, response, pipe, ioutil, http, url, context, allowEmpty, language);
            })
            .exceptionally(throwable -> handleError(request, response, throwable));
    }
    /**
     * Handle PUT /fake/test-query-parameters.
     * @param request the server request
     * @param response the server response
     * @param pipe pipe 
     * @param ioutil ioutil 
     * @param http http 
     * @param url url 
     * @param context context 
     * @param allowEmpty allowEmpty 
     * @param language language 
     */
    abstract void handleTestQueryParameterCollectionFormat(ServerRequest request, ServerResponse response, List<String> pipe, List<String> ioutil, List<String> http, List<String> url, List<String> context, String allowEmpty, String language);


    abstract Void handleError(ServerRequest request, ServerResponse response, Throwable throwable);
}
