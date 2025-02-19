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

import io.helidon.webserver.http.HttpRules;
import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;
import io.helidon.webserver.http.HttpService;

@io.helidon.common.Generated(value = "org.openapitools.codegen.languages.JavaHelidonServerCodegen",
                             trigger = "tag = 'Fake'",
                             version = "stable")
public interface FakeService extends HttpService {

    /**
     * A service registers itself by updating the routing rules.
     * @param rules the routing rules.
     */
    @Override
    default void routing(HttpRules rules) {
        rules.get("/BigDecimalMap", this::fakeBigDecimalMap);
        rules.get("/health", this::fakeHealthGet);
        rules.get("/http-signature-test", this::fakeHttpSignatureTest);
        rules.post("/outer/boolean", this::fakeOuterBooleanSerialize);
        rules.post("/outer/composite", this::fakeOuterCompositeSerialize);
        rules.post("/outer/number", this::fakeOuterNumberSerialize);
        rules.post("/outer/string", this::fakeOuterStringSerialize);
        rules.post("/property/enum-int", this::fakePropertyEnumIntegerSerialize);
        rules.post("/additionalProperties-reference", this::testAdditionalPropertiesReference);
        rules.put("/body-with-binary", this::testBodyWithBinary);
        rules.put("/body-with-file-schema", this::testBodyWithFileSchema);
        rules.put("/body-with-query-params", this::testBodyWithQueryParams);
        rules.patch("/", this::testClientModel);
        rules.post("/", this::testEndpointParameters);
        rules.get("/", this::testEnumParameters);
        rules.delete("/", this::testGroupParameters);
        rules.post("/inline-additionalProperties", this::testInlineAdditionalProperties);
        rules.post("/inline-freeform-additionalProperties", this::testInlineFreeformAdditionalProperties);
        rules.get("/jsonFormData", this::testJsonFormData);
        rules.post("/nullable", this::testNullable);
        rules.put("/test-query-parameters", this::testQueryParameterCollectionFormat);
        rules.post("/stringMap-reference", this::testStringMapReference);
    }


    /**
     * GET /fake/BigDecimalMap.
     *
     * @param request the server request
     * @param response the server response
     */
    void fakeBigDecimalMap(ServerRequest request, ServerResponse response);
    /**
     * GET /fake/health : Health check endpoint.
     *
     * @param request the server request
     * @param response the server response
     */
    void fakeHealthGet(ServerRequest request, ServerResponse response);
    /**
     * GET /fake/http-signature-test : test http signature authentication.
     *
     * @param request the server request
     * @param response the server response
     */
    void fakeHttpSignatureTest(ServerRequest request, ServerResponse response);
    /**
     * POST /fake/outer/boolean.
     *
     * @param request the server request
     * @param response the server response
     */
    void fakeOuterBooleanSerialize(ServerRequest request, ServerResponse response);
    /**
     * POST /fake/outer/composite.
     *
     * @param request the server request
     * @param response the server response
     */
    void fakeOuterCompositeSerialize(ServerRequest request, ServerResponse response);
    /**
     * POST /fake/outer/number.
     *
     * @param request the server request
     * @param response the server response
     */
    void fakeOuterNumberSerialize(ServerRequest request, ServerResponse response);
    /**
     * POST /fake/outer/string.
     *
     * @param request the server request
     * @param response the server response
     */
    void fakeOuterStringSerialize(ServerRequest request, ServerResponse response);
    /**
     * POST /fake/property/enum-int.
     *
     * @param request the server request
     * @param response the server response
     */
    void fakePropertyEnumIntegerSerialize(ServerRequest request, ServerResponse response);
    /**
     * POST /fake/additionalProperties-reference : test referenced additionalProperties.
     *
     * @param request the server request
     * @param response the server response
     */
    void testAdditionalPropertiesReference(ServerRequest request, ServerResponse response);
    /**
     * PUT /fake/body-with-binary.
     *
     * @param request the server request
     * @param response the server response
     */
    void testBodyWithBinary(ServerRequest request, ServerResponse response);
    /**
     * PUT /fake/body-with-file-schema.
     *
     * @param request the server request
     * @param response the server response
     */
    void testBodyWithFileSchema(ServerRequest request, ServerResponse response);
    /**
     * PUT /fake/body-with-query-params.
     *
     * @param request the server request
     * @param response the server response
     */
    void testBodyWithQueryParams(ServerRequest request, ServerResponse response);
    /**
     * PATCH /fake : To test \&quot;client\&quot; model.
     *
     * @param request the server request
     * @param response the server response
     */
    void testClientModel(ServerRequest request, ServerResponse response);
    /**
     * POST /fake : Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 .
     *
     * @param request the server request
     * @param response the server response
     */
    void testEndpointParameters(ServerRequest request, ServerResponse response);
    /**
     * GET /fake : To test enum parameters.
     *
     * @param request the server request
     * @param response the server response
     */
    void testEnumParameters(ServerRequest request, ServerResponse response);
    /**
     * DELETE /fake : Fake endpoint to test group parameters (optional).
     *
     * @param request the server request
     * @param response the server response
     */
    void testGroupParameters(ServerRequest request, ServerResponse response);
    /**
     * POST /fake/inline-additionalProperties : test inline additionalProperties.
     *
     * @param request the server request
     * @param response the server response
     */
    void testInlineAdditionalProperties(ServerRequest request, ServerResponse response);
    /**
     * POST /fake/inline-freeform-additionalProperties : test inline free-form additionalProperties.
     *
     * @param request the server request
     * @param response the server response
     */
    void testInlineFreeformAdditionalProperties(ServerRequest request, ServerResponse response);
    /**
     * GET /fake/jsonFormData : test json serialization of form data.
     *
     * @param request the server request
     * @param response the server response
     */
    void testJsonFormData(ServerRequest request, ServerResponse response);
    /**
     * POST /fake/nullable : test nullable parent property.
     *
     * @param request the server request
     * @param response the server response
     */
    void testNullable(ServerRequest request, ServerResponse response);
    /**
     * PUT /fake/test-query-parameters.
     *
     * @param request the server request
     * @param response the server response
     */
    void testQueryParameterCollectionFormat(ServerRequest request, ServerResponse response);
    /**
     * POST /fake/stringMap-reference : test referenced string map.
     *
     * @param request the server request
     * @param response the server response
     */
    void testStringMapReference(ServerRequest request, ServerResponse response);
}
