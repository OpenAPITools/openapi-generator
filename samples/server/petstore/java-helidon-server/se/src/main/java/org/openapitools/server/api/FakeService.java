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

import io.helidon.webserver.Routing;
import io.helidon.webserver.ServerRequest;
import io.helidon.webserver.ServerResponse;
import io.helidon.webserver.Service;

public interface FakeService extends Service { 

    /**
     * A service registers itself by updating the routing rules.
     * @param rules the routing rules.
     */
    @Override
    default void update(Routing.Rules rules) {
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


    /**
     * GET /fake/health : Health check endpoint.
     * @param request the server request
     * @param response the server response
     */
    void fakeHealthGet(ServerRequest request, ServerResponse response);

    /**
     * GET /fake/http-signature-test : test http signature authentication.
     * @param request the server request
     * @param response the server response
     * @param pet Pet object that needs to be added to the store 
     */
    void fakeHttpSignatureTest(ServerRequest request, ServerResponse response, Pet pet);

    /**
     * POST /fake/outer/boolean.
     * @param request the server request
     * @param response the server response
     */
    void fakeOuterBooleanSerialize(ServerRequest request, ServerResponse response);

    /**
     * POST /fake/outer/composite.
     * @param request the server request
     * @param response the server response
     * @param outerComposite Input composite as post body 
     */
    void fakeOuterCompositeSerialize(ServerRequest request, ServerResponse response, OuterComposite outerComposite);

    /**
     * POST /fake/outer/number.
     * @param request the server request
     * @param response the server response
     */
    void fakeOuterNumberSerialize(ServerRequest request, ServerResponse response);

    /**
     * POST /fake/outer/string.
     * @param request the server request
     * @param response the server response
     */
    void fakeOuterStringSerialize(ServerRequest request, ServerResponse response);

    /**
     * POST /fake/property/enum-int.
     * @param request the server request
     * @param response the server response
     * @param outerObjectWithEnumProperty Input enum (int) as post body 
     */
    void fakePropertyEnumIntegerSerialize(ServerRequest request, ServerResponse response, OuterObjectWithEnumProperty outerObjectWithEnumProperty);

    /**
     * PUT /fake/body-with-binary.
     * @param request the server request
     * @param response the server response
     */
    void testBodyWithBinary(ServerRequest request, ServerResponse response);

    /**
     * PUT /fake/body-with-file-schema.
     * @param request the server request
     * @param response the server response
     * @param fileSchemaTestClass fileSchemaTestClass 
     */
    void testBodyWithFileSchema(ServerRequest request, ServerResponse response, FileSchemaTestClass fileSchemaTestClass);

    /**
     * PUT /fake/body-with-query-params.
     * @param request the server request
     * @param response the server response
     * @param user user 
     */
    void testBodyWithQueryParams(ServerRequest request, ServerResponse response, User user);

    /**
     * PATCH /fake : To test \&quot;client\&quot; model.
     * @param request the server request
     * @param response the server response
     * @param client client model 
     */
    void testClientModel(ServerRequest request, ServerResponse response, Client client);

    /**
     * POST /fake : Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 .
     * @param request the server request
     * @param response the server response
     */
    void testEndpointParameters(ServerRequest request, ServerResponse response);

    /**
     * GET /fake : To test enum parameters.
     * @param request the server request
     * @param response the server response
     */
    void testEnumParameters(ServerRequest request, ServerResponse response);

    /**
     * DELETE /fake : Fake endpoint to test group parameters (optional).
     * @param request the server request
     * @param response the server response
     */
    void testGroupParameters(ServerRequest request, ServerResponse response);

    /**
     * POST /fake/inline-additionalProperties : test inline additionalProperties.
     * @param request the server request
     * @param response the server response
     */
    void testInlineAdditionalProperties(ServerRequest request, ServerResponse response);

    /**
     * GET /fake/jsonFormData : test json serialization of form data.
     * @param request the server request
     * @param response the server response
     */
    void testJsonFormData(ServerRequest request, ServerResponse response);

    /**
     * PUT /fake/test-query-parameters.
     * @param request the server request
     * @param response the server response
     */
    void testQueryParameterCollectionFormat(ServerRequest request, ServerResponse response);

}
