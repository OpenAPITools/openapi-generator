/*
 * Copyright 2026 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.cppboostbeastserver;

import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

public class CppBoostBeastServerCodegenTest {

    private static final String SERVER_REGRESSION_SPEC =
            "src/test/resources/3_1/cpp-boost-beast-server/server-regression.yaml";

    private static Path generate(String spec, java.util.Map<String, Object> properties)
            throws IOException {
        Path outputRoot = Files.createDirectories(Path.of("target"));
        Path output = Files.createTempDirectory(outputRoot, "cpp-boost-beast-server-test-");
        output.toFile().deleteOnExit();
        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-server")
                .setInputSpec(spec)
                .setOutputDir(output.toString());
        properties.forEach(configurator::addAdditionalProperty);
        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        return output;
    }

    private static Path writeTempSpec(String... lines) throws IOException {
        Path spec = Files.createTempFile("cpp-boost-beast-server-spec-", ".yaml");
        spec.toFile().deleteOnExit();
        Files.writeString(spec, String.join("\n", lines) + "\n");
        return spec.toAbsolutePath();
    }

    private static final String[] HEADER = {
            "openapi: 3.1.0", "info: {title: t, version: '1'}", "paths:"};

    private static String[] spec(String... body) {
        String[] all = new String[body.length + HEADER.length];
        System.arraycopy(HEADER, 0, all, 0, HEADER.length);
        System.arraycopy(body, 0, all, HEADER.length, body.length);
        return all;
    }

    @Test
    public void defaultsAreConfigured() {
        org.openapitools.codegen.languages.CppBoostBeastServerCodegen codegen =
                new org.openapitools.codegen.languages.CppBoostBeastServerCodegen();
        Assert.assertEquals(codegen.getName(), "cpp-boost-beast-server");
        Assert.assertEquals(codegen.getTag(), CodegenType.SERVER);
        Assert.assertEquals(codegen.modelPackage(), "org.openapitools.server.model");
        Assert.assertEquals(codegen.apiPackage(), "org.openapitools.server.api");
        Assert.assertTrue(codegen.getOutputDir().contains("cpp-boost-beast-server"));
        List<String> destinations = codegen.supportingFiles().stream()
                .map(file -> file.getDestinationFilename())
                .sorted()
                .collect(java.util.stream.Collectors.toList());
        Assert.assertTrue(destinations.contains("HttpServer.h"),
                "runtime HttpServer.h must be a supporting file");
        Assert.assertTrue(destinations.contains("BodyJson.h"),
                "runtime BodyJson.h must be a supporting file");
        Assert.assertTrue(destinations.contains("Oas31Validator.h"),
                "shared validation header must be a supporting file");
        Assert.assertTrue(destinations.contains("schema_ir.generated.cpp"),
                "schema IR source must be a supporting file");
        Assert.assertFalse(destinations.contains("main.cpp"),
                "main.cpp must not be generated without addApiImplStubs");
    }

    @Test
    public void generatesFullContractFromRegressionSpec() throws IOException {
        Path output = generate(SERVER_REGRESSION_SPEC, java.util.Map.of());

        String apiHeader = Files.readString(output.resolve("api/DefaultApi.h"));
        Assert.assertTrue(apiHeader.contains("virtual void getPetById("),
                "service interface must declare getPetById");
        Assert.assertTrue(apiHeader.contains("struct GetPetByIdRequest"),
                "request struct must be named from the operationId");
        Assert.assertTrue(apiHeader.contains("void send200(Pet value) const"),
                "responder must expose send200 for the Pet response");
        Assert.assertTrue(apiHeader.contains("void send204() const"),
                "responder must expose the empty 204");
        Assert.assertTrue(apiHeader.contains(
                        "void sendDefault(ErrorResponse value, unsigned status) const"),
                "responder must expose the default ErrorResponse sender");

        String apiSource = Files.readString(output.resolve("api/DefaultApi.cpp"));
        Assert.assertTrue(apiSource.contains("router->add("),
                "route registration must be emitted");
        Assert.assertTrue(apiSource.contains("splitOn(values.first->second, '|')"),
                "pipe-delimited query collection must split on '|'");
        Assert.assertTrue(apiSource.contains("std::regex"),
                "pattern constraints must emit a regex check");
        Assert.assertTrue(apiSource.contains("kAllowed"),
                "enum constraints must emit an allow-list check");

        Assert.assertTrue(Files.exists(output.resolve("server/HttpServer.h")));
        Assert.assertTrue(Files.exists(output.resolve("server/Router.h")));
        Assert.assertTrue(Files.exists(output.resolve("server/Responder.h")));
        Assert.assertTrue(Files.exists(output.resolve("server/Problem.h")));
        Assert.assertTrue(Files.exists(output.resolve("server/ParamCodecs.h")));
        Assert.assertTrue(Files.exists(output.resolve("server/BodyJson.h")));
        Assert.assertTrue(Files.exists(output.resolve("model/Pet.h")),
                "models must be generated");
    }

    @Test
    public void addApiImplStubsEmitsMainAndStubs() throws IOException {
        Path output = generate(SERVER_REGRESSION_SPEC,
                java.util.Map.of("addApiImplStubs", Boolean.TRUE));
        Assert.assertTrue(Files.exists(output.resolve("main.cpp")),
                "addApiImplStubs must generate main.cpp");
        String apiHeader = Files.readString(output.resolve("api/DefaultApi.h"));
        Assert.assertTrue(apiHeader.contains("class DefaultApiStub : public DefaultApi"),
                "addApiImplStubs must generate a stub service");
        String main = Files.readString(output.resolve("main.cpp"));
        Assert.assertTrue(
                main.contains("DefaultApi::attach(server, std::make_shared<DefaultApiStub>())"),
                "main.cpp must attach the stub service");
    }

    @Test
    public void compileWithValidationFalseStripsSchemaIr() throws IOException {
        Path output = generate(SERVER_REGRESSION_SPEC,
                java.util.Map.of("compileWithValidation", Boolean.FALSE));
        Assert.assertFalse(Files.exists(output.resolve("model/schema_ir.generated.cpp")),
                "IR source must be stripped when validation is disabled");
        Assert.assertFalse(Files.exists(output.resolve("model/Oas31SchemaRegistry.h")),
                "IR registry must be stripped when validation is disabled");
        String cmake = Files.readString(output.resolve("CMakeLists.txt"));
        Assert.assertFalse(cmake.contains("schema_ir.generated"),
                "CMake must not reference the stripped IR");
        Assert.assertTrue(Files.exists(output.resolve("model/Oas31Validator.h")),
                "header-only validator must remain");
    }

    @Test
    public void degradesMultipartOnlyBodyToNoTypedBody() throws IOException {
        Path output = generate(writeTempSpec(spec(
                "  /upload:",
                "    post:",
                "      operationId: upload",
                "      requestBody:",
                "        content:",
                "          multipart/form-data:",
                "            schema:",
                "              type: object",
                "      responses:",
                "        '200': {description: ok}")).toString(),
                java.util.Map.of());
        String apiHeader = Files.readString(output.resolve("api/DefaultApi.h"));
        Assert.assertTrue(apiHeader.contains("struct UploadRequest"),
                "operation must still generate its request contract");
        Assert.assertFalse(apiHeader.contains("body{};"),
                "a multipart-only body must degrade to no typed body field");
    }

    @Test
    public void degradesFormUrlencodedBodyToNoTypedBody() throws IOException {
        Path output = generate(writeTempSpec(spec(
                "  /form:",
                "    post:",
                "      operationId: submit",
                "      requestBody:",
                "        content:",
                "          application/x-www-form-urlencoded:",
                "            schema:",
                "              type: object",
                "      responses:",
                "        '200': {description: ok}")).toString(),
                java.util.Map.of());
        String apiHeader = Files.readString(output.resolve("api/DefaultApi.h"));
        Assert.assertTrue(apiHeader.contains("struct SubmitRequest"),
                "operation must still generate its request contract");
        Assert.assertFalse(apiHeader.contains("body{};"),
                "a form-urlencoded-only body must degrade to no typed body");
    }

    @Test
    public void keepsJsonMemberAndDropsXmlMemberFromMixedBody() throws IOException {
        Path output = generate(writeTempSpec(spec(
                "  /mixed:",
                "    post:",
                "      operationId: mixed",
                "      requestBody:",
                "        required: true",
                "        content:",
                "          application/json:",
                "            schema: {type: object}",
                "          application/xml:",
                "            schema: {type: object}",
                "      responses:",
                "        '200': {description: ok}")).toString(),
                java.util.Map.of());
        String apiSource = Files.readString(output.resolve("api/DefaultApi.cpp"));
        Assert.assertTrue(
                apiSource.contains("\"application/json\" };"),
                "declared media types must be filtered to the JSON member");
        Assert.assertFalse(apiSource.contains("application/xml"),
                "the XML member must not appear in the accepted list");
    }

    @Test
    public void textPlainResponseDegradesToJsonSerialization() throws IOException {
        Path output = generate(writeTempSpec(spec(
                "  /text:",
                "    get:",
                "      operationId: getText",
                "      responses:",
                "        '200':",
                "          description: ok",
                "          content:",
                "            text/plain:",
                "              schema: {type: string}")).toString(),
                java.util.Map.of());
        String apiHeader = Files.readString(output.resolve("api/DefaultApi.h"));
        Assert.assertTrue(apiHeader.contains("void send200(std::string value) const"),
                "response model must serialize as JSON regardless of "
                        + "the declared text media type");
    }

    @Test
    public void eventStreamResponseDegradesToJsonSerialization() throws IOException {
        Path output = generate(writeTempSpec(spec(
                "  /stream:",
                "    get:",
                "      operationId: stream",
                "      responses:",
                "        '200':",
                "          description: events",
                "          content:",
                "            text/event-stream:",
                "              schema: {type: string}")).toString(),
                java.util.Map.of());
        String apiHeader = Files.readString(output.resolve("api/DefaultApi.h"));
        Assert.assertTrue(apiHeader.contains("void send200(std::string value) const"),
                "SSE declaration must degrade to a plain JSON sender");
    }
    @Test
    public void degradesContentStyleParameterToDroppedField() throws IOException {
        Path output = generate(writeTempSpec(spec(
                "  /p:",
                "    get:",
                "      operationId: op",
                "      parameters:",
                "        - name: token",
                "          in: query",
                "          content:",
                "            application/json:",
                "              schema: {type: string}",
                "      responses:",
                "        '200': {description: ok}")).toString(),
                java.util.Map.of());
        String apiHeader = Files.readString(output.resolve("api/DefaultApi.h"));
        Assert.assertTrue(apiHeader.contains("struct OpRequest"),
                "operation must still generate its request contract");
        Assert.assertFalse(apiHeader.contains("token{}"),
                "a content-style parameter must be dropped from the handler");
    }

    @Test
    public void degradesCookieMatrixStyleParameter() throws IOException {
        Path output = generate(writeTempSpec(spec(
                "  /c:",
                "    get:",
                "      operationId: op",
                "      parameters:",
                "        - name: session",
                "          in: cookie",
                "          style: matrix",
                "          schema: {type: string}",
                "      responses:",
                "        '200': {description: ok}")).toString(),
                java.util.Map.of());
        String apiHeader = Files.readString(output.resolve("api/DefaultApi.h"));
        Assert.assertTrue(apiHeader.contains("struct OpRequest"),
                "operation must still generate its request contract");
        Assert.assertFalse(apiHeader.contains("session{}"),
                "a cookie parameter with an unsupported style must be dropped");
    }

    @Test
    public void degradesCookieArrayParameter() throws IOException {
        Path output = generate(writeTempSpec(spec(
                "  /c:",
                "    get:",
                "      operationId: op",
                "      parameters:",
                "        - name: session",
                "          in: cookie",
                "          schema: {type: array, items: {type: string}}",
                "      responses:",
                "        '200': {description: ok}")).toString(),
                java.util.Map.of());
        String apiHeader = Files.readString(output.resolve("api/DefaultApi.h"));
        Assert.assertTrue(apiHeader.contains("struct OpRequest"),
                "operation must still generate its request contract");
        Assert.assertFalse(apiHeader.contains("session{}"),
                "an array cookie parameter must be dropped; the codec is scalar-only");
    }

    @Test
    public void degradesObjectQueryParameterWithoutDeepObject() throws IOException {
        Path output = generate(writeTempSpec(spec(
                "  /q:",
                "    get:",
                "      operationId: op",
                "      parameters:",
                "        - name: filter",
                "          in: query",
                "          schema: {type: object, additionalProperties: {type: string}}",
                "      responses:",
                "        '200': {description: ok}")).toString(),
                java.util.Map.of());
        String apiHeader = Files.readString(output.resolve("api/DefaultApi.h"));
        Assert.assertTrue(apiHeader.contains("struct OpRequest"),
                "operation must still generate its request contract");
        Assert.assertFalse(apiHeader.contains("filter{}"),
                "an object query parameter outside deepObject must be dropped");
    }

    @Test
    public void acceptsDeepObjectStringMapQueryParameter() throws IOException {
        Path output = generate(writeTempSpec(spec(
                "  /q:",
                "    get:",
                "      operationId: op",
                "      parameters:",
                "        - name: filter",
                "          in: query",
                "          style: deepObject",
                "          schema: {type: object, additionalProperties: {type: string}}",
                "      responses:",
                "        '200': {description: ok}")).toString(), java.util.Map.of());
        String apiHeader = Files.readString(output.resolve("api/DefaultApi.h"));
        Assert.assertTrue(
                apiHeader.contains("std::map<std::string, std::string> filter{}"),
                "deepObject string map must generate as a std::map field");
    }

    @Test
    public void degradesNonScalarArrayParameterItems() throws IOException {
        Path output = generate(writeTempSpec(spec(
                "  /q:",
                "    get:",
                "      operationId: op",
                "      parameters:",
                "        - name: things",
                "          in: query",
                "          schema: {type: array, items: {type: object}}",
                "      responses:",
                "        '200': {description: ok}")).toString(),
                java.util.Map.of());
        String apiHeader = Files.readString(output.resolve("api/DefaultApi.h"));
        Assert.assertTrue(apiHeader.contains("struct OpRequest"),
                "operation must still generate its request contract");
        Assert.assertFalse(apiHeader.contains("things{}"),
                "an array parameter with non-scalar items must be dropped");
    }

    @Test
    public void degradesHeterogeneousParameterEnum() throws IOException {
        Path output = generate(writeTempSpec(spec(
                "  /q:",
                "    get:",
                "      operationId: op",
                "      parameters:",
                "        - name: mixed",
                "          in: query",
                "          schema: {type: string, enum: [alpha, 1]}",
                "      responses:",
                "        '200': {description: ok}")).toString(),
                java.util.Map.of());
        String apiHeader = Files.readString(output.resolve("api/DefaultApi.h"));
        Assert.assertTrue(apiHeader.contains("struct OpRequest"),
                "operation must still generate its request contract");
        Assert.assertFalse(apiHeader.contains("mixed{}"),
                "a parameter with a mixed string/numeric enum must be dropped");
    }

    @Test
    public void rejectsAmbiguousRouteShapes() throws IOException {
        Path spec = writeTempSpec(spec(
                "  /a/{x}/c:",
                "    get:",
                "      operationId: first",
                "      responses:",
                "        '200': {description: ok}",
                "  /a/{y}/c:",
                "    get:",
                "      operationId: second",
                "      responses:",
                "        '200': {description: ok}"));
        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-server")
                .setInputSpec(spec.toString())
                .setOutputDir(Files.createTempDirectory("routes-").toString())
                .setValidateSpec(false);
        IllegalArgumentException error = Assert.expectThrows(
                IllegalArgumentException.class,
                () -> new DefaultGenerator()
                        .opts(configurator.toClientOptInput()).generate());
        Assert.assertTrue(error.getMessage().contains("/a/{x}/c"),
                "diagnostic must list the first template");
        Assert.assertTrue(error.getMessage().contains("/a/{y}/c"),
                "diagnostic must list the second template");
    }

    @Test
    public void rejectsQuerySuffixRouteAsSameShape() throws IOException {
        // Router::splitPath strips the query string at REGISTRATION time, so
        // '/responses?beta=true' registers the very same route as
        // '/responses' — a literal duplicate, not a ranking puzzle. The
        // shape gate must say exactly that (the same wording as two
        // same-shape templates), instead of letting the pair fall through to
        // the witness probe and mislabelling it as registration-order
        // ambiguity. This is the OpenAI document's beta-path idiom.
        Path spec = writeTempSpec(spec(
                "  /responses:",
                "    post:",
                "      operationId: createResponse",
                "      responses:",
                "        '200': {description: ok}",
                "  /responses?beta=true:",
                "    post:",
                "      operationId: betaCreateResponse",
                "      responses:",
                "        '200': {description: ok}"));
        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-server")
                .setInputSpec(spec.toString())
                .setOutputDir(Files.createTempDirectory("query-dup-").toString())
                .setValidateSpec(false);
        IllegalArgumentException error = Assert.expectThrows(
                IllegalArgumentException.class,
                () -> new DefaultGenerator()
                        .opts(configurator.toClientOptInput()).generate());
        Assert.assertTrue(error.getMessage().contains("have the same shape"),
                "a ?-suffixed duplicate must be reported as the same shape, got: "
                        + error.getMessage());
    }

    @Test
    public void rejectsRangedResponseCodes() throws IOException {
        Path spec = writeTempSpec(spec(
                "  /r:",
                "    get:",
                "      operationId: ranged",
                "      responses:",
                "        '2XX':",
                "          description: any success",
                "          content:",
                "            application/json:",
                "              schema: {type: string}"));
        IllegalArgumentException error = Assert.expectThrows(
                IllegalArgumentException.class,
                () -> generate(spec.toString(), java.util.Map.of()));
        Assert.assertTrue(error.getMessage().contains("2XX"),
                "diagnostic must name the ranged code");
    }

    @Test
    public void unsupportedSecuritySchemeDegradesToRuntimeDenial() throws IOException {
        Path output = generate(writeTempSpec(
                "openapi: 3.1.0",
                "info: {title: t, version: '1'}",
                "security:",
                "  - oauth: []",
                "paths:",
                "  /o:",
                "    get:",
                "      operationId: needsOauth",
                "      responses:",
                "        '200': {description: ok}",
                "components:",
                "  securitySchemes:",
                "    oauth:",
                "      type: oauth2",
                "      flows: {}").toString(), java.util.Map.of());
        String apiSource = Files.readString(output.resolve("api/DefaultApi.cpp"));
        // The requirement survives into the route table with its declared
        // type; the runtime has no credential extractor for oauth2, so
        // structurallySatisfied() denies every request (401) rather than
        // silently allowing it.
        Assert.assertTrue(apiSource.contains("\"oauth\", \"oauth2\""),
                "the oauth requirement must stay in the route table as type oauth2");
    }

    @Test
    public void generatesFromCanonicalPetstore() throws IOException {
        // The repository harness (AllGeneratorsTest) requires every
        // registered generator to accept src/test/resources/3_0/petstore.yaml,
        // which mixes XML/form/multipart payloads with an oauth2 scheme.
        // This pins the degrade contract for the canonical spec.
        Path output = generate("src/test/resources/3_0/petstore.yaml",
                java.util.Map.of());
        Assert.assertTrue(Files.exists(output.resolve("api/PetApi.cpp"))
                || Files.exists(output.resolve("api/PetsApi.cpp")),
                "the canonical petstore must generate API sources");
    }

    @Test
    public void normalizesMediaTypeParametersInFacts() throws IOException {
        Path spec = writeTempSpec(spec(
                "  /m:",
                "    post:",
                "      operationId: mediaParams",
                "      requestBody:",
                "        content:",
                "          'application/json; charset=utf-8':",
                "            schema: {type: string}",
                "      responses:",
                "        '200': {description: ok}"));
        Path output = generate(spec.toString(), java.util.Map.of());
        String apiSource = Files.readString(output.resolve("api/DefaultApi.cpp"));
        Assert.assertTrue(apiSource.contains("\"application/json\""),
                "declared media-type parameters must be stripped in kMediaTypes");
        Assert.assertFalse(apiSource.contains("charset"),
                "charset must not survive into the generated match list");
    }

    @Test
    public void resolvesRefRequestBodiesAndParameters() throws IOException {
        Path spec = writeTempSpec(
                "openapi: 3.1.0",
                "info: {title: t, version: '1'}",
                "paths:",
                "  /ref:",
                "    post:",
                "      operationId: refBody",
                "      requestBody:",
                "        $ref: '#/components/requestBodies/PetBody'",
                "      parameters:",
                "        - $ref: '#/components/parameters/Tier'",
                "      responses:",
                "        '200': {description: ok}",
                "components:",
                "  requestBodies:",
                "    PetBody:",
                "      content:",
                "        application/json:",
                "          schema: {type: string}",
                "  parameters:",
                "    Tier:",
                "      name: tier",
                "      in: query",
                "      schema:",
                "        type: integer",
                "        enum: [10, 20]");
        Path output = generate(spec.toString(), java.util.Map.of());
        String apiHeader = Files.readString(output.resolve("api/DefaultApi.h"));
        Assert.assertTrue(apiHeader.contains("RefBodyRequest"),
                "request struct must exist for the $ref-body operation");
        String apiSource = Files.readString(output.resolve("api/DefaultApi.cpp"));
        Assert.assertTrue(apiSource.contains("fromJsonBody"),
                "$ref request body must still be decoded");
        Assert.assertTrue(apiSource.contains("kAllowed"),
                "$ref parameter constraints must survive");
    }

    @Test
    public void generatesFromOas30Spec() throws IOException {
        // The shared pipeline must keep 3.0 documents working (JSON-only).
        Path spec = writeTempSpec(
                "openapi: 3.0.3",
                "info: {title: t, version: '1'}",
                "paths:",
                "  /pets/{petId}:",
                "    get:",
                "      operationId: getPet",
                "      parameters:",
                "        - name: petId",
                "          in: path",
                "          required: true",
                "          schema: {type: integer, format: int64}",
                "      responses:",
                "        '200':",
                "          description: ok",
                "          content:",
                "            application/json:",
                "              schema: {type: string}");
        Path output = generate(spec.toString(), java.util.Map.of());
        Assert.assertTrue(Files.exists(output.resolve("api/DefaultApi.h")),
                "3.0 spec must generate the API");
        Assert.assertTrue(Files.exists(output.resolve("model/ValidationTypes.h")),
                "3.0 spec must generate the shared validation runtime");
    }

    @Test
    public void untypedFreeFormSchemasResolveWithoutBaseSentinel() throws IOException {
        // DefaultCodegen seeds AnyType -> oas_any_type_not_mapped, a header
        // placeholder this generator family never provides. The OpenAI
        // corpus (FunctionToolParam_output_schema) reaches it through an
        // anyOf of a freeform object and null; the server must resolve the
        // branch to boost::json::value exactly like the client, which wipes
        // the inherited map.
        Path output = generate(writeTempSpec(spec(
                "  /hold:",
                "    post:",
                "      operationId: hold",
                "      requestBody:",
                "        required: true",
                "        content:",
                "          application/json:",
                "            schema: { $ref: '#/components/schemas/Holder' }",
                "      responses:",
                "        '200': {description: ok}",
                "components:",
                "  schemas:",
                "    Holder:",
                "      type: object",
                "      properties:",
                "        output_schema:",
                "          anyOf:",
                "            - additionalProperties: {}",
                "              type: object",
                "            - type: \"null\"")).toString(),
                java.util.Map.of());
        String wrapper = Files.readString(
                output.resolve("model/Holder_output_schema.h"));
        Assert.assertFalse(wrapper.contains("oas_any_type_not_mapped"),
                "the base AnyType placeholder must never reach generated sources");
        Assert.assertTrue(wrapper.contains("boost::json::value"),
                "the freeform branch must resolve to boost::json::value");
    }

    @Test
    public void qualifiesBodyModelCollidingWithRequestStruct() throws IOException {
        // operationId 'echo' yields struct EchoRequest; a body model of the
        // same name would be re-declared inside it (injected-class-name), so
        // the field type must be namespace-qualified.
        Path output = generate(writeTempSpec(spec(
                "  /echo:",
                "    post:",
                "      operationId: echo",
                "      requestBody:",
                "        required: true",
                "        content:",
                "          application/json:",
                "            schema: { $ref: '#/components/schemas/EchoRequest' }",
                "      responses:",
                "        '200': {description: ok}",
                "components:",
                "  schemas:",
                "    EchoRequest:",
                "      type: object",
                "      properties:",
                "        text: {type: string}")).toString(), java.util.Map.of());
        String apiHeader = Files.readString(output.resolve("api/DefaultApi.h"));
        Assert.assertTrue(apiHeader.contains(
                        "org.openapitools.server.model::EchoRequest body{};")
                        || apiHeader.contains(
                        "org::openapitools::server::model::EchoRequest body{};"),
                "a colliding body model must be qualified, not shadowed");
    }

    @Test
    public void recoversTypedBodyAndIncludeForMixedJsonMember() throws IOException {
        // multipart + JSON: DefaultCodegen flattens the form fields and never
        // imports the JSON member's model. The assembler must both type the
        // body from the recovered $ref and append its #include.
        Path output = generate(writeTempSpec(spec(
                "  /edit:",
                "    post:",
                "      operationId: edit",
                "      requestBody:",
                "        required: true",
                "        content:",
                "          multipart/form-data:",
                "            schema:",
                "              type: object",
                "              properties:",
                "                image: {type: string, format: binary}",
                "          application/json:",
                "            schema: { $ref: '#/components/schemas/EditBody' }",
                "      responses:",
                "        '200': {description: ok}",
                "components:",
                "  schemas:",
                "    EditBody:",
                "      type: object",
                "      properties:",
                "        prompt: {type: string}")).toString(), java.util.Map.of());
        String apiHeader = Files.readString(output.resolve("api/DefaultApi.h"));
        Assert.assertTrue(apiHeader.contains("#include \"EditBody.h\""),
                "the recovered body model's header must be included");
        Assert.assertTrue(apiHeader.contains("EditBody body{};"),
                "the JSON member's model must type the body field");
        Assert.assertFalse(apiHeader.contains("image{}"),
                "form-field parameters must not appear as handler fields");
    }

    @Test
    public void degradesBodyModelAliasedToVariant() throws IOException {
        // A named oneOf component is a model, so it passes the model-name
        // check; but its C++ type is std::variant, which fromJsonLeaf cannot
        // decode. The handler must receive no typed body.
        Path output = generate(writeTempSpec(spec(
                "  /union:",
                "    post:",
                "      operationId: sendUnion",
                "      requestBody:",
                "        required: true",
                "        content:",
                "          application/json:",
                "            schema: { $ref: '#/components/schemas/UnionBody' }",
                "      responses:",
                "        '200': {description: ok}",
                "components:",
                "  schemas:",
                "    UnionBody:",
                "      oneOf:",
                "        - { $ref: '#/components/schemas/Alpha' }",
                "        - { $ref: '#/components/schemas/Beta' }",
                "    Alpha:",
                "      type: object",
                "      properties: {a: {type: string}}",
                "    Beta:",
                "      type: object",
                "      properties: {b: {type: string}}")).toString(),
                java.util.Map.of());
        String apiHeader = Files.readString(output.resolve("api/DefaultApi.h"));
        Assert.assertTrue(apiHeader.contains("struct SendUnionRequest"),
                "operation must still generate its request contract");
        Assert.assertFalse(apiHeader.contains("body{}"),
                "a union body must degrade to no typed body field");
    }

    @Test
    public void degradesEnumClassTypedQueryParameter() throws IOException {
        // A $ref'd string-enum component gives the parameter the enum class
        // as dataType; parseScalar has no overload for it, so the parameter
        // degrades rather than failing to compile.
        Path output = generate(writeTempSpec(spec(
                "  /colored:",
                "    get:",
                "      operationId: colored",
                "      parameters:",
                "        - name: color",
                "          in: query",
                "          schema: { $ref: '#/components/schemas/Color' }",
                "      responses:",
                "        '200': {description: ok}",
                "components:",
                "  schemas:",
                "    Color:",
                "      type: string",
                "      enum: [red, green, blue]")).toString(), java.util.Map.of());
        String apiHeader = Files.readString(output.resolve("api/DefaultApi.h"));
        Assert.assertTrue(apiHeader.contains("struct ColoredRequest"),
                "operation must still generate its request contract");
        Assert.assertFalse(apiHeader.contains("color{}"),
                "an enum-class-typed parameter must be dropped");
    }

    @Test
    public void typesJsonBodyWhenXmlDeclaredFirst() throws IOException {
        // DefaultCodegen types the body parameter from the FIRST content
        // entry. When that entry is a media type the runtime cannot parse
        // (XML here) but a JSON member names a model, the handler must be
        // typed from the JSON representation, not the dropped one.
        Path output = generate(writeTempSpec(spec(
                "  /switch:",
                "    post:",
                "      operationId: switch",
                "      requestBody:",
                "        required: true",
                "        content:",
                "          application/xml:",
                "            schema: { $ref: '#/components/schemas/XmlBody' }",
                "          application/json:",
                "            schema: { $ref: '#/components/schemas/JsonBody' }",
                "      responses:",
                "        '200': {description: ok}",
                "components:",
                "  schemas:",
                "    XmlBody:",
                "      type: object",
                "      properties: {xml: {type: string}}",
                "    JsonBody:",
                "      type: object",
                "      properties: {json: {type: string}}")).toString(),
                java.util.Map.of());
        String apiHeader = Files.readString(output.resolve("api/DefaultApi.h"));
        Assert.assertTrue(apiHeader.contains("JsonBody body{};"),
                "the JSON member's model must type the body when XML came first");
        Assert.assertFalse(apiHeader.contains("XmlBody body{};"),
                "the dropped XML representation must not type the body field");
    }

    @Test
    public void qualifiesReadmeSendTypeForModelResponses() throws IOException {
        // The README quick-start class lives outside the generated api
        // namespace, where the header's `using namespace <model>;` is not in
        // effect, so model response types must carry the `model::` alias.
        Path output = generate(SERVER_REGRESSION_SPEC, java.util.Map.of());
        String readme = Files.readString(output.resolve("README.md"));
        // Operations render alphabetically; the first one (codec) responds
        // with the Report model, which must carry the `model::` alias.
        Assert.assertTrue(readme.contains("model::Report value{};"),
                "the quick-start must qualify the model response type");
    }

    @Test
    public void servesTaggedVariantResponseTyped() throws IOException {
        // A oneOf whose branches share a C++ type is a std::variant of tagged
        // CompositionBranchValue members. Responses serialize by visiting the
        // active branch (bodyLeaf unwrap), so the sender stays typed.
        Path output = generate(SERVER_REGRESSION_SPEC, java.util.Map.of());
        String apiHeader = Files.readString(output.resolve("api/DefaultApi.h"));
        Assert.assertTrue(apiHeader.contains("void send200(Pick value) const"),
                "the variant response must generate a typed sender");
    }

    @Test
    public void rejectsCrossLayoutRouteOverlapWithWitness() throws IOException {
        // /a/{x}b and /a/a{y} have different shape keys but both match /a/ab
        // with equal ranking; the witness probe must prove the collision.
        Path spec = writeTempSpec(spec(
                "  /a/{x}b:",
                "    get:",
                "      operationId: first",
                "      responses:",
                "        '200': {description: ok}",
                "  /a/a{y}:",
                "    get:",
                "      operationId: second",
                "      responses:",
                "        '200': {description: ok}"));
        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-server")
                .setInputSpec(spec.toString())
                .setOutputDir(Files.createTempDirectory("overlap-").toString())
                .setValidateSpec(false);
        IllegalArgumentException error = Assert.expectThrows(
                IllegalArgumentException.class,
                () -> new DefaultGenerator()
                        .opts(configurator.toClientOptInput()).generate());
        Assert.assertTrue(error.getMessage().contains("/a/ab"),
                "diagnostic must name the ambiguous witness path, got: "
                        + error.getMessage());
    }

    @Test
    public void rejectsAdjacentPathExpressions() throws IOException {
        // The router cannot split a capture boundary with no literal between
        // expressions, so /a/{first}{second} must be rejected up front.
        Path spec = writeTempSpec(spec(
                "  /a/{first}{second}:",
                "    get:",
                "      operationId: adjacent",
                "      responses:",
                "        '200': {description: ok}"));
        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-server")
                .setInputSpec(spec.toString())
                .setOutputDir(Files.createTempDirectory("adjacent-").toString())
                .setValidateSpec(false);
        IllegalArgumentException error = Assert.expectThrows(
                IllegalArgumentException.class,
                () -> new DefaultGenerator()
                        .opts(configurator.toClientOptInput()).generate());
        Assert.assertTrue(error.getMessage().contains("adjacent expressions"),
                "diagnostic must name the adjacency defect, got: "
                        + error.getMessage());
    }

    @Test
    public void sanitizesCommentTerminatorsInInfoFields() throws IOException {
        // A title containing */ would close the generated block comment and
        // turn the remaining text into code in every generated file.
        Path spec = writeTempSpec(
                "openapi: 3.1.0",
                "info: {title: \"evil */ int x;\", version: '1'}",
                "paths:",
                "  /ping:",
                "    get:",
                "      operationId: ping",
                "      responses:",
                "        '200': {description: ok}");
        Path output = generate(spec.toString(), java.util.Map.of());
        String apiHeader = Files.readString(output.resolve("api/DefaultApi.h"));
        Assert.assertTrue(apiHeader.contains("evil * / int x;"),
                "the terminator must be neutralized in the license header");
        Assert.assertFalse(apiHeader.contains("*/ int x"),
                "no generated comment may embed the raw terminator");
    }

    @Test
    public void keepsNullableModelBodyTypedAsOptional() throws IOException {
        // A nullable component body must lower to std::optional<...> and stay
        // typed: the JSON runtime decodes std::optional natively, so only
        // variant unions are untypable.
        Path spec = writeTempSpec(spec(
                "  /maybe:",
                "    post:",
                "      operationId: maybe",
                "      requestBody:",
                "        required: false",
                "        content:",
                "          application/json:",
                "            schema: { $ref: '#/components/schemas/MaybeName' }",
                "      responses:",
                "        '200': {description: ok}",
                "components:",
                "  schemas:",
                "    MaybeName:",
                "      type: ['null', 'string']"));
        Path output = generate(spec.toString(), java.util.Map.of());
        String apiHeader = Files.readString(output.resolve("api/DefaultApi.h"));
        Assert.assertTrue(apiHeader.contains("body{};"),
                "the request struct must exist");
        Assert.assertTrue(
                apiHeader.contains("std::optional<std::string> body{};")
                        || apiHeader.contains("std::optional<std::string> body{"),
                "a nullable body must stay typed as std::optional, header was:\n"
                        + apiHeader.substring(0, Math.min(apiHeader.length(), 4000)));
    }

    @Test
    public void rendersHtmlSignificantNamesUnescapedInLiterals() throws IOException {
        // Path, operationId, and parameter names with & < > " must reach the
        // C++ literals verbatim (C++-escaped once), not HTML-escaped.
        Path spec = writeTempSpec(spec(
                "  /a&b:",
                "    get:",
                "      operationId: getIt",
                "      parameters:",
                "        - name: \"fr&ac\"",
                "          in: query",
                "          schema: {type: string}",
                "      responses:",
                "        '200': {description: ok}"));
        Path output = generate(spec.toString(), java.util.Map.of());
        String apiSource = Files.readString(output.resolve("api/DefaultApi.cpp"));
        Assert.assertTrue(apiSource.contains("\"/a&b\""),
                "the registered route must carry the raw path, not &amp;");
        Assert.assertFalse(apiSource.contains("/a&amp;b"),
                "html escaping must not corrupt the route literal");
        Assert.assertTrue(apiSource.contains("\"fr&ac\""),
                "the query lookup key must carry the raw parameter name");
        Assert.assertFalse(apiSource.contains("fr&amp;ac"),
                "html escaping must not corrupt the parameter name");
        Assert.assertTrue(apiSource.contains("impl->getIt("),
                "the dispatch call must carry the raw operationId nickname");
    }

    @Test
    public void rejectsBranchyRouteOverlapWithWitness() throws IOException {
        // Alternating wildcard/literal layouts with equal literal-token
        // ranking: '/v/{p1}a{p2}a{p3}a' and '/v/a{p4}a{p5}a{p6}' both match
        // '/v/aaa'. The intersection search must terminate in polynomial
        // time on this branchy pair and still PROVE the collision (a
        // step-budgeted DFS could under-report it; the trie BFS cannot).
        Path spec = writeTempSpec(spec(
                "  /v/{p1}a{p2}a{p3}a:",
                "    get:",
                "      operationId: odd",
                "      responses:",
                "        '200': {description: ok}",
                "  /v/a{p4}a{p5}a{p6}:",
                "    get:",
                "      operationId: even",
                "      responses:",
                "        '200': {description: ok}"));
        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-server")
                .setInputSpec(spec.toString())
                .setOutputDir(Files.createTempDirectory("branchy-").toString())
                .setValidateSpec(false);
        IllegalArgumentException error = Assert.expectThrows(
                IllegalArgumentException.class,
                () -> new DefaultGenerator()
                        .opts(configurator.toClientOptInput()).generate());
        Assert.assertTrue(error.getMessage().contains("'/v/"),
                "diagnostic must name a witness path, got: " + error.getMessage());
    }
}
