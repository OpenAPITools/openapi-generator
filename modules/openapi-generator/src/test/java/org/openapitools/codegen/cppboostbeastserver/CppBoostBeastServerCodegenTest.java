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
    public void rejectsMultipartRequestBody() throws IOException {
        Path spec = writeTempSpec(spec(
                "  /upload:",
                "    post:",
                "      operationId: upload",
                "      requestBody:",
                "        content:",
                "          multipart/form-data:",
                "            schema:",
                "              type: object",
                "      responses:",
                "        '200': {description: ok}"));
        IllegalArgumentException error = Assert.expectThrows(
                IllegalArgumentException.class,
                () -> generate(spec.toString(), java.util.Map.of()));
        Assert.assertTrue(error.getMessage().contains("multipart/form-data"),
                "diagnostic must name the media type: " + error.getMessage());
        Assert.assertTrue(error.getMessage().startsWith("cpp-boost-beast-server: "));
    }

    @Test
    public void rejectsFormUrlencodedBody() throws IOException {
        Path spec = writeTempSpec(spec(
                "  /form:",
                "    post:",
                "      operationId: submit",
                "      requestBody:",
                "        content:",
                "          application/x-www-form-urlencoded:",
                "            schema:",
                "              type: object",
                "      responses:",
                "        '200': {description: ok}"));
        IllegalArgumentException error = Assert.expectThrows(
                IllegalArgumentException.class,
                () -> generate(spec.toString(), java.util.Map.of()));
        Assert.assertTrue(error.getMessage().contains("application/x-www-form-urlencoded"));
    }

    @Test
    public void rejectsPlainTextResponse() throws IOException {
        Path spec = writeTempSpec(spec(
                "  /text:",
                "    get:",
                "      operationId: getText",
                "      responses:",
                "        '200':",
                "          description: ok",
                "          content:",
                "            text/plain:",
                "              schema: {type: string}"));
        IllegalArgumentException error = Assert.expectThrows(
                IllegalArgumentException.class,
                () -> generate(spec.toString(), java.util.Map.of()));
        Assert.assertTrue(error.getMessage().contains("text/plain"));
    }

    @Test
    public void rejectsEventStreamResponse() throws IOException {
        Path spec = writeTempSpec(spec(
                "  /stream:",
                "    get:",
                "      operationId: stream",
                "      responses:",
                "        '200':",
                "          description: events",
                "          content:",
                "            text/event-stream:",
                "              schema: {type: string}"));
        IllegalArgumentException error = Assert.expectThrows(
                IllegalArgumentException.class,
                () -> generate(spec.toString(), java.util.Map.of()));
        Assert.assertTrue(error.getMessage().contains("text/event-stream"));
    }

    @Test
    public void rejectsContentStyleParameter() throws IOException {
        Path spec = writeTempSpec(spec(
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
                "        '200': {description: ok}"));
        IllegalArgumentException error = Assert.expectThrows(
                IllegalArgumentException.class,
                () -> generate(spec.toString(), java.util.Map.of()));
        Assert.assertTrue(error.getMessage().contains("content-style"));
    }

    @Test
    public void rejectsCookieMatrixStyle() throws IOException {
        Path spec = writeTempSpec(spec(
                "  /c:",
                "    get:",
                "      operationId: op",
                "      parameters:",
                "        - name: session",
                "          in: cookie",
                "          style: matrix",
                "          schema: {type: string}",
                "      responses:",
                "        '200': {description: ok}"));
        IllegalArgumentException error = Assert.expectThrows(
                IllegalArgumentException.class,
                () -> generate(spec.toString(), java.util.Map.of()));
        Assert.assertTrue(error.getMessage().contains("matrix"));
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
}
