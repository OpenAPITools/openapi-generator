/*
 * Copyright 2026 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.cppboostbeast;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.Paths;
import io.swagger.v3.oas.models.callbacks.Callback;
import io.swagger.v3.oas.models.headers.Header;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.ComposedSchema;
import io.swagger.v3.oas.models.media.Content;
import io.swagger.v3.oas.models.media.IntegerSchema;
import io.swagger.v3.oas.models.media.MediaType;
import io.swagger.v3.oas.models.media.NumberSchema;
import io.swagger.v3.oas.models.media.ObjectSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.responses.ApiResponses;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.CppBoostBeastClientCodegen;
import org.openapitools.codegen.languages.Oas31CompositionLowering;
import org.openapitools.codegen.languages.Oas31KeywordScanner;
import org.openapitools.codegen.meta.FeatureSet;
import org.openapitools.codegen.meta.features.GlobalFeature;
import org.openapitools.codegen.meta.features.ParameterFeature;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class Oas31GeneratedIrTest extends Oas31IrTestSupport {
    @Test
    public void generatesTypedJsonValuesForOpenApi31Schemas() throws IOException {
        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/json-value-regression.yaml")
                .setOutputDir(output.getAbsolutePath());

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path modelHeader = output.toPath().resolve("model/JsonValueContainer.h");
        Path modelSource = output.toPath().resolve("model/JsonValueContainer.cpp");
        Path cmakeLists = output.toPath().resolve("CMakeLists.txt");
        Path httpClientSource = output.toPath().resolve("api/HttpClientImpl.cpp");

        TestUtils.assertFileContains(modelHeader,
                "std::nullptr_t",
                "boost::json::value",
                "std::map<std::string, boost::json::value>");
        TestUtils.assertFileContains(modelSource,
                "boost::json::serialize",
                VALIDATION_NAMESPACE + "::parseExactJson",
                VALIDATION_NAMESPACE + "::requireModelConvertibleJson(exactJson)",
                VALIDATION_NAMESPACE + "::ExactInstanceScope");
        TestUtils.assertFileNotContains(modelSource,
                "boost::property_tree",
                "fromJsonValue(boost::json::parse");
        TestUtils.assertFileContains(cmakeLists,
                "find_package(Boost 1.75 REQUIRED COMPONENTS json)",
                "find_package(Threads REQUIRED)",
                "find_package(OpenSSL 1.1.0 REQUIRED COMPONENTS SSL Crypto)",
                "set_property(TARGET Threads::Threads PROPERTY IMPORTED_GLOBAL TRUE)",
                "set_property(TARGET OpenSSL::SSL PROPERTY IMPORTED_GLOBAL TRUE)",
                "PUBLIC Boost::boost Boost::json OpenSSL::SSL Threads::Threads",
                "model/schema_ir.generated.cpp",
                "PATTERN \"*.h\" PATTERN \"*.hpp\"");
        TestUtils.assertFileNotContains(cmakeLists, "api/HttpClient.cpp");
        TestUtils.assertFileContains(httpClientSource,
                "SSL_CTX_set_min_proto_version(",
                "TLS1_2_VERSION",
                "boost::asio::ssl::verify_peer",
                "boost::asio::ssl::host_name_verification(m_host)",
                "request.method_string(verb)",
                "target must use HTTP origin-form",
                "header name is reserved by the transport");
    }

    @Test
    public void escapesSpecificationValuesInGeneratedCppLiterals() throws IOException {
        File output = Files.createTempDirectory("cpp-boost-beast-literals").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/"
                        + "literal-escaping-regression.yaml")
                .setOutputDir(output.getAbsolutePath());

        List<File> files = new DefaultGenerator()
                .opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path apiHeader = output.toPath().resolve("api/DefaultApi.h");
        Path apiSource = output.toPath().resolve("api/DefaultApi.cpp");
        Path modelSource = output.toPath().resolve("model/LiteralContainer.cpp");
        TestUtils.assertFileContains(apiSource,
                "\\\"quoted\\\"",
                "q\\\"uery",
                "api-key",
                "X-\\\"Token",
                "read\\\"scope");
        TestUtils.assertFileContains(apiSource,
                "throw DefaultApiException(statusCode, \"bad \\\"quoted\\\"\\nsecond line\", responseBody);");
        TestUtils.assertFileContains(modelSource,
                "object[\"quo\\\"te\"]",
                "object.find(\"quo\\\"te\")",
                "line\\\\backslash\\\"quote");
        TestUtils.assertFileContains(apiHeader,
                "const std::string& context = \"/\\\"quoted\\\"\")");
    }

    @Test
    public void emitsWave1SchemaIrWithExactNumericLexemes() throws IOException {
        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-ir").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/schema-ir-lexeme-regression.yaml")
                .setOutputDir(output.getAbsolutePath());

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path irHeader = output.toPath().resolve("model/Oas31SchemaRegistry.h");
        Path irSource = output.toPath().resolve("model/schema_ir.generated.cpp");
        Path exactHeader = output.toPath().resolve("model/Oas31ExactNumber.h");
        Path irStructs = output.toPath().resolve("model/Oas31SchemaIr.h");
        Path validatorHeader = output.toPath().resolve("model/Oas31Validator.h");

        Assert.assertTrue(java.nio.file.Files.exists(irHeader),
                "Oas31SchemaRegistry.h must be emitted");
        Assert.assertTrue(java.nio.file.Files.exists(irSource),
                "schema_ir.generated.cpp must be emitted");
        Assert.assertFalse(java.nio.file.Files.exists(
                        output.toPath().resolve("model/schema_validate.generated.cpp")),
                "obsolete schema_validate.generated.cpp must not be emitted");
        // Runtime-support headers are rendered into model/.
        Assert.assertTrue(java.nio.file.Files.exists(exactHeader),
                "Oas31ExactNumber.h must be emitted into model/");
        Assert.assertTrue(java.nio.file.Files.exists(irStructs),
                "Oas31SchemaIr.h must be emitted into model/");
        Assert.assertTrue(java.nio.file.Files.exists(validatorHeader),
                "Oas31Validator.h must be emitted into model/");

        String ir = java.nio.file.Files.readString(irSource);
        // Numeric constraints and deep enum stores must carry their ORIGINAL
        // lexemes verbatim: wrapped bounds go through setExact, const through
        // parseLexeme, and pristine enum arrays through parseExactJson. Never
        // permit a rounded double rendering; ExactNumber reconstructs exactly.
        TestUtils.assertFileContains(irSource,
                "setExact(n.minimum, n.hasMinimum, \"0.3\")",
                "setExact(n.multipleOf, n.hasMultipleOf, \"0.1\")",
                "setExact(n.maximum, n.hasMaximum, \"1000\")",
                "setExact(n.exclusiveMinimum, n.hasExclusiveMinimum, \"0\")",
                "setExact(n.exclusiveMaximum, n.hasExclusiveMaximum, \"1.0\")",
                "parseExactJson(R\"OAS0([1.5,2.25,3.0])OAS0\")",
                "ExactNumber::parseLexeme(\"42\")");
        // A rounded double tie (e.g. 0.30000000000000004) must never leak in.
        Assert.assertFalse(ir.contains("0.30000000000000004"),
                "IR must not contain a rounded double rendering of 0.3");
        // Lexemes survive verbatim inside the emitted strings.
        Assert.assertTrue(ir.contains("\"0.3\"") && ir.contains("\"0.1\""),
                "decimal lexemes must appear verbatim in the IR source");

        // Main branches are resolved by their registry IDs and decoded through
        // the shared SchemaEvaluator directly, without an unused wrapper TU.
        TestUtils.assertFileContains(irSource,
                "if (id == \"Amount_branch_0\")",
                "if (id == \"Amount_branch_2\")");

        // The generated registry owns exactly one process-wide evaluator, and
        // model decode adapters route through that same instance.
        Path amountSource = output.toPath().resolve("model/Amount.cpp");
        TestUtils.assertFileContains(irHeader,
                "SchemaResourceRegistry const& schemaRegistry();",
                "SchemaEvaluator const& sharedSchemaEvaluator();",
                "SchemaIndex schemaNodeFor(std::string const& id);");
        TestUtils.assertFileContains(irSource,
                "SchemaEvaluator const& sharedSchemaEvaluator()",
                "static SchemaEvaluator const evaluator(schemaRegistry())",
                "n.schemaPath = \"#/components/schemas/Amount/oneOf/0\"",
                "n.absSchemaUri = \"urn:openapi-generator:cpp-boost-beast:schema#/components/schemas/Amount/oneOf/0\"",
                "n.annExamplesJson = ",
                "[0.3,0.4]",
                "n.annContentSchemaJson = ",
                "n.annExtras.push_back({\"x-note\"");
        TestUtils.assertFileContains(amountSource,
                VALIDATION_NAMESPACE + "::sharedSchemaEvaluator().validate");
        TestUtils.assertFileNotContains(amountSource,
                "static SchemaEvaluator const evaluator");
    }

    @Test
    public void recoversEmptyEnumFromJsonSpec() throws Exception {
        // JSON one-line input must still recover `enum: []` (reject-all) via the
        // format-tolerant raw-text recovery; the parser otherwise degrades the
        // branch to types=[string] and only the string case would wrongly pass.
        Path inputDirectory = java.nio.file.Files.createTempDirectory(
                java.nio.file.Files.createDirectories(Path.of("target")),
                "jsts enum empty ");
        Path spec = inputDirectory.resolve("schema #1.json");
        java.nio.file.Files.writeString(spec,
                "{\"openapi\":\"3.1.0\",\"info\":{\"title\":\"t\",\"version\":\"1.0.0\"},"
              + "\"paths\":{},\"components\":{\"schemas\":{\"G0\":"
              + "{\"oneOf\":[{\"enum\":[]}]}}}}");
        File output = inputDirectory.resolve("generated").toFile();
        CodegenConfigurator cfg = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec(spec.toString())
                .setOutputDir(output.getAbsolutePath());
        new DefaultGenerator().opts(cfg.toClientOptInput()).generate();
        Path ir = output.toPath().resolve("model/schema_ir.generated.cpp");
        Assert.assertTrue(java.nio.file.Files.exists(ir), "IR must be emitted");
        String content = java.nio.file.Files.readString(ir);
        Assert.assertTrue(content.contains("n.hasEnumJson = true;"),
                "empty enum must be materialised as zero-member deep store "
                + "(reject-all) for JSON specs: " + content);
    }

    @Test
    public void recoversEmptyEnumForQuotedYamlComponentName() throws Exception {
        Path spec = java.nio.file.Files.createTempFile("jsts-enum-empty-quoted", ".yaml");
        spec.toFile().deleteOnExit();
        java.nio.file.Files.writeString(spec,
                "openapi: 3.1.0\n"
              + "info: {title: t, version: 1.0.0}\n"
              + "paths: {}\n"
              + "components:\n"
              + "  schemas:\n"
              + "    'G0 # enum':\n"
              + "      oneOf:\n"
              + "        - enum: []\n");
        File output = java.nio.file.Files.createTempDirectory(
                "cpp-boost-beast-enum-empty-quoted").toFile();
        output.deleteOnExit();
        CodegenConfigurator cfg = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec(spec.toString())
                .setOutputDir(output.getAbsolutePath())
                .setValidateSpec(false);
        new DefaultGenerator().opts(cfg.toClientOptInput()).generate();
        Path ir = output.toPath().resolve("model/schema_ir.generated.cpp");
        Assert.assertTrue(java.nio.file.Files.readString(ir)
                        .contains("n.hasEnumJson = true;"),
                "empty enum recovery must use parsed YAML keys, not textual key matching");
    }

    @Test
    public void recoversPristineFactsFromInlineOperationSchemas() throws Exception {
        Path spec = java.nio.file.Files.createTempFile("oas31-inline-recovery", ".yaml");
        spec.toFile().deleteOnExit();
        java.nio.file.Files.writeString(spec,
                "openapi: 3.1.0\n"
              + "info: {title: t, version: 1.0.0}\n"
              + "paths:\n"
              + "  /items:\n"
              + "    post:\n"
              + "      requestBody:\n"
              + "        content:\n"
              + "          application/json:\n"
              + "            schema:\n"
              + "              type: object\n"
              + "              properties:\n"
              + "                choice:\n"
              + "                  oneOf:\n"
              + "                    - enum: []\n"
              + "                tuple:\n"
              + "                  type: array\n"
              + "                  prefixItems:\n"
              + "                    - {type: string}\n"
              + "                    - {type: integer}\n"
              + "      responses:\n"
              + "        '204': {description: ok}\n");
        File output = java.nio.file.Files.createTempDirectory(
                "cpp-boost-beast-inline-recovery").toFile();
        output.deleteOnExit();
        CodegenConfigurator cfg = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec(spec.toString())
                .setOutputDir(output.getAbsolutePath());

        new DefaultGenerator().opts(cfg.toClientOptInput()).generate();

        Path irSource = output.toPath().resolve("model/schema_ir.generated.cpp");
        TestUtils.assertFileContains(irSource,
                "n.hasEnumJson = true;",
                "n.prefixItems.push_back(");
    }

    @Test
    public void preservesNestedSchemasThatShareATitleDuringRawRecovery() throws IOException {
        File output = Files.createTempDirectory("cpp-boost-beast-title-collision").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/"
                        + "raw-recovery-title-collision.yaml")
                .setOutputDir(output.getAbsolutePath());

        List<File> files = new DefaultGenerator()
                .opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path nestedHeader = output.toPath().resolve("model/Grammar_format.h");
        Path enclosingHeader = output.toPath().resolve("model/Grammar_format_1.h");
        TestUtils.assertFileContains(nestedHeader,
                "m_Definition",
                "m_Syntax");
        TestUtils.assertFileNotContains(nestedHeader, "m_Grammar");
        TestUtils.assertFileContains(enclosingHeader,
                "std::string getType() const { return \"grammar\"; }",
                "m_Grammar");
        TestUtils.assertFileNotContains(enclosingHeader,
                "m_Definition",
                "m_Syntax");
    }


    @Test
    public void recoversFloatCountBoundsFromJsonSpec() throws Exception {
        // swagger-models drops `minItems: 1.0` (getMinItems()==null); the exact
        // raw lexeme must be recovered and emitted so the count bound is
        // enforced via ExactNumber (1.0 == 1 mathematically).
        Path spec = java.nio.file.Files.createTempFile("jsts-bound-float", ".json");
        spec.toFile().deleteOnExit();
        java.nio.file.Files.writeString(spec,
                "{\"openapi\":\"3.1.0\",\"info\":{\"title\":\"t\",\"version\":\"1.0.0\"},"
              + "\"paths\":{},\"components\":{\"schemas\":{\"G0\":"
              + "{\"oneOf\":[{\"minItems\":1.0,\"maxProperties\":2.0}]}}}}");
        File output = java.nio.file.Files.createTempDirectory(
                "cpp-boost-beast-bound-float").toFile();
        output.deleteOnExit();
        CodegenConfigurator cfg = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec(spec.toString())
                .setOutputDir(output.getAbsolutePath());
        new DefaultGenerator().opts(cfg.toClientOptInput()).generate();
        Path ir = output.toPath().resolve("model/schema_ir.generated.cpp");
        Assert.assertTrue(java.nio.file.Files.exists(ir), "IR must be emitted");
        String content = java.nio.file.Files.readString(ir);
        Assert.assertTrue(content.contains("\"1.0\""),
                "minItems float lexeme must be preserved: " + content);
        Assert.assertTrue(content.contains("\"2.0\""),
                "maxProperties float lexeme must be preserved: " + content);
    }

    @Test
    public void wave25EmitsStringAndPatternSurface() throws Exception {
        // Type-ARRAY union flags, code-point string lengths, patternProperties
        // and propertyNames must ALL be emitted into the densified IR (never
        // silent, never collapsed to a single type).
        Path spec = java.nio.file.Files.createTempFile("jsts-wave25", ".json");
        spec.toFile().deleteOnExit();
        java.nio.file.Files.writeString(spec,
                "{\"openapi\":\"3.1.0\",\"info\":{\"title\":\"t\",\"version\":\"1.0.0\"},"
              + "\"paths\":{},\"components\":{\"schemas\":{\"G0\":"
              + "{\"oneOf\":[{\"type\":[\"integer\",\"string\"],"
              + "\"minLength\":2.0,\"patternProperties\":{\"f.*o\":{\"type\":\"integer\"}},"
              + "\"propertyNames\":{\"maxLength\":5}}]}}}}");
        File output = java.nio.file.Files.createTempDirectory(
                "cpp-boost-beast-wave25").toFile();
        output.deleteOnExit();
        CodegenConfigurator cfg = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec(spec.toString())
                .setOutputDir(output.getAbsolutePath());
        new DefaultGenerator().opts(cfg.toClientOptInput()).generate();
        Path ir = output.toPath().resolve("model/schema_ir.generated.cpp");
        Assert.assertTrue(java.nio.file.Files.exists(ir), "IR must be emitted");
        String content = java.nio.file.Files.readString(ir);
        // typeFlags 72u = integer(64) | string(8): the FULL union must survive
        // — never a single-type collapse.
        Assert.assertTrue(content.contains("n.typeFlags = 72u;"),
                "type-array must be emitted as the union bitmask: " + content);
        // decimal minLength lexeme preserved via exact-number recovery.
        Assert.assertTrue(content.contains("setExact(n.minLength, n.hasMinLength, \"2.0\")"),
                "minLength decimal lexeme must be preserved: " + content);
        // patternProperties + propertyNames must materialise child rows.
        Assert.assertTrue(content.contains("n.patternProperties.push_back({"),
                "patternProperties child rows must be emitted: " + content);
        Assert.assertTrue(content.contains("n.propertyNames ="),
                "propertyNames child row must be wired: " + content);
    }

    @Test
    public void generatedPathEmitsFullNumericBooleanIr() throws IOException {
        // End-to-end wire pass: the REAL generator must emit the full
        // numeric/boolean keyword set as densified IR from a single committed
        // OAS 3.1 document (oas31-generated-path-regression.yaml). This is the
        // JVM-side guard that keeps the generated path green; the C++ side
        // exercises the shared evaluator through generated model adapters.
        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-wire").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/oas31-generated-path-regression.yaml")
                .setOutputDir(output.getAbsolutePath());

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path irSource = output.toPath().resolve("model/schema_ir.generated.cpp");
        Path irHeader = output.toPath().resolve("model/Oas31SchemaRegistry.h");

        Assert.assertTrue(java.nio.file.Files.exists(irSource),
                "schema_ir.generated.cpp must be emitted");
        Assert.assertFalse(java.nio.file.Files.exists(
                        output.toPath().resolve("model/schema_validate.generated.cpp")),
                "obsolete schema_validate.generated.cpp must not be emitted");
        Assert.assertTrue(java.nio.file.Files.exists(irHeader),
                "Oas31SchemaRegistry.h must be emitted");

        // Each numeric/boolean keyword must survive as its ORIGINAL lexeme (the
        // >2^53 const, decimal multipleOf, huge/tiny exponent bounds, and exact
        // enum arrays). Scalar constraints use ExactNumber::parseLexeme while
        // pristine enums use the deep exact-JSON store. Scalar `const` may be
        // normalized by OpenAPINormalizer to a single-element enum before IR
        // emission; the exact accept/reject semantics remain unchanged.
        TestUtils.assertFileContains(irSource,
                "ExactNumber::parseLexeme(\"1180591620717411303424\")",
                "ExactNumber::parseLexeme(\"1\")",
                "ExactNumber::parseLexeme(\"0\")",
                "parseExactJson(R\"OAS0([1,2.5])OAS0\")",
                "setExact(n.multipleOf, n.hasMultipleOf, \"0.1\")",
                "setExact(n.multipleOf, n.hasMultipleOf, \"0.3\")",
                "setExact(n.minimum, n.hasMinimum, \"5\")",
                "setExact(n.maximum, n.hasMaximum, \"10\")",
                "parseExactJson(R\"OAS0([true])OAS0\")");
        // A partner tell-tale of double rounding must never appear.
        Assert.assertFalse(
                java.nio.file.Files.readString(irSource).contains("1180591620717411325952"),
                "IR must not contain a rounded double rendering of 2^70");

        // Each schema must be addressable by a direct registry lookup.
        TestUtils.assertFileContains(irSource,
                "if (id == \"ExactEqualsOne_branch_0\")",
                "if (id == \"ExactIntegerType_branch_0\")",
                "if (id == \"MulTenth_branch_0\")",
                "if (id == \"MulThird_branch_0\")",
                "if (id == \"RangeMinMax_branch_0\")",
                "if (id == \"ZeroConst_branch_0\")",
                "if (id == \"BigConst_branch_0\")",
                "if (id == \"HugeMax_branch_0\")",
                "if (id == \"TinyMin_branch_0\")",
                "if (id == \"BoolConstTrue_branch_0\")",
                "if (id == \"BoolEnumTrue_branch_0\")",
                "if (id == \"NumberEnumSpellings_branch_0\")");
    }

    @Test
    public void wave1CompleteEmittedIrEndToEnd() throws IOException {
        // Completion guard: the REAL generator must lower a committed OAS 3.1
        // doc (oas31-wave1-complete-regression.yaml) into the densified IR for
        // boolean value-schemas, `not`, deep const/enum (NON-scalar JSON
        // store), uniqueItems, and $ref + resource identity (SchemaResource
        // baseUri/dialectUri/anchor/rootNodes + per-node resourceIdentity).
        // This is the JVM-side assertion of the new emitted IR; the C++
        // compile+verdict side is covered by the committed oas-compliance gate
        // script.
        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-w1c").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/oas31-wave1-complete-regression.yaml")
                .setOutputDir(output.getAbsolutePath())
                .addAdditionalProperty("oas31BaseUri", "urn:openapi-generator:cpp-boost-beast:wave1");

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path irSource = output.toPath().resolve("model/schema_ir.generated.cpp");
        Assert.assertTrue(java.nio.file.Files.exists(irSource),
                "schema_ir.generated.cpp must be emitted");

        String ir = java.nio.file.Files.readString(irSource);

        // Boolean value-schemas must lower to BooleanValue::true_/false_.
        Assert.assertTrue(ir.contains("n.booleanValue = BooleanValue::false_;")
                        && ir.contains("n.booleanValue = BooleanValue::true_;"),
                "boolean value-schemas must emit booleanValue true_/false_");

        // `not`: the NotString branch must reference a densified not-node.
        Assert.assertTrue(java.util.regex.Pattern.compile("n\\.notSchema = \\d+;")
                        .matcher(ir).find(),
                "a `not` branch must emit n.notSchema = <index>;");
        // The not-child helper row is appended (typeFlags string = 8u), never a root.
        Assert.assertTrue(ir.contains("NotString_branch_0_not")
                        && ir.contains("n.typeFlags = 8u;"),
                "the `not` subschema must be densified as its own row");

        // Deep const/enum values use the exact parser and retain a parallel
        // lexeme table; assert payloads independently of the collision-safe
        // raw-string delimiter selected by the emitter.
        TestUtils.assertFileContains(irSource,
                "n.hasEnumJson = true;",
                "parseExactJson(R\"",
                "n.enumJsonLexemes = std::move(_exact.lexemes);",
                "[[1,2,3]]",          // DeepConstArray -> enum [ [1,2,3] ]
                "[{\"a\":1,\"b\":[true,null,2.5]}]", // DeepConstObject object member
                "[[1,2],[3,4]]",     // DeepEnumArray
                "[{\"x\":1},7]");  // DeepEnumMixed object+number member
        // Object/array members must NEVER leak into the numeric parseLexeme bucket
        // (only genuine numbers 7 use it).
        Assert.assertFalse(ir.contains("parseLexeme(\"[1,2"),
                "structural enum members must not be fed to parseLexeme");

        // uniqueItems.
        Assert.assertTrue(ir.contains("n.hasUniqueItems = true;"),
                "uniqueItems must be emitted");

        // $ref: genuine local refs lower to a transparent applicator child.
        // Registry indices are layout-dependent (the combined registry grew to
        // include composed `_component` wrapper rows), so assert the SHAPE: an
        // applicator=ref block must be followed by a children.push_back, and at
        // least two such hops must exist (RefConstForty + RefToThing).
        java.util.regex.Matcher refHop = java.util.regex.Pattern.compile(
                "n\\.applicator = ApplicatorKind::ref;\\s*\\n\\s*n\\.children\\.push_back\\(\\d+\\);")
                .matcher(ir);
        int refHops = 0;
        while (refHop.find()) ++refHops;
        Assert.assertTrue(refHops >= 2,
                "at least two $ref hops expected (RefConstForty, RefToThing), saw " + refHops);

        // Resource identity (SchemaResource baseUri/dialect/rootNodes + node
        // resourceIdentity). The configured retrieval URI must be retained; the
        // OAS 3.1 document has no jsonSchemaDialect, so the emitter selects the
        // pinned OAS dialect. Every main validator row is a resource root, while
        // helper rows appended after the main rows are not.
        TestUtils.assertFileContains(irSource,
                "res.baseUri = \"urn:openapi-generator:cpp-boost-beast:wave1\";",
                "res.dialect = \"https://spec.openapis.org/oas/3.1/dialect/2024-11-10\";");
        // MAIN validator rows are resource roots; helper helper `not`-child rows
        // are NOT. Registry indices moved when the combined registry grew, so
        // assert the relationship (roots exist, not-child excluded) instead of
        // pinned indices.
        java.util.regex.Matcher rootM = java.util.regex.Pattern.compile(
                "res\\.rootNodes\\.push_back\\((\\d+)\\);")
                .matcher(ir);
        java.util.Set<Integer> roots = new java.util.HashSet<>();
        while (rootM.find()) roots.add(Integer.valueOf(rootM.group(1)));
        Assert.assertTrue(roots.size() >= 2, "at least two resource roots expected, saw " + roots);
        java.util.regex.Matcher notChildM = java.util.regex.Pattern.compile(
                "if \\(id == \\\"(NotString_branch_0_not)\\\"\\) return (\\d+);")
                .matcher(ir);
        Assert.assertTrue(notChildM.find(), "schemaNodeFor must map the not-child row");
        int notChildIndex = Integer.parseInt(notChildM.group(2));
        Assert.assertFalse(roots.contains(notChildIndex),
                "the helper `not`-child row (node " + notChildIndex + ") must not be a resource root");
        Assert.assertTrue(ir.contains("n.resourceIdentity = 0;"),
                "every densified node must carry a resourceIdentity");

        // Main schemas remain directly addressable through the registry.
        TestUtils.assertFileContains(irSource,
                "if (id == \"NotString_branch_0\")",
                "if (id == \"AlwaysTrueSchema_branch_0\")",
                "if (id == \"RefToThing_branch_0\")");
    }
}
