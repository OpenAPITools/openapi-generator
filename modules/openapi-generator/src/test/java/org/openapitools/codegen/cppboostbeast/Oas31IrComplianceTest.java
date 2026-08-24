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
import io.swagger.v3.oas.models.media.Content;
import io.swagger.v3.oas.models.media.MediaType;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.ComposedSchema;
import io.swagger.v3.oas.models.media.IntegerSchema;
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

public class Oas31IrComplianceTest {
    private static final String VALIDATION_NAMESPACE =
            "org::openapitools::client::model::detail::schema_validation";

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
                "find_package(Boost 1.75 REQUIRED)",
                "find_package(Threads REQUIRED)",
                "find_package(OpenSSL 1.1.0 REQUIRED COMPONENTS SSL Crypto)",
                "set_property(TARGET Threads::Threads PROPERTY IMPORTED_GLOBAL TRUE)",
                "set_property(TARGET OpenSSL::SSL PROPERTY IMPORTED_GLOBAL TRUE)",
                "PUBLIC Boost::boost OpenSSL::SSL Threads::Threads",
                "model/schema_ir.generated.cpp",
                "model/schema_validate.generated.cpp",
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
        Path dispatch = output.toPath().resolve("model/schema_validate.generated.cpp");
        Path exactHeader = output.toPath().resolve("model/Oas31ExactNumber.h");
        Path irStructs = output.toPath().resolve("model/Oas31SchemaIr.h");
        Path validatorHeader = output.toPath().resolve("model/Oas31Validator.h");

        Assert.assertTrue(java.nio.file.Files.exists(irHeader),
                "Oas31SchemaRegistry.h must be emitted");
        Assert.assertTrue(java.nio.file.Files.exists(irSource),
                "schema_ir.generated.cpp must be emitted");
        Assert.assertTrue(java.nio.file.Files.exists(dispatch),
                "schema_validate.generated.cpp must be emitted");
        // Runtime-support headers are rendered into model/.
        Assert.assertTrue(java.nio.file.Files.exists(exactHeader),
                "Oas31ExactNumber.h must be emitted into model/");
        Assert.assertTrue(java.nio.file.Files.exists(irStructs),
                "Oas31SchemaIr.h must be emitted into model/");
        Assert.assertTrue(java.nio.file.Files.exists(validatorHeader),
                "Oas31Validator.h must be emitted into model/");

        String ir = java.nio.file.Files.readString(irSource);
        // Numeric constraints must carry their ORIGINAL lexeme verbatim (the
        // wrapped bounds go through setExact, enum/const through parseLexeme) —
        // never a rounded double rendering, so ExactNumber reconstructs exactly.
        TestUtils.assertFileContains(irSource,
                "setExact(n.minimum, n.hasMinimum, \"0.3\")",
                "setExact(n.multipleOf, n.hasMultipleOf, \"0.1\")",
                "setExact(n.maximum, n.hasMaximum, \"1000\")",
                "setExact(n.exclusiveMinimum, n.hasExclusiveMinimum, \"0\")",
                "setExact(n.exclusiveMaximum, n.hasExclusiveMaximum, \"1.0\")",
                "ExactNumber::parseLexeme(\"1.5\")",
                "ExactNumber::parseLexeme(\"2.25\")",
                "ExactNumber::parseLexeme(\"3.0\")",
                "ExactNumber::parseLexeme(\"42\")");
        // A rounded double tie (e.g. 0.30000000000000004) must never leak in.
        Assert.assertFalse(ir.contains("0.30000000000000004"),
                "IR must not contain a rounded double rendering of 0.3");
        // Lexemes survive verbatim inside the emitted strings.
        Assert.assertTrue(ir.contains("\"0.3\"") && ir.contains("\"0.1\""),
                "decimal lexemes must appear verbatim in the IR source");

        // validate_<id> thin dispatch delegates to SchemaEvaluator over the node.
        String dispatchContent = java.nio.file.Files.readString(dispatch);
        Assert.assertTrue(dispatchContent.contains("validate_Amount_branch_0"),
                "thin dispatch must emit validate_Amount_branch_0");
        Assert.assertTrue(dispatchContent.contains("validate_Amount_branch_2"),
                "thin dispatch must emit validate_Amount_branch_2");
        TestUtils.assertFileContains(dispatch,
                "namespace " + VALIDATION_NAMESPACE + " {",
                "sharedSchemaEvaluator().validate",
                "schemaNodeFor");
        TestUtils.assertFileNotContains(dispatch,
                "static SchemaEvaluator const evaluator");

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
    public void generatedPathEmitsFullNumericBooleanDispatch() throws IOException {
        // End-to-end wire pass: the REAL generator must emit the full
        // numeric/boolean keyword set as densified IR + a thin validate_<id>
        // dispatch from a single committed OAS 3.1 doc
        // (oas31-generated-path-regression.yaml). This is the JVM-side guard
        // that keeps the generated path green; the C++ side (compile + run
        // verdicts through the emitted dispatch) is covered by the committed
        // oas-compliance gate script.
        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-wire").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/oas31-generated-path-regression.yaml")
                .setOutputDir(output.getAbsolutePath());

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path irSource = output.toPath().resolve("model/schema_ir.generated.cpp");
        Path dispatch = output.toPath().resolve("model/schema_validate.generated.cpp");
        Path irHeader = output.toPath().resolve("model/Oas31SchemaRegistry.h");

        Assert.assertTrue(java.nio.file.Files.exists(irSource),
                "schema_ir.generated.cpp must be emitted");
        Assert.assertTrue(java.nio.file.Files.exists(dispatch),
                "schema_validate.generated.cpp must be emitted");
        Assert.assertTrue(java.nio.file.Files.exists(irHeader),
                "Oas31SchemaRegistry.h must be emitted");

        // Each numeric/boolean keyword must survive as its ORIGINAL lexeme (the
        // >2^53 const, decimal multipleOf, and huge/tiny exponent bounds) so
        // ExactNumber::parseLexeme reconstructs the value exactly.  Note: scalar
        // `const` is normalized by openapi-generator to a single-element `enum`
        // (OpenAPINormalizer) before IR emission, which is semantically identical
        // to const (single deep-equal value) and yields the same accept/reject
        // verdicts; the values below therefore appear as ExactNumber::parseLexeme.
        TestUtils.assertFileContains(irSource,
                "ExactNumber::parseLexeme(\"1180591620717411303424\")",
                "ExactNumber::parseLexeme(\"1\")",
                "ExactNumber::parseLexeme(\"0\")",
                "ExactNumber::parseLexeme(\"2.5\")",
                "setExact(n.multipleOf, n.hasMultipleOf, \"0.1\")",
                "setExact(n.multipleOf, n.hasMultipleOf, \"0.3\")",
                "setExact(n.minimum, n.hasMinimum, \"5\")",
                "setExact(n.maximum, n.hasMaximum, \"10\")",
                "n.enumBooleans.push_back(true)");
        // A partner tell-tale of double rounding must never appear.
        Assert.assertFalse(
                java.nio.file.Files.readString(irSource).contains("1180591620717411325952"),
                "IR must not contain a rounded double rendering of 2^70");

        // The thin dispatch must expose validate_<id> for every schema in the doc.
        TestUtils.assertFileContains(dispatch,
                "validate_ExactEqualsOne_branch_0",
                "validate_ExactIntegerType_branch_0",
                "validate_MulTenth_branch_0",
                "validate_MulThird_branch_0",
                "validate_RangeMinMax_branch_0",
                "validate_ZeroConst_branch_0",
                "validate_BigConst_branch_0",
                "validate_HugeMax_branch_0",
                "validate_TinyMin_branch_0",
                "validate_BoolConstTrue_branch_0",
                "validate_BoolEnumTrue_branch_0",
                "validate_NumberEnumSpellings_branch_0");
        TestUtils.assertFileContains(dispatch,
                "namespace " + VALIDATION_NAMESPACE + " {",
                "sharedSchemaEvaluator().validate",
                "schemaNodeFor");
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
        Path dispatch = output.toPath().resolve("model/schema_validate.generated.cpp");
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

        // Every schema still gets a thin validate_<id> dispatch.
        TestUtils.assertFileContains(dispatch,
                "validate_NotString_branch_0",
                "validate_AlwaysTrueSchema_branch_0",
                "validate_RefToThing_branch_0");
    }

    @Test
    public void notAssertionNowSupportedOnOneOf() {
        // `not` is implemented by the shared IR/evaluator, so generation no
        // longer fail-closes; the subschema must be surfaced to the IR emitter
        // via validation-not-schema.
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI = new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.0.4");
        io.swagger.v3.oas.models.Components components = new io.swagger.v3.oas.models.Components();
        Map<String, Schema> schemas = new HashMap<>();

        ComposedSchema schema = new ComposedSchema();
        StringSchema branchWithNot = new StringSchema();
        io.swagger.v3.oas.models.media.Schema notSchema =
                new io.swagger.v3.oas.models.media.Schema();
        notSchema.setType("integer");
        branchWithNot.setNot(notSchema);
        schema.addOneOfItem(branchWithNot);
        schemas.put("SchemaWithNotOnOneOf", schema);
        components.setSchemas(schemas);
        openAPI.setComponents(components);

        // Must NOT throw; the branch must carry the `not` subschema for IR.
        codegen.preprocessOpenAPI(openAPI);
        Oas31CompositionLowering.CompositionDescriptor desc =
                codegen.getCompositionDescriptor("SchemaWithNotOnOneOf");
        Assert.assertNotNull(desc, "SchemaWithNotOnOneOf must have a descriptor");
        Assert.assertNotNull(
                desc.getBranches().get(0).getValidateParams().get("validation-not-schema"),
                "not subschema must be surfaced as validation-not-schema");
    }

    @Test
    public void notAssertionNowSupportedOnAnyOf() {
        // `not` is implemented; anyOf no longer fail-closes and the subschema
        // is surfaced for IR emission.
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI = new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.0.4");
        io.swagger.v3.oas.models.Components components = new io.swagger.v3.oas.models.Components();
        Map<String, Schema> schemas = new HashMap<>();

        ComposedSchema schema = new ComposedSchema();
        StringSchema branchWithNot = new StringSchema();
        io.swagger.v3.oas.models.media.Schema notSchema =
                new io.swagger.v3.oas.models.media.Schema();
        notSchema.setType("object");
        branchWithNot.setNot(notSchema);
        schema.addAnyOfItem(branchWithNot);
        schemas.put("SchemaWithNotOnAnyOf", schema);
        components.setSchemas(schemas);
        openAPI.setComponents(components);

        codegen.preprocessOpenAPI(openAPI);
        Oas31CompositionLowering.CompositionDescriptor desc =
                codegen.getCompositionDescriptor("SchemaWithNotOnAnyOf");
        Assert.assertNotNull(desc, "SchemaWithNotOnAnyOf must have a descriptor");
        Assert.assertNotNull(
                desc.getBranches().get(0).getValidateParams().get("validation-not-schema"),
                "not subschema must be surfaced as validation-not-schema");
    }

    @Test
    public void notAssertionNowSupportedOnAllOf() {
        // `not` is implemented by the shared evaluator, so even allOf no
        // longer fail-closes and the subschema is surfaced for IR.
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI = new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.0.4");
        io.swagger.v3.oas.models.Components components = new io.swagger.v3.oas.models.Components();
        Map<String, Schema> schemas = new HashMap<>();

        ComposedSchema schema = new ComposedSchema();
        io.swagger.v3.oas.models.media.Schema branchWithNot =
                new io.swagger.v3.oas.models.media.Schema();
        branchWithNot.setType("object");
        io.swagger.v3.oas.models.media.Schema notSchema =
                new io.swagger.v3.oas.models.media.Schema();
        notSchema.setType("array");
        branchWithNot.setNot(notSchema);
        schema.addAllOfItem(branchWithNot);
        schemas.put("SchemaWithNotOnAllOf", schema);
        components.setSchemas(schemas);
        openAPI.setComponents(components);

        codegen.preprocessOpenAPI(openAPI);
        Oas31CompositionLowering.CompositionDescriptor desc =
                codegen.getCompositionDescriptor("SchemaWithNotOnAllOf");
        Assert.assertNotNull(desc, "SchemaWithNotOnAllOf must have a descriptor");
        Assert.assertNotNull(
                desc.getBranches().get(0).getValidateParams().get("validation-not-schema"),
                "not subschema must be surfaced as validation-not-schema");
    }

    @Test
    public void generatedCompositionDecodeDelegatesToSharedExactEvaluator() throws IOException {
        String specContent =
            "openapi: 3.0.3\n" +
            "info:\n" +
            "  title: validator-output-test\n" +
            "  version: 1.0.0\n" +
            "paths: {}\n" +
            "components:\n" +
            "  schemas:\n" +
            "    ConstrainedNumber:\n" +
            "      oneOf:\n" +
            "        - type: integer\n" +
            "          multipleOf: 3\n" +
            "          minimum: 10\n" +
            "          maximum: 100\n" +
            "          exclusiveMinimum: true\n" +
            "        - type: integer\n" +
            "          enum: [1, 2, 3]\n";

        java.nio.file.Path specFile = java.nio.file.Files.createTempFile("validator-output-", ".yaml");
        specFile.toFile().deleteOnExit();
        java.nio.file.Files.writeString(specFile, specContent);

        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-validator-output").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec(specFile.toAbsolutePath().toString())
                .setOutputDir(output.getAbsolutePath())
                .addAdditionalProperty("packageName", "CppBoostBeastValidatorTest");

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path constrainedSource = output.toPath().resolve("model/ConstrainedNumber.cpp");
        Path irSource = output.toPath().resolve("model/schema_ir.generated.cpp");
        TestUtils.assertFileExists(constrainedSource);
        TestUtils.assertFileExists(irSource);

        TestUtils.assertFileContains(constrainedSource,
                VALIDATION_NAMESPACE + "::schemaNodeFor",
                VALIDATION_NAMESPACE + "::sharedSchemaEvaluator().validate");
        TestUtils.assertFileNotContains(constrainedSource,
                "std::fmod(",
                "rawInt == static_cast<std::int64_t>");
        TestUtils.assertFileContains(irSource,
                "setExact(n.multipleOf, n.hasMultipleOf, \"3\")",
                "setExact(n.minimum, n.hasMinimum, \"10\")",
                "setExact(n.maximum, n.hasMaximum, \"100\")",
                "ExactNumber::parseLexeme(\"1\")",
                "ExactNumber::parseLexeme(\"2\")",
                "ExactNumber::parseLexeme(\"3\")");
    }

    // --- Strong review: properties/additionalProperties fail-closed ---

    @Test
    public void propertiesOnOneOfBranchNoLongerFailsGeneration() {
        // Properties on a composition branch no longer fail generation
        // because 'properties' was removed from unsupported assertions.
        // The branch descriptor should have object-properties in supported.
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI = new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.0.4");
        io.swagger.v3.oas.models.Components components = new io.swagger.v3.oas.models.Components();
        Map<String, Schema> schemas = new HashMap<>();

        ComposedSchema schema = new ComposedSchema();
        ObjectSchema objBranch = new ObjectSchema();
        objBranch.addProperties("name", new StringSchema());
        // No required — only properties, no required
        schema.addOneOfItem(objBranch);
        schemas.put("SchemaWithProperties", schema);
        components.setSchemas(schemas);
        openAPI.setComponents(components);

        // Must not throw
        codegen.preprocessOpenAPI(openAPI);

        Oas31CompositionLowering.CompositionDescriptor desc =
                codegen.getCompositionDescriptor("SchemaWithProperties");
        Assert.assertNotNull(desc, "SchemaWithProperties must have a descriptor");
        Assert.assertEquals(desc.getBranches().size(), 1);
        Oas31CompositionLowering.CompositionBranchDescriptor branch = desc.getBranches().get(0);
        // Properties on branches may not produce "object-properties" in the
        // supported set. Verify the branch is present and carries the type
        // assertion.
        Assert.assertTrue(branch.getSupportedAssertions().contains("type"),
                "Branch with properties must have type assertion in supported");
    }

    @Test
    public void requiredOnlyOnBranchSucceeds() {
        // required-only on a composition branch must NOT fail generation.
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI = new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.0.4");
        io.swagger.v3.oas.models.Components components = new io.swagger.v3.oas.models.Components();
        Map<String, Schema> schemas = new HashMap<>();

        ComposedSchema schema = new ComposedSchema();
        ObjectSchema objBranch = new ObjectSchema();
        objBranch.setRequired(Arrays.asList("name"));
        // No properties — only required
        schema.addOneOfItem(objBranch);
        schemas.put("SchemaWithRequiredOnly", schema);
        components.setSchemas(schemas);
        openAPI.setComponents(components);

        // Must not throw
        codegen.preprocessOpenAPI(openAPI);

        Oas31CompositionLowering.CompositionDescriptor desc =
                codegen.getCompositionDescriptor("SchemaWithRequiredOnly");
        Assert.assertNotNull(desc, "SchemaWithRequiredOnly must have a descriptor");
        Assert.assertEquals(desc.getBranches().size(), 1);

        Oas31CompositionLowering.CompositionBranchDescriptor branch = desc.getBranches().get(0);
        Assert.assertTrue(branch.getSupportedAssertions().contains("object-properties"),
                "Required-only branch must have object-properties in supported");
        Assert.assertFalse(branch.getUnsupportedAssertions().contains("properties"),
                "Required-only branch must not have properties in unsupported");
    }

    // --- Strong review: boolean schema fail-closed ---

    @Test
    public void booleanTrueSchemaOnOneOfBranchNowSupported() {
        // OAS 3.1 true value schema (always-match) is implemented by the shared
        // IR/evaluator (BooleanValue::true_) and must surface the literal
        // through validation-boolean-value instead of fail-closing.
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI = new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.1.0");
        io.swagger.v3.oas.models.Components components = new io.swagger.v3.oas.models.Components();
        Map<String, Schema> schemas = new HashMap<>();

        ComposedSchema schema = new ComposedSchema();
        Schema boolTrueBranch = new Schema();
        boolTrueBranch.booleanSchemaValue(true);
        schema.addOneOfItem(boolTrueBranch);
        schemas.put("SchemaWithBoolTrue", schema);
        components.setSchemas(schemas);
        openAPI.setComponents(components);

        codegen.preprocessOpenAPI(openAPI);
        Oas31CompositionLowering.CompositionDescriptor desc =
                codegen.getCompositionDescriptor("SchemaWithBoolTrue");
        Assert.assertNotNull(desc, "SchemaWithBoolTrue must have a descriptor");
        Assert.assertEquals(
                desc.getBranches().get(0).getValidateParams().get("validation-boolean-value"),
                Boolean.TRUE,
                "boolean true value-schema must be surfaced");
    }

    @Test
    public void booleanFalseSchemaOnOneOfBranchNowSupported() {
        // OAS 3.1 false value schema (never-match) is implemented by the shared
        // IR/evaluator (BooleanValue::false_).
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI = new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.1.0");
        io.swagger.v3.oas.models.Components components = new io.swagger.v3.oas.models.Components();
        Map<String, Schema> schemas = new HashMap<>();

        ComposedSchema schema = new ComposedSchema();
        Schema boolFalseBranch = new Schema();
        boolFalseBranch.booleanSchemaValue(false);
        schema.addOneOfItem(boolFalseBranch);
        schemas.put("SchemaWithBoolFalse", schema);
        components.setSchemas(schemas);
        openAPI.setComponents(components);

        codegen.preprocessOpenAPI(openAPI);
        Oas31CompositionLowering.CompositionDescriptor desc =
                codegen.getCompositionDescriptor("SchemaWithBoolFalse");
        Assert.assertNotNull(desc, "SchemaWithBoolFalse must have a descriptor");
        Assert.assertEquals(
                desc.getBranches().get(0).getValidateParams().get("validation-boolean-value"),
                Boolean.FALSE,
                "boolean false value-schema must be surfaced");
    }

    @Test
    public void duplicateBooleanValueSchemasRetainBranchCardinality() throws Exception {
        Path workspace = java.nio.file.Files.createTempDirectory(
                java.nio.file.Files.createDirectories(Path.of("target")),
                "oas31-duplicate-boolean-branches");
        Path spec = workspace.resolve("input.json");
        java.nio.file.Files.writeString(spec,
                "{\"openapi\":\"3.1.0\",\"info\":{\"title\":\"t\",\"version\":\"1\"},"
              + "\"paths\":{},\"components\":{\"schemas\":{\"G0\":"
              + "{\"oneOf\":[true,true,false]}}}}}");
        File output = workspace.resolve("generated").toFile();
        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec(spec.toString())
                .setOutputDir(output.getAbsolutePath());

        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        String ir = java.nio.file.Files.readString(
                output.toPath().resolve("model/schema_ir.generated.cpp"));
        TestUtils.assertFileContains(
                output.toPath().resolve("model/schema_ir.generated.cpp"),
                "G0_branch_0", "G0_branch_1", "G0_branch_2");
        Assert.assertTrue(ir.contains("BooleanValue::true_"));
        Assert.assertTrue(ir.contains("BooleanValue::false_"));
    }

    // --- Strong review: additionalProperties false fail-closed ---

    @Test
    public void additionalPropertiesFalseOnOneOfBranchSurfacesAsReject() {
        // additionalProperties: false on a composition branch is NO LONGER
        // fail-closed. It is emitted as the `reject` tri-state so the evaluator
        // rejects unlisted keys; generation must proceed.
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI = new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.0.4");
        io.swagger.v3.oas.models.Components components = new io.swagger.v3.oas.models.Components();
        Map<String, Schema> schemas = new HashMap<>();

        ComposedSchema schema = new ComposedSchema();
        ObjectSchema objBranch = new ObjectSchema();
        // OAS 3.0: additionalProperties: false via setAdditionalProperties(Boolean)
        objBranch.setAdditionalProperties(Boolean.FALSE);
        schema.addOneOfItem(objBranch);
        schemas.put("SchemaWithAddPropsFalse", schema);
        components.setSchemas(schemas);
        openAPI.setComponents(components);

        // Must NOT throw (no longer fail-closed).
        codegen.preprocessOpenAPI(openAPI);

        Oas31CompositionLowering.CompositionDescriptor desc =
                codegen.getCompositionDescriptor("SchemaWithAddPropsFalse");
        Assert.assertNotNull(desc, "SchemaWithAddPropsFalse must have a descriptor");
        Oas31CompositionLowering.CompositionBranchDescriptor branch =
                desc.getBranches().get(0);
        Assert.assertTrue(branch.getSupportedAssertions().contains("additional-properties"),
                "additional-properties must be a supported assertion now");
        Assert.assertEquals(
                branch.getValidateParams().get("validation-additional-properties-kind"),
                "reject",
                "additionalProperties:false must surface the reject tri-state");
        Assert.assertFalse(branch.getUnsupportedAssertions().contains("additional-properties"),
                "additionalProperties must no longer be unsupported");
    }

    @Test
    public void conditionalIfOnOneOfBranchIsEmittedNotThrown() {
        // A oneOf branch carrying if/then/else is NO LONGER fail-closed. The if
        // schema is surfaced into the branch validateParams and generation
        // proceeds (honest: a bare if-then-else without ref coverage is
        // measured as FAIL downstream, never BLOCKED-at-emission).
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI = new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.0.4");
        io.swagger.v3.oas.models.Components components = new io.swagger.v3.oas.models.Components();
        Map<String, Schema> schemas = new HashMap<>();

        ComposedSchema schema = new ComposedSchema();
        StringSchema conditionalBranch = new StringSchema();
        io.swagger.v3.oas.models.media.Schema ifSchema =
                new io.swagger.v3.oas.models.media.Schema();
        ifSchema.setType("object");
        conditionalBranch.setIf(ifSchema);
        schema.addOneOfItem(conditionalBranch);
        schemas.put("SchemaWithUnsupportedAssertion", schema);
        components.setSchemas(schemas);
        openAPI.setComponents(components);

        // Must NOT throw anymore.
        codegen.preprocessOpenAPI(openAPI);

        Oas31CompositionLowering.CompositionDescriptor desc =
                codegen.getCompositionDescriptor("SchemaWithUnsupportedAssertion");
        Assert.assertNotNull(desc,
                "SchemaWithUnsupportedAssertion must have a descriptor");
        Oas31CompositionLowering.CompositionBranchDescriptor branch =
                desc.getBranches().get(0);
        Assert.assertNotNull(branch.getValidateParams().get("validation-if-schema"),
                "validation-if-schema must be surfaced for IR emission");
        Assert.assertFalse(branch.getUnsupportedAssertions().contains("conditional"),
                "conditional must no longer be unsupported");
    }

    @Test
    public void contentEncodingAnyOfNoLongerThrows() {
        // contentEncoding is annotation-only per 2020-12 §8.2.6 (no validation
        // behavior — cannot affect anyOf membership), so a content-encoded
        // anyOf branch must not fail generation.
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI = new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.0.4");
        io.swagger.v3.oas.models.Components components = new io.swagger.v3.oas.models.Components();
        Map<String, Schema> schemas = new HashMap<>();

        ComposedSchema schema = new ComposedSchema();
        StringSchema contentEncodedBranch = new StringSchema();
        contentEncodedBranch.setContentEncoding("base64");
        schema.addAnyOfItem(contentEncodedBranch);
        schemas.put("SchemaWithContentAnyOf", schema);
        components.setSchemas(schemas);
        openAPI.setComponents(components);

        // Must NOT throw anymore.
        codegen.preprocessOpenAPI(openAPI);

        Oas31CompositionLowering.CompositionDescriptor desc =
                codegen.getCompositionDescriptor("SchemaWithContentAnyOf");
        Assert.assertNotNull(desc, "SchemaWithContentAnyOf must have a descriptor");
        Oas31CompositionLowering.CompositionBranchDescriptor branch =
                desc.getBranches().get(0);
        Assert.assertFalse(branch.getUnsupportedAssertions().contains("content-encoding"),
                "contentEncoding must no longer be fail-closed");
        Assert.assertTrue(branch.getSupportedAssertions().contains("content-encoding"),
                "contentEncoding must be surfaced as supported (annotation keyword)");
    }

    // ========================================================================
    // OAS 3.1 dialect detection and normative structure gate
    // ========================================================================

    @Test
    public void resolvesPinnedOas31Dialects() {
        // Pinned revision + its OAS alias both map to OAS_31.
        Assert.assertEquals(
                CppBoostBeastClientCodegen.OasDialect.OAS_31,
                CppBoostBeastClientCodegen.resolveEffectiveDialect(
                        "https://spec.openapis.org/oas/3.1/dialect/2024-11-10", null));
        Assert.assertEquals(
                CppBoostBeastClientCodegen.OasDialect.OAS_31,
                CppBoostBeastClientCodegen.resolveEffectiveDialect(
                        "https://spec.openapis.org/oas/3.1/dialect/base", null));
        // Root $schema takes precedence over jsonSchemaDialect.
        Assert.assertEquals(
                CppBoostBeastClientCodegen.OasDialect.DRAFT_2020_12_REC,
                CppBoostBeastClientCodegen.resolveEffectiveDialect(
                        "https://spec.openapis.org/oas/3.1/dialect/2024-11-10",
                        "https://json-schema.org/draft/2020-12/schema"));
        // Unrecognized dialect.
        Assert.assertEquals(
                CppBoostBeastClientCodegen.OasDialect.UNRECOGNIZED,
                CppBoostBeastClientCodegen.resolveEffectiveDialect(
                        "https://example.org/custom-dialect", null));
        // No declaration.
        Assert.assertEquals(
                CppBoostBeastClientCodegen.OasDialect.UNSPECIFIED,
                CppBoostBeastClientCodegen.resolveEffectiveDialect(null, null));
    }

    @Test
    public void documentDialectDefaultsToOas31ForOas31Docs() {
        io.swagger.v3.oas.models.OpenAPI oas31 = new io.swagger.v3.oas.models.OpenAPI();
        oas31.setOpenapi("3.1.0");
        Assert.assertEquals(
                CppBoostBeastClientCodegen.OasDialect.OAS_31,
                CppBoostBeastClientCodegen.resolveDocumentDialect(oas31));

        io.swagger.v3.oas.models.OpenAPI oas30 = new io.swagger.v3.oas.models.OpenAPI();
        oas30.setOpenapi("3.0.3");
        Assert.assertEquals(
                CppBoostBeastClientCodegen.OasDialect.UNSPECIFIED,
                CppBoostBeastClientCodegen.resolveDocumentDialect(oas30));

        io.swagger.v3.oas.models.OpenAPI custom = new io.swagger.v3.oas.models.OpenAPI();
        custom.setOpenapi("3.1.0");
        custom.setJsonSchemaDialect("https://example.org/custom-dialect");
        Assert.assertEquals(
                CppBoostBeastClientCodegen.OasDialect.UNRECOGNIZED,
                CppBoostBeastClientCodegen.resolveDocumentDialect(custom));
    }

    @Test
    public void normativeStructureGateFlagsMissingFields() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();

        // Fully-normative OAS 3.1 document → no diagnostics.
        io.swagger.v3.oas.models.OpenAPI valid = new io.swagger.v3.oas.models.OpenAPI();
        valid.setOpenapi("3.1.0");
        valid.setInfo(new io.swagger.v3.oas.models.info.Info()
                .title("Test").version("1.0.0"));
        valid.setPaths(new io.swagger.v3.oas.models.Paths());
        valid.getPaths().addPathItem("/ping", new io.swagger.v3.oas.models.PathItem());
        Assert.assertTrue(codegen.validateNormativeOas3Structure(valid).isEmpty());

        // No info object → flagged.
        io.swagger.v3.oas.models.OpenAPI noInfo = new io.swagger.v3.oas.models.OpenAPI();
        noInfo.setOpenapi("3.1.0");
        noInfo.setPaths(new io.swagger.v3.oas.models.Paths());
        boolean foundNoInfo = false;
        for (String d : codegen.validateNormativeOas3Structure(noInfo)) {
            if (d.startsWith("missing root `info`")) foundNoInfo = true;
        }
        Assert.assertTrue(foundNoInfo);

        // info without title/version → flagged.
        io.swagger.v3.oas.models.OpenAPI noTitleVersion = new io.swagger.v3.oas.models.OpenAPI();
        noTitleVersion.setOpenapi("3.1.0");
        noTitleVersion.setInfo(new io.swagger.v3.oas.models.info.Info());
        List<String> tv = codegen.validateNormativeOas3Structure(noTitleVersion);
        Assert.assertTrue(tv.stream().anyMatch(d -> d.startsWith("missing `info.title`")));
        Assert.assertTrue(tv.stream().anyMatch(d -> d.startsWith("missing `info.version`")));

        // No paths/components/webhooks → flagged.
        io.swagger.v3.oas.models.OpenAPI empty = new io.swagger.v3.oas.models.OpenAPI();
        empty.setOpenapi("3.1.0");
        empty.setInfo(new io.swagger.v3.oas.models.info.Info()
                .title("T").version("1.0"));
        boolean foundNoContainer = false;
        for (String d : codegen.validateNormativeOas3Structure(empty)) {
            if (d.contains("at least one of `paths`")) foundNoContainer = true;
        }
        Assert.assertTrue(foundNoContainer);
    }

    @Test
    public void dialectPolicyRefusesUnrecognizedDialect() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();

        io.swagger.v3.oas.models.OpenAPI oas31 = new io.swagger.v3.oas.models.OpenAPI();
        oas31.setOpenapi("3.1.0");
        oas31.setInfo(new io.swagger.v3.oas.models.info.Info()
                .title("T").version("1.0"));
        // Recognized/absent dialect → no refusal.
        Assert.assertTrue(codegen.validateDialectPolicy(oas31).isEmpty());

        io.swagger.v3.oas.models.OpenAPI custom = new io.swagger.v3.oas.models.OpenAPI();
        custom.setOpenapi("3.1.0");
        custom.setJsonSchemaDialect("https://example.org/not-known");
        boolean refused = codegen.validateDialectPolicy(custom)
                .stream().anyMatch(d -> d.contains("unrecognized jsonSchemaDialect"));
        Assert.assertTrue(refused);
    }

    // ========================================================================
    // Exhaustive schema-valued-position scanner + honest occurrence ledger
    // ========================================================================

    private static io.swagger.v3.oas.models.OpenAPI openApiWithSchemas(
            String version, Map<String, Schema> schemas) {
        io.swagger.v3.oas.models.OpenAPI openAPI =
                new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi(version);
        io.swagger.v3.oas.models.Components components =
                new io.swagger.v3.oas.models.Components();
        components.setSchemas(schemas);
        openAPI.setComponents(components);
        return openAPI;
    }

    @Test
    public void exhaustiveScannerIndexesNestedSchemaValuedPositions() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        Schema root = new Schema();
        root.setType("object");

        // properties -> array -> items -> object with string length assertion.
        ArraySchema arr = new ArraySchema();
        arr.setMinItems(1);
        Schema itemObj = new ObjectSchema();
        itemObj.setMinLength(2);
        arr.setItems(itemObj);
        Map<String, Schema> props = new HashMap<>();
        props.put("arr", arr);
        root.setProperties(props);
        root.setRequired(Arrays.asList("arr"));

        // Previously-missed schema-valued keywords (silent-skip risks of the scanner).
        Map<String, Schema> patternProps = new HashMap<>();
        patternProps.put("^x-", new StringSchema());
        root.setPatternProperties(patternProps);
        Map<String, Schema> depSchemas = new HashMap<>();
        depSchemas.put("credit_card", new ObjectSchema());
        root.setDependentSchemas(depSchemas);
        root.setMinProperties(1);
        root.setMinContains(1);
        root.setContains(new StringSchema());
        root.setNot(new StringSchema());
        root.setIf(new ObjectSchema());
        root.setThen(new ObjectSchema());
        root.setElse(new ObjectSchema());
        root.setUnevaluatedItems(new StringSchema());
        root.setUnevaluatedProperties(new ObjectSchema());
        root.setContentSchema(new ObjectSchema());

        Map<String, Schema> schemas = new HashMap<>();
        schemas.put("Root", root);
        io.swagger.v3.oas.models.OpenAPI openAPI = openApiWithSchemas("3.1.0", schemas);

        Oas31KeywordScanner.KeywordOccurrenceLedger ledger =
                codegen.scanSchemaKeywordOccurrences(openAPI);

        // Every previously-missed / exhaustive position must be indexed.
        Assert.assertTrue(ledger.hasKeyword("properties"), "properties indexed");
        Assert.assertTrue(ledger.hasKeyword("minItems"), "minItems indexed");
        Assert.assertTrue(ledger.hasKeyword("items"), "items indexed");
        Assert.assertTrue(ledger.hasKeyword("patternProperties"), "patternProperties indexed");
        Assert.assertTrue(ledger.hasKeyword("dependentSchemas"), "dependentSchemas indexed");
        Assert.assertTrue(ledger.hasKeyword("minProperties"), "minProperties indexed");
        Assert.assertTrue(ledger.hasKeyword("minContains"), "minContains indexed");
        Assert.assertTrue(ledger.hasKeyword("contains"), "contains indexed");
        Assert.assertTrue(ledger.hasKeyword("not"), "not indexed");
        Assert.assertTrue(ledger.hasKeyword("if"), "if indexed");
        Assert.assertTrue(ledger.hasKeyword("then"), "then indexed");
        Assert.assertTrue(ledger.hasKeyword("else"), "else indexed");
        Assert.assertTrue(ledger.hasKeyword("unevaluatedItems"), "unevaluatedItems indexed");
        Assert.assertTrue(ledger.hasKeyword("unevaluatedProperties"), "unevaluatedProperties indexed");
        Assert.assertTrue(ledger.hasKeyword("contentSchema"), "contentSchema indexed");
        Assert.assertTrue(ledger.hasKeyword("required"), "required indexed");

        // The scanner must walk nested schema-valued child positions.
        boolean itemChildWalked = ledger.getOccurrences().stream()
                .anyMatch(o -> o.getLocation().contains("/properties/arr/items"));
        Assert.assertTrue(itemChildWalked, "items child schema location must be walked");
        boolean contentChildWalked = ledger.getOccurrences().stream()
                .anyMatch(o -> o.getLocation().contains("/contentSchema"));
        Assert.assertTrue(contentChildWalked, "contentSchema child schema must be walked");
        boolean propPatternWalked = ledger.getOccurrences().stream()
                .anyMatch(o -> o.getLocation().contains("/patternProperties/"));
        Assert.assertTrue(propPatternWalked, "patternProperties child schema must be walked");
    }

    @Test
    public void handledKeywordsAreClassifiedAsEmitted() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        Schema root = new Schema();
        root.setType("object");
        root.setPatternProperties(Collections.singletonMap("^x-", new StringSchema()));
        root.setDependentSchemas(Collections.singletonMap("k", new ObjectSchema()));
        root.setMinContains(1);
        root.setMaxContains(3);
        root.setUnevaluatedItems(new StringSchema());
        root.setMinProperties(1);
        root.setMaxProperties(5);
        root.setNot(new ObjectSchema());
        root.setContentSchema(new ObjectSchema());

        Map<String, Schema> schemas = new HashMap<>();
        schemas.put("Root", root);
        io.swagger.v3.oas.models.OpenAPI openAPI = openApiWithSchemas("3.1.0", schemas);

        Oas31KeywordScanner.KeywordOccurrenceLedger ledger =
                codegen.scanSchemaKeywordOccurrences(openAPI);

        // Every validity-affecting keyword in this fixture has an emitted
        // evaluator path.
        java.util.List<String> previouslyMissed = Arrays.asList(
                "dependentSchemas", "minContains",
                "maxContains", "unevaluatedItems");
        for (String k : previouslyMissed) {
            Assert.assertTrue(ledger.hasKeyword(k),
                    "keyword '" + k + "' must be indexed");
            boolean allEmitted = ledger.forKeyword(k).stream()
                    .allMatch(o -> o.getStatus()
                            == Oas31KeywordScanner.KeywordOccurrenceStatus.EMITTED);
            Assert.assertTrue(allEmitted,
                    "keyword '" + k + "' must be emitted");
        }
        // patternProperties is handled by the generated pattern engine.
        Assert.assertTrue(ledger.hasKeyword("patternProperties"),
                "patternProperties must be indexed");
        boolean patternPropsEmitted = ledger.forKeyword("patternProperties").stream()
                .allMatch(o -> o.getStatus()
                        == Oas31KeywordScanner.KeywordOccurrenceStatus.EMITTED);
        Assert.assertTrue(patternPropsEmitted,
                "patternProperties must be EMITTED (pattern-engine pass)");

        // contentSchema is schema-valued but annotation-only.
        Assert.assertTrue(ledger.hasKeyword("contentSchema"),
                "contentSchema must now be indexed");
        boolean contentIsAnnotation = ledger.forKeyword("contentSchema").stream()
                .allMatch(o -> o.getStatus()
                        == Oas31KeywordScanner.KeywordOccurrenceStatus.ANNOTATION);
        Assert.assertTrue(contentIsAnnotation,
                "contentSchema must be annotation-only");
    }

    @Test
    public void supportedKeywordsAreClassifiedAsEmitted() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        Schema s = new Schema();
        s.setType("string");
        s.addEnumItemObject("a");
        s.setConst("fixed");
        s.setPattern("^a");
        s.setMinLength(1);
        s.setMaxLength(5);
        s.setMinItems(1);
        s.setMaxItems(3);
        s.setUniqueItems(true);
        s.setMultipleOf(java.math.BigDecimal.valueOf(2));
        s.setMinimum(java.math.BigDecimal.valueOf(0));
        s.setMaximum(java.math.BigDecimal.valueOf(10));

        Map<String, Schema> schemas = new HashMap<>();
        schemas.put("S", s);
        io.swagger.v3.oas.models.OpenAPI openAPI = openApiWithSchemas("3.1.0", schemas);

        Oas31KeywordScanner.KeywordOccurrenceLedger ledger =
                codegen.scanSchemaKeywordOccurrences(openAPI);
        for (String supported : Arrays.asList(
                "type", "enum", "const", "pattern", "minLength", "maxLength",
                "minItems", "maxItems", "uniqueItems", "multipleOf",
                "minimum", "maximum")) {
            boolean allEmitted = ledger.forKeyword(supported).stream()
                    .allMatch(o -> o.getStatus()
                            == Oas31KeywordScanner.KeywordOccurrenceStatus.EMITTED);
            Assert.assertTrue(allEmitted,
                    "'".concat(supported).concat("' must be EMITTED (validator present)"));
        }
    }

    @Test
    public void cleanlyPreservesOas30DualPathKeywords() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        // 3.0 dual-path: nullable, boolean exclusiveMin/Max, singular `example`.
        Schema s = new Schema();
        s.setType("number");
        s.setNullable(true);
        s.setExclusiveMinimum(true);
        s.setExclusiveMaximum(true);
        s.setMinimum(java.math.BigDecimal.valueOf(0));
        s.setExample("sample");

        Map<String, Schema> schemas = new HashMap<>();
        schemas.put("S", s);
        io.swagger.v3.oas.models.OpenAPI openAPI = openApiWithSchemas("3.0.4", schemas);

        Oas31KeywordScanner.KeywordOccurrenceLedger ledger =
                codegen.scanSchemaKeywordOccurrences(openAPI);
        Assert.assertTrue(ledger.forKeyword("nullable").stream()
                        .allMatch(o -> o.getStatus()
                                == Oas31KeywordScanner.KeywordOccurrenceStatus.EMITTED),
                "nullable must be treated as handled (3.0 dual-path)");
        Assert.assertTrue(ledger.forKeyword("minimum").stream()
                        .allMatch(o -> o.getStatus()
                                == Oas31KeywordScanner.KeywordOccurrenceStatus.EMITTED),
                "3.0 boolean exclusiveMinimum still emits numeric-range");
        Assert.assertTrue(ledger.forKeyword("example").stream()
                        .allMatch(o -> o.getStatus()
                                == Oas31KeywordScanner.KeywordOccurrenceStatus.ANNOTATION),
                "singular example must be annotation (3.0 dual-path)");
    }

    @Test
    public void failClosedKeywordsSurfaceInLedger() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        Schema root = new Schema();
        root.setType("object");
        root.setMinProperties(1);
        root.setPatternProperties(Collections.singletonMap(".", new StringSchema()));
        root.setNot(new StringSchema());
        root.setContains(new StringSchema());
        root.setContentEncoding("base64");
        root.setContentMediaType("text/plain");
        Schema contentSchema = new Schema();
        contentSchema.setType("string");
        root.setContentSchema(contentSchema);
        io.swagger.v3.oas.models.OpenAPI openAPI =
                openApiWithSchemas("3.1.0", Collections.singletonMap("Root", root));

        java.util.Set<String> fc = codegen.failClosedKeywords(openAPI);
        // Generated+run keywords must NOT be fail-closed (the ledger records
        // them EMITTED; runtime: not.json 40/0/0, min/maxProperties 10/0/0,
        // patternProperties + propertyNames suites green through the GENERATED
        // dispatch, contains family green). contentEncoding/contentMediaType/
        // contentSchema are annotation-only per 2020-12 §8.2.6.
        Assert.assertFalse(fc.contains("minProperties"), "minProperties is emitted (object-property-count)");
        Assert.assertFalse(fc.contains("not"), "not is emitted (shared evaluator)");
        Assert.assertFalse(fc.contains("patternProperties"), "patternProperties is emitted (pattern-engine pass)");
        Assert.assertFalse(fc.contains("propertyNames"), "propertyNames is emitted (pattern-engine pass)");
        Assert.assertFalse(fc.contains("contains"), "contains must be supported (contains-family validators)");
        Assert.assertFalse(fc.contains("minContains"), "minContains is emitted (count bound)");
        Assert.assertFalse(fc.contains("maxContains"), "maxContains is emitted (count bound)");
        Assert.assertFalse(fc.contains("contentEncoding"),
                "contentEncoding must be ANNOTATION, not fail-closed");
        Assert.assertFalse(fc.contains("contentMediaType"),
                "contentMediaType must be ANNOTATION, not fail-closed");
        Assert.assertFalse(fc.contains("contentSchema"),
                "contentSchema must be ANNOTATION, not fail-closed");
        Oas31KeywordScanner.KeywordOccurrenceLedger ledger =
                codegen.scanSchemaKeywordOccurrences(openAPI);
        Assert.assertFalse(ledger.failClosed().contains("contains"),
                "contains must be EMITTED in the ledger");
        Assert.assertFalse(ledger.failClosed().contains("patternProperties"));
        Assert.assertFalse(ledger.failClosed().contains("propertyNames"));
        Assert.assertFalse(ledger.failClosed().contains("minProperties"));
        Assert.assertFalse(ledger.failClosed().contains("not"));
    }

    @Test
    public void preprocessingRejectsUnrecognizedDialect() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        io.swagger.v3.oas.models.OpenAPI openAPI =
                openApiWithSchemas("3.1.0", Collections.emptyMap());
        openAPI.setJsonSchemaDialect("https://example.org/not-supported");

        Assert.assertThrows(IllegalArgumentException.class,
                () -> codegen.preprocessOpenAPI(openAPI));
    }

    @Test
    public void preprocessingRejectsUnnormalizedDynamicReference() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        Schema dynamicRef = new Schema();
        dynamicRef.set$dynamicRef("#node");
        io.swagger.v3.oas.models.OpenAPI openAPI = openApiWithSchemas(
                "3.1.0", Collections.singletonMap("Node", dynamicRef));

        Assert.assertThrows(IllegalArgumentException.class,
                () -> codegen.preprocessOpenAPI(openAPI));
    }

    @Test
    public void exhaustiveScannerIndexesSchemasOutsideComponents() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        OpenAPI openAPI = openApiWithSchemas("3.1.0", Collections.emptyMap());

        StringSchema parameterSchema = new StringSchema();
        parameterSchema.setPattern("^[a-z]+$");
        Parameter parameter = new Parameter();
        parameter.setName("filter");
        parameter.setIn("query");
        parameter.setSchema(parameterSchema);

        ObjectSchema requestSchema = new ObjectSchema();
        requestSchema.setMinProperties(1);
        RequestBody requestBody = new RequestBody();
        requestBody.setContent(new Content().addMediaType("application/json",
                new MediaType().schema(requestSchema)));

        ArraySchema responseSchema = new ArraySchema();
        responseSchema.setMaxItems(3);
        Header responseHeader = new Header();
        StringSchema headerSchema = new StringSchema();
        headerSchema.setMaxLength(8);
        responseHeader.setSchema(headerSchema);
        ApiResponse response = new ApiResponse();
        response.setContent(new Content().addMediaType("application/json",
                new MediaType().schema(responseSchema)));
        response.setHeaders(Collections.singletonMap("X-Token", responseHeader));

        ArraySchema callbackSchema = new ArraySchema();
        callbackSchema.setUniqueItems(true);
        ApiResponse callbackResponse = new ApiResponse();
        callbackResponse.setContent(new Content().addMediaType("application/json",
                new MediaType().schema(callbackSchema)));
        Operation callbackOperation = new Operation();
        callbackOperation.setResponses(new ApiResponses().addApiResponse("200", callbackResponse));
        Callback callback = new Callback();
        callback.put("{$request.body#/callbackUrl}", new PathItem().post(callbackOperation));

        Operation operation = new Operation();
        operation.setParameters(Collections.singletonList(parameter));
        operation.setRequestBody(requestBody);
        operation.setResponses(new ApiResponses().addApiResponse("200", response));
        operation.setCallbacks(Collections.singletonMap("onResult", callback));
        openAPI.setPaths(new Paths().addPathItem("/items", new PathItem().post(operation)));

        Oas31KeywordScanner.KeywordOccurrenceLedger ledger =
                codegen.scanSchemaKeywordOccurrences(openAPI);
        Assert.assertTrue(ledger.forKeyword("pattern").stream().anyMatch(o ->
                o.getLocation().equals("#/paths/~1items/post/parameters/0/schema")));
        Assert.assertTrue(ledger.forKeyword("minProperties").stream().anyMatch(o ->
                o.getLocation().contains("/requestBody/content/application~1json/schema")));
        Assert.assertTrue(ledger.forKeyword("maxItems").stream().anyMatch(o ->
                o.getLocation().contains("/responses/200/content/application~1json/schema")));
        Assert.assertTrue(ledger.forKeyword("maxLength").stream().anyMatch(o ->
                o.getLocation().contains("/responses/200/headers/X-Token/schema")));
        Assert.assertTrue(ledger.forKeyword("uniqueItems").stream().anyMatch(o ->
                o.getLocation().contains("/callbacks/onResult/")));
    }

    @Test
    public void preprocessingRejectsUnnormalizedDynamicReferenceOutsideComponents() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        OpenAPI openAPI = openApiWithSchemas("3.1.0", Collections.emptyMap());
        Schema dynamicRef = new Schema();
        dynamicRef.set$dynamicRef("#node");
        Parameter parameter = new Parameter();
        parameter.setName("filter");
        parameter.setIn("query");
        parameter.setSchema(dynamicRef);
        Operation operation = new Operation();
        operation.setParameters(Collections.singletonList(parameter));
        operation.setResponses(new ApiResponses().addApiResponse("204", new ApiResponse()));
        openAPI.setPaths(new Paths().addPathItem("/items", new PathItem().get(operation)));

        Assert.assertThrows(IllegalArgumentException.class,
                () -> codegen.preprocessOpenAPI(openAPI));
    }

    @Test
    public void nestedCompositionBranchPositionsAreScanned() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        ComposedSchema schema = new ComposedSchema();
        ObjectSchema branch = new ObjectSchema();
        branch.setMinProperties(1);
        schema.addOneOfItem(branch);
        schema.addOneOfItem(new StringSchema());

        Map<String, Schema> schemas = new HashMap<>();
        schemas.put("Composed", schema);
        io.swagger.v3.oas.models.OpenAPI openAPI = openApiWithSchemas("3.0.4", schemas);

        Oas31KeywordScanner.KeywordOccurrenceLedger ledger =
                codegen.scanSchemaKeywordOccurrences(openAPI);
        Assert.assertTrue(ledger.hasKeyword("oneOf"), "oneOf must be indexed");
        boolean branchLocation = ledger.getOccurrences().stream()
                .anyMatch(o -> o.getLocation().contains("/oneOf/0") && o.getKeyword().equals("minProperties"));
        Assert.assertTrue(branchLocation,
                "minProperties on a composition branch must be scanned at its branch location");
    }

    // ======================================================================
    // Object/array structural IR — Java-side focused tests
    // ======================================================================

    @Test
    public void wave2ObjectArrayStructuralKeywordsSurfaceIntoBranchParams() {
        // The branch assertion scan must surface the FULL object/array
        // structural keyword set into validateParams — properties (per-property
        // subschemas), required, additionalProperties tri-state, min/maxProperties,
        // prefixItems (by index), items, min/maxItems, and uniqueItems PRESENCE
        // (true AND false must both be surfaced, never dropped).
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        Map<String, Schema> schemas = new HashMap<>();

        ComposedSchema objectSchema = new ComposedSchema();
        ObjectSchema objBranch = new ObjectSchema();
        objBranch.addProperty("name", new StringSchema());
        objBranch.addProperty("age", new IntegerSchema());
        objBranch.addRequiredItem("name");
        objBranch.addRequiredItem("age");
        objBranch.setAdditionalProperties(Boolean.FALSE);
        objBranch.setMinProperties(1);
        objBranch.setMaxProperties(5);
        objectSchema.addOneOfItem(objBranch);
        schemas.put("ObjectStructural", objectSchema);

        ComposedSchema arraySchema = new ComposedSchema();
        ArraySchema arrayBranch = new ArraySchema();
        java.util.List<Schema> prefix = new java.util.ArrayList<>();
        prefix.add(new StringSchema());
        prefix.add(new IntegerSchema());
        arrayBranch.setPrefixItems(prefix);
        arrayBranch.setItems(new io.swagger.v3.oas.models.media.BooleanSchema());
        arrayBranch.setMinItems(1);
        arrayBranch.setMaxItems(4);
        arrayBranch.setUniqueItems(Boolean.FALSE);
        arraySchema.addOneOfItem(arrayBranch);
        schemas.put("ArrayStructural", arraySchema);

        io.swagger.v3.oas.models.OpenAPI openAPI = openApiWithSchemas("3.1.0", schemas);
        codegen.preprocessOpenAPI(openAPI);

        Oas31CompositionLowering.CompositionDescriptor objDesc =
                codegen.getCompositionDescriptor("ObjectStructural");
        Assert.assertNotNull(objDesc, "ObjectStructural must have a descriptor");
        Map<String, Object> objParams = objDesc.getBranches().get(0).getValidateParams();
        Assert.assertTrue(objParams.get("validation-properties") instanceof Map,
                "validation-properties must carry the per-property schema map");
        Map<?, ?> props = (Map<?, ?>) objParams.get("validation-properties");
        Assert.assertTrue(props.containsKey("name") && props.containsKey("age"),
                "both declared properties must be surfaced");
        Assert.assertNotNull(objParams.get("validation-required"),
                "required list must be surfaced");
        java.util.List<String> reqNames = new java.util.ArrayList<>(
                (java.util.List<String>) objParams.get("validation-required"));
        java.util.Collections.sort(reqNames);
        Assert.assertEquals(reqNames, java.util.Arrays.asList("age", "name"),
                "required list must surface both required members");
        Assert.assertEquals(objParams.get("validation-additional-properties-kind"),
                "reject", "additionalProperties:false must surface as reject");
        Assert.assertEquals(objParams.get("validation-min-properties").toString(),
                "1", "minProperties must be surfaced");
        Assert.assertEquals(objParams.get("validation-max-properties").toString(),
                "5", "maxProperties must be surfaced");

        Oas31CompositionLowering.CompositionDescriptor arrDesc =
                codegen.getCompositionDescriptor("ArrayStructural");
        Assert.assertNotNull(arrDesc, "ArrayStructural must have a descriptor");
        Map<String, Object> arrParams = arrDesc.getBranches().get(0).getValidateParams();
        Assert.assertEquals(((java.util.List<?>) arrParams.get("validation-prefix-items")).size(), 2,
                "prefixItems must be surfaced by index (2 entries)");
        Assert.assertNotNull(arrParams.get("validation-items"),
                "items schema must be surfaced");
        Assert.assertEquals(arrParams.get("validation-min-items").toString(),
                "1", "minItems must be surfaced");
        Assert.assertEquals(arrParams.get("validation-max-items").toString(),
                "4", "maxItems must be surfaced");
        Assert.assertNotNull(arrParams.get("validation-unique-items"),
                "uniqueItems PRESENCE must be surfaced");
        Assert.assertEquals(arrParams.get("validation-unique-items"), Boolean.FALSE,
                "uniqueItems:false must be preserved (no-op emission, never dropped)");
    }

    @Test
    public void emitsWave2ObjectArrayStructuralIr() throws IOException {
        // GENERATED-path guard: the REAL generator must densify the §10 object /
        // array structural keyword set into schema_ir.generated.cpp rows —
        // PropertyBinding node refs, required, additionalProperties tri-state
        // (reject/schema/allowed), min/maxProperties, prefixItems/items node
        // refs, min/maxItems, uniqueItems both forms, enum:[] reject-all, and
        // container-depth EXACT numeric lexemes. The C++ side (compile + run
        // verdicts through the GENERATED validate_<id> dispatch) is owned by the
        // engine gate (oas-compliance/gate-oastructural.sh).
        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-oa").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/oas31-wave2-structural-regression.yaml")
                .setOutputDir(output.getAbsolutePath())
                // The OAS-wrapped doc is fed to the generator without the OAS
                // spec validator, because `$defs` pointer refs are JSON-Schema,
                // not OAS components; mirror that here.
                .setValidateSpec(false);

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path irSource = output.toPath().resolve("model/schema_ir.generated.cpp");
        Path dispatch = output.toPath().resolve("model/schema_validate.generated.cpp");
        Assert.assertTrue(java.nio.file.Files.exists(irSource),
                "schema_ir.generated.cpp must be emitted");
        Assert.assertTrue(java.nio.file.Files.exists(dispatch),
                "schema_validate.generated.cpp must be emitted");

        String ir = java.nio.file.Files.readString(irSource);

        // -- Object structural: properties / required / additionalProperties / counts --
        Assert.assertTrue(ir.contains("n.hasObjectSchema = true;"),
                "object branch must set hasObjectSchema");
        Assert.assertTrue(java.util.regex.Pattern.compile(
                        "b\\.name = \"age\"; b\\.node = \\d+;")
                        .matcher(ir).find(),
                "property-subschema child rows must be referenced by node index");
        Assert.assertTrue(java.util.regex.Pattern.compile(
                        "b\\.name = \"name\"; b\\.node = \\d+;")
                        .matcher(ir).find(),
                "property-subschema child rows must be referenced by node index");
        TestUtils.assertFileContains(irSource,
                "n.required.push_back(\"name\")",
                "n.required.push_back(\"age\")",
                "n.additionalProperties = AdditionalPropertiesKind::reject;",
                "setExact(n.minProperties, n.hasMinProperties, \"1\")",
                "setExact(n.maxProperties, n.hasMaxProperties, \"5\")");
        // additionalProperties schema-form -> child node ref; true-form -> allowed.
        TestUtils.assertFileContains(irSource,
                "n.additionalProperties = AdditionalPropertiesKind::schema;",
                "n.additionalProperties = AdditionalPropertiesKind::allowed;");
        Assert.assertTrue(java.util.regex.Pattern.compile("n\\.additionalSchema = \\d+;")
                        .matcher(ir).find(),
                "schema-form additionalProperties must reference a densified child row");

        String allowedBranch = schemaNodeBlock(ir,
                "#/components/schemas/AddPropsAllowed/oneOf/0");
        Assert.assertTrue(allowedBranch.contains(
                        "n.additionalProperties = AdditionalPropertiesKind::allowed;"),
                "additionalProperties:true must stay on its own extracted branch");
        Assert.assertFalse(allowedBranch.contains(
                        "n.additionalProperties = AdditionalPropertiesKind::schema;"),
                "schema-form additionalProperties must not leak between extracted branches");

        String schemaBranch = schemaNodeBlock(ir,
                "#/components/schemas/AddPropsSchema/oneOf/0");
        Assert.assertTrue(schemaBranch.contains(
                        "n.additionalProperties = AdditionalPropertiesKind::schema;"),
                "schema-form additionalProperties must stay on its own extracted branch");
        Assert.assertTrue(java.util.regex.Pattern.compile("n\\.additionalSchema = \\d+;")
                        .matcher(schemaBranch).find(),
                "schema-form additionalProperties must retain its branch-local child row");

        String dynamicAnchorCarrier = schemaNodeBlockForSourceName(
                ir, "DynamicAnchorObject_component_app1");
        Assert.assertTrue(dynamicAnchorCarrier.contains(
                        "n.additionalProperties = AdditionalPropertiesKind::reject;"),
                "synthetic ref carrier must retain additionalProperties:false");
        Assert.assertTrue(java.util.regex.Pattern.compile(
                        "b\\.name = \"a\"; b\\.node = \\d+;")
                        .matcher(dynamicAnchorCarrier).find(),
                "synthetic ref carrier must retain names declared by properties");

        // -- Array structural: prefixItems / items / min-maxItems / uniqueItems --
        TestUtils.assertFileContains(irSource,
                "n.prefixItems.push_back(",
                "setExact(n.minItems, n.hasMinItems, \"1\")",
                "setExact(n.maxItems, n.hasMaxItems, \"4\")",
                "n.hasUniqueItems = true;");
        Assert.assertTrue(java.util.regex.Pattern.compile("n\\.items = \\d+;")
                        .matcher(ir).find(),
                "items must be emitted as a node ref");
        long prefixCount = java.util.regex.Pattern.compile("n\\.prefixItems\\.push_back").matcher(ir)
                .results().count();
        Assert.assertTrue(prefixCount >= 2, "prefixItems must emit at least 2 indexed child refs");
        // uniqueItems:false must still materialise a dispatch (never BLOCKED-at-emission).
        String dispatchContent = java.nio.file.Files.readString(dispatch);
        TestUtils.assertFileContains(dispatch,
                "validate_UniqueItemsFalse_branch_0",
                "validate_ObjectBranch_branch_0",
                "validate_ArrayBranch_branch_0");

        // -- enum: [] reject-all --
        Assert.assertTrue(ir.contains("n.hasEnumJson = true;"),
                "empty-enum branch must emit the deep store guard");
        Assert.assertTrue(ir.contains("parseExactJson(R\"") && ir.contains("([])"),
                "enum:[] must emit a ZERO-member exact deep enumJson literal");

        // -- Container-depth EXACT numeric lexemes (never a double round-trip) --
        // The nested number lexemes survive VERBATIM (1.0 stays 1.0). Jackson's
        // decimal renderer strips trailing zeros (2.500 -> 2.5), which is
        // EXACT-EQUALITY-equivalent under JSON-Schema number semantics
        // (1 == 1.0 == 1e0); a lossy double round-trip (e.g. 2.5000000000000004)
        // would NOT be equivalent and must never appear.
        Assert.assertTrue(ir.contains("[{\"amount\":1.0,\"tag\":\"x\"},[2.5,3]]"),
                "nested numbers must survive verbatim inside the exact deep literal");
        Assert.assertTrue(ir.contains("n.enumJsonLexemes = std::move(_exact.lexemes);"),
                "deep enum numbers must retain their exact lexeme table");
        Assert.assertFalse(ir.contains("2.5000000000000004"),
                "a lossy double rendering of the nested decimal must never leak in");

        // -- Root-node accounting: main rows 0..M-1 only; component/helper rows
        // appended after M are NOT resource roots. 13 composed components =>
        // last main root index 12. --
        Assert.assertTrue(ir.contains("res.rootNodes.push_back(12);"),
                "13 composed components => last main root index 12");
        Assert.assertFalse(ir.contains("res.rootNodes.push_back(13);"),
                "component/helper rows must not be resource roots");
        Assert.assertTrue(dispatchContent.contains("validate_DefsNestedProperty_branch_0"),
                "DefsNestedProperty must be dispatched");
    }

    @Test
    public void refSiblingsAndDefsRefsEmitResolutionRows() throws IOException {
        // §10.3: (a) $ref + sibling keywords (2020-12) — the ref node must keep
        // its ref applicator AND densify the sibling keyword (minProperties)
        // inline; (b) $defs-scope refs (JSTS hoists into components.schemas)
        // must resolve to a densified <name>_component row; (c) refs to plain
        // extracted components must resolve to their component row.
        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-refsib").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/oas31-wave2-structural-regression.yaml")
                .setOutputDir(output.getAbsolutePath())
                // The OAS-wrapped doc is fed to the generator without the OAS
                // spec validator, because `$defs` pointer refs are JSON-Schema,
                // not OAS components; mirror that here.
                .setValidateSpec(false);

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path irSource = output.toPath().resolve("model/schema_ir.generated.cpp");
        Path dispatch = output.toPath().resolve("model/schema_validate.generated.cpp");
        String ir = java.nio.file.Files.readString(irSource);
        String dispatchContent = java.nio.file.Files.readString(dispatch);

        // (a) $ref + siblings: RefWithSibling node is a ref applicator AND
        // carries the sibling minProperties inline (both apply, 2020-12).
        TestUtils.assertFileContains(irSource,
                "n.applicator = ApplicatorKind::ref;",
                "setExact(n.minProperties, n.hasMinProperties, \"2\")");
        Assert.assertTrue(dispatchContent.contains("validate_RefWithSibling_branch_0"),
                "RefWithSibling must be dispatched");

        // (b) $defs-scope ref: DefsRef resolves to the hoisted hoistedDef
        // component row (densified, enum members a/b).
        Assert.assertTrue(ir.contains("hoistedDef_component"),
                "defs-scope target must be surfaced as a densified component row");
        Assert.assertTrue(dispatchContent.contains("validate_DefsRef_branch_0"),
                "DefsRef must be dispatched");
        Assert.assertTrue(dispatchContent.contains("validate_DefsNestedProperty_branch_0"),
                "DefsNestedProperty must be dispatched");
        // The nested #/$defs/% property child must be bound to a densified row
        // (its ref maps to the hoisted component row, not an inert -1).
        Assert.assertTrue(java.util.regex.Pattern.compile(
                        "b\\.name = \"inner\"; b\\.node = \\d+;")
                        .matcher(ir).find(),
                "nested #/$defs/% ref property must resolve to a real row");

        // (c) ref to a plain extracted component: PlainTarget_component row must
        // be densified (hasObjectSchema + required id) and reachable via the
        // RefToPlain ref node.
        Assert.assertTrue(ir.contains("PlainTarget_component"),
                "plain ref target must be surfaced as a densified component row");
        TestUtils.assertFileContains(irSource,
                "n.hasObjectSchema = true;",
                "n.required.push_back(\"id\");");
        Assert.assertTrue(dispatchContent.contains("validate_RefToPlain_branch_0"),
                "RefToPlain must be dispatched");
    }

    @Test
    public void honorsConfiguredYamlCodePointLimitDuringRawRecovery() throws IOException {
        io.swagger.v3.parser.util.DeserializationUtils.Options yamlOptions =
                io.swagger.v3.parser.util.DeserializationUtils.getOptions();
        synchronized (yamlOptions) {
            int previousLimit = yamlOptions.getMaxYamlCodePoints();
            try {
                yamlOptions.setMaxYamlCodePoints(4 * 1024 * 1024);
                Path root = Files.createTempDirectory("cpp-boost-beast-large-yaml");
                root.toFile().deleteOnExit();
                Path input = root.resolve("large-input.yaml");
                String spec = "# " + "x".repeat(3 * 1024 * 1024) + "\n"
                        + "openapi: 3.1.0\n"
                        + "info:\n"
                        + "  title: Large YAML recovery\n"
                        + "  version: 1.0.0\n"
                        + "paths: {}\n"
                        + "components:\n"
                        + "  schemas:\n"
                        + "    LimitProbe:\n"
                        + "      type: string\n";
                Files.writeString(input, spec);

                Path output = root.resolve("output");
                CodegenConfigurator configurator = new CodegenConfigurator()
                        .setGeneratorName("cpp-boost-beast-client")
                        .setInputSpec(input.toString())
                        .setOutputDir(output.toString());
                List<File> files = new DefaultGenerator()
                        .opts(configurator.toClientOptInput()).generate();
                files.forEach(File::deleteOnExit);

                TestUtils.assertFileExists(
                        output.resolve("model/schema_ir.generated.cpp"));
            } finally {
                yamlOptions.setMaxYamlCodePoints(previousLimit);
            }
        }
    }

    @Test
    public void partitionsLargeSchemaRegistriesIntoCompiledSources() throws IOException {
        Path root = Files.createTempDirectory("cpp-boost-beast-ir-chunks");
        root.toFile().deleteOnExit();
        Path input = root.resolve("large-schema.yaml");
        StringBuilder spec = new StringBuilder();
        spec.append("openapi: 3.1.0\n")
                .append("info:\n")
                .append("  title: Large schema registry\n")
                .append("  version: 1.0.0\n")
                .append("paths: {}\n")
                .append("components:\n")
                .append("  schemas:\n")
                .append("    LargeObject:\n")
                .append("      type: object\n")
                .append("      properties:\n");
        for (int property = 0; property < 513; property++) {
            spec.append("        property").append(property).append(":\n")
                    .append("          type: string\n");
        }
        Files.writeString(input, spec.toString());

        Path output = root.resolve("output");
        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec(input.toString())
                .setOutputDir(output.toString());
        List<File> files = new DefaultGenerator()
                .opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path modelDirectory = output.resolve("model");
        Path coordinator = modelDirectory.resolve("schema_ir.generated.cpp");
        List<Path> chunks;
        try (java.util.stream.Stream<Path> entries = Files.list(modelDirectory)) {
            chunks = entries
                    .filter(path -> path.getFileName().toString()
                            .startsWith("schema_ir.generated.chunk"))
                    .sorted()
                    .collect(java.util.stream.Collectors.toList());
        }
        Assert.assertTrue(chunks.size() > 1,
                "a registry larger than one source partition must emit chunks");

        String coordinatorSource = Files.readString(coordinator);
        java.util.regex.Matcher reserve = java.util.regex.Pattern
                .compile("reg\\.nodes\\.reserve\\((\\d+)\\);")
                .matcher(coordinatorSource);
        Assert.assertTrue(reserve.find(), "coordinator must reserve the complete registry");
        int expectedNodes = Integer.parseInt(reserve.group(1));
        int emittedNodes = 0;
        String cmake = Files.readString(output.resolve("CMakeLists.txt"));
        for (int chunk = 0; chunk < chunks.size(); chunk++) {
            Path source = chunks.get(chunk);
            String content = Files.readString(source);
            int nodesInChunk = countOccurrences(
                    content, "reg.nodes.push_back(std::move(n));");
            Assert.assertTrue(nodesInChunk > 0 && nodesInChunk < expectedNodes,
                    "each generated source must contain a proper node partition");
            emittedNodes += nodesInChunk;
            Assert.assertTrue(content.contains("void appendSchemaRegistryChunk" + chunk),
                    "chunk must expose its registry append function");
            Assert.assertTrue(content.contains("SchemaIndex schemaNodeForChunk" + chunk),
                    "chunk must expose its node lookup function");
            Assert.assertTrue(coordinatorSource.contains(
                            "detail::appendSchemaRegistryChunk" + chunk + "(reg);"),
                    "coordinator must append every emitted chunk");
            Assert.assertTrue(cmake.contains("model/" + source.getFileName()),
                    "CMake target must compile every emitted chunk");
        }
        String firstUnusedChunk = "schema_ir.generated.chunk" + chunks.size() + ".cpp";
        Assert.assertFalse(Files.exists(modelDirectory.resolve(firstUnusedChunk)),
                "generation must not leave empty source partitions");
        Assert.assertFalse(cmake.contains("model/" + firstUnusedChunk),
                "CMake must not reference an unused source partition");
        Assert.assertEquals(emittedNodes, expectedNodes,
                "source partitioning must emit every registry row exactly once");
        Assert.assertFalse(coordinatorSource.contains("reg.nodes.push_back(std::move(n));"),
                "the coordinator must not retain the monolithic initializer body");
    }

    private static String schemaNodeBlock(String ir, String schemaPath) {
        String marker = "n.schemaPath = \"" + schemaPath + "\";";
        int pathIndex = ir.indexOf(marker);
        Assert.assertTrue(pathIndex >= 0, "missing generated schema row for " + schemaPath);
        int start = ir.lastIndexOf("{ // node ", pathIndex);
        int end = ir.indexOf("reg.nodes.push_back(std::move(n));", pathIndex);
        Assert.assertTrue(start >= 0 && end > pathIndex,
                "malformed generated schema row for " + schemaPath);
        return ir.substring(start, end);
    }

    private static String schemaNodeBlockForSourceName(String ir, String sourceName) {
        String marker = "n.sourceName = \"" + sourceName + "\";";
        int sourceIndex = ir.indexOf(marker);
        Assert.assertTrue(sourceIndex >= 0, "missing generated schema row for " + sourceName);
        int start = ir.lastIndexOf("{ // node ", sourceIndex);
        int end = ir.indexOf("reg.nodes.push_back(std::move(n));", sourceIndex);
        Assert.assertTrue(start >= 0 && end > sourceIndex,
                "malformed generated schema row for " + sourceName);
        return ir.substring(start, end);
    }

    private static int countOccurrences(String text, String needle) {
        int count = 0;
        int offset = 0;
        while ((offset = text.indexOf(needle, offset)) >= 0) {
            count++;
            offset += needle.length();
        }
        return count;
    }


}