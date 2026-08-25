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

public class Oas31SchemaIrIntegrationTest extends Oas31IrTestSupport {
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
        Assert.assertTrue(java.nio.file.Files.exists(irSource),
                "schema_ir.generated.cpp must be emitted");
        Assert.assertFalse(java.nio.file.Files.exists(
                        output.toPath().resolve("model/schema_validate.generated.cpp")),
                "obsolete schema_validate.generated.cpp must not be emitted");

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
        // uniqueItems:false still materializes a registry node (never BLOCKED-at-emission).
        // Every branch is directly addressable through the registry.
        TestUtils.assertFileContains(irSource,
                "if (id == \"UniqueItemsFalse_branch_0\")",
                "if (id == \"ObjectBranch_branch_0\")",
                "if (id == \"ArrayBranch_branch_0\")");

        // -- enum: [] reject-all --
        Assert.assertTrue(ir.contains("n.hasEnumJson = true;"),
                "empty-enum branch must emit the deep store guard");
        Assert.assertTrue(ir.contains("parseExactJson(R\"") && ir.contains("([])"),
                "enum:[] must emit a ZERO-member exact deep enumJson literal");

        // -- Container-depth EXACT numeric lexemes (never a double round-trip) --
        // Raw recovery preserves the original nested number spellings, including
        // insignificant trailing zeroes. Although JSON Schema numeric equality
        // treats 2.500 and 2.5 alike, retaining 2.500 proves that no lossy
        // floating-point round-trip occurred.
        Assert.assertTrue(ir.contains("[{\"amount\":1.0,\"tag\":\"x\"},[2.500,3]]"),
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
        TestUtils.assertFileContains(irSource,
                "if (id == \"DefsNestedProperty_branch_0\")");
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
        String ir = java.nio.file.Files.readString(irSource);

        // (a) $ref + siblings: RefWithSibling node is a ref applicator AND
        // carries the sibling minProperties inline (both apply, 2020-12).
        TestUtils.assertFileContains(irSource,
                "n.applicator = ApplicatorKind::ref;",
                "setExact(n.minProperties, n.hasMinProperties, \"2\")");
        TestUtils.assertFileContains(irSource,
                "if (id == \"RefWithSibling_branch_0\")");

        // (b) $defs-scope ref: DefsRef resolves to the hoisted hoistedDef
        // component row (densified, enum members a/b).
        Assert.assertTrue(ir.contains("hoistedDef_component"),
                "defs-scope target must be surfaced as a densified component row");
        TestUtils.assertFileContains(irSource,
                "if (id == \"DefsRef_branch_0\")",
                "if (id == \"DefsNestedProperty_branch_0\")");
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
        TestUtils.assertFileContains(irSource,
                "if (id == \"RefToPlain_branch_0\")");
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
    public void preservesNullLiteralsAndDisambiguatesComponentIds() throws IOException {
        Path root = Files.createTempDirectory("cpp-boost-beast-raw-literals");
        root.toFile().deleteOnExit();
        Path input = root.resolve("raw-literals.yaml");
        String spec = "openapi: 3.1.0\n"
                + "info:\n"
                + "  title: Raw literal recovery\n"
                + "  version: 1.0.0\n"
                + "paths: {}\n"
                + "components:\n"
                + "  schemas:\n"
                + "    NullConst:\n"
                + "      oneOf:\n"
                + "        - const: null\n"
                + "    NullAnnotations:\n"
                + "      type: object\n"
                + "      default: null\n"
                + "      const: null\n"
                + "      examples: []\n"
                + "    Item:\n"
                + "      type: object\n"
                + "      properties:\n"
                + "        id:\n"
                + "          type: string\n"
                + "    Container:\n"
                + "      type: object\n"
                + "      properties:\n"
                + "        entries:\n"
                + "          type: array\n"
                + "          items:\n"
                + "            $ref: '#/components/schemas/Item'\n"
                + "    schema-name:\n"
                + "      type: string\n"
                + "    schema.name:\n"
                + "      type: integer\n";
        Files.writeString(input, spec);

        Path output = root.resolve("output");
        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec(input.toString())
                .setOutputDir(output.toString());
        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        Path irSource = output.resolve("model/schema_ir.generated.cpp");
        String ir = Files.readString(irSource);
        String nullConst = schemaNodeBlock(ir, "#/components/schemas/NullConst/oneOf/0");
        Assert.assertTrue(nullConst.contains("n.hasConst = true;")
                        && nullConst.contains("null"),
                "const: null must remain an exact deep const constraint");
        String nullAnnotations = schemaNodeBlock(ir, "#/components/schemas/NullAnnotations");
        Assert.assertTrue(nullAnnotations.contains("n.annDefaultJson = \"null\";"),
                "default: null must remain an annotation");
        Assert.assertTrue(nullAnnotations.contains("n.annExamplesJson = \"[]\";"),
                "examples: [] must remain an annotation");
        Assert.assertTrue(ir.contains("schema_name_component_1")
                        && ir.contains("schema_name_component_2"),
                "component names that sanitize identically must receive distinct IDs");

        Path containerHeader = output.resolve("model/Container.h");
        TestUtils.assertFileContains(containerHeader, "std::vector<Item>");
        TestUtils.assertFileNotContains(containerHeader, "std::shared_ptr<Item>");
    }

    @Test
    public void typeErasedOneOfUsesBranchSchemaValidators() throws IOException {
        Path root = Files.createTempDirectory("cpp-boost-beast-type-erased-oneof");
        root.toFile().deleteOnExit();
        Path input = root.resolve("type-erased-oneof.yaml");
        String spec = "openapi: 3.1.0\n"
                + "info:\n"
                + "  title: Type-erased oneOf\n"
                + "  version: 1.0.0\n"
                + "paths: {}\n"
                + "components:\n"
                + "  schemas:\n"
                + "    ConstrainedStringUnion:\n"
                + "      oneOf:\n"
                + "        - minLength: 4\n";
        Files.writeString(input, spec);

        Path output = root.resolve("output");
        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec(input.toString())
                .setOutputDir(output.toString());
        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        Path header = output.resolve("model/ConstrainedStringUnion.h");
        String headerContent = Files.readString(header);
        Assert.assertTrue(headerContent.contains(
                        "using ConstrainedStringUnion = boost::json::value;"),
                "type-erased oneOf must use a JSON value alias; header: " + headerContent);
        Path source = output.resolve("model/ConstrainedStringUnion.cpp");
        TestUtils.assertFileContains(source,
                "schemaNodeFor(\"ConstrainedStringUnion_branch_0\")",
                "sharedSchemaEvaluator().validate(");
        Assert.assertFalse(Files.readString(source).contains("branchMatches = true;"),
                "type-erased oneOf must not degrade constrained schemas to broad type checks");

        String ir = Files.readString(output.resolve("model/schema_ir.generated.cpp"));
        String constrainedBranch = schemaNodeBlock(
                ir, "#/components/schemas/ConstrainedStringUnion/oneOf/0");
        Assert.assertTrue(constrainedBranch.contains(
                        "setExact(n.minLength, n.hasMinLength, \"4\")"),
                "the branch validator must retain the semantic minLength assertion");
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
            int nodesInChunk = CppBoostBeastTestSupport.countOccurrences(
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

}
