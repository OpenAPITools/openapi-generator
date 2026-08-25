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

public class Oas31CompositionComplianceTest extends Oas31IrTestSupport {
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
    public void propertiesOnOneOfBranchAreEmitted() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI = new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.0.4");
        io.swagger.v3.oas.models.Components components = new io.swagger.v3.oas.models.Components();
        Map<String, Schema> schemas = new HashMap<>();

        ComposedSchema schema = new ComposedSchema();
        ObjectSchema objBranch = new ObjectSchema();
        objBranch.addProperties("name", new StringSchema());
        schema.addOneOfItem(objBranch);
        schemas.put("SchemaWithProperties", schema);
        components.setSchemas(schemas);
        openAPI.setComponents(components);

        codegen.preprocessOpenAPI(openAPI);

        Oas31CompositionLowering.CompositionDescriptor desc =
                codegen.getCompositionDescriptor("SchemaWithProperties");
        Assert.assertNotNull(desc, "SchemaWithProperties must have a descriptor");
        Assert.assertEquals(desc.getBranches().size(), 1);
        Oas31CompositionLowering.CompositionBranchDescriptor branch = desc.getBranches().get(0);
        Assert.assertTrue(branch.getSupportedAssertions().contains("object-properties"),
                "Object branch properties must be emitted to the schema IR");
        Assert.assertFalse(branch.getUnsupportedAssertions().contains("properties"),
                "Object branch properties must not be fail-closed");
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
}
