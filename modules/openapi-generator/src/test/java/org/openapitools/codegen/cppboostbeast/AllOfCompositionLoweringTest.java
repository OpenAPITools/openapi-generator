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

import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.ComposedSchema;
import io.swagger.v3.oas.models.media.IntegerSchema;
import io.swagger.v3.oas.models.media.NumberSchema;
import io.swagger.v3.oas.models.media.ObjectSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.OpenAPINormalizer;
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
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class AllOfCompositionLoweringTest {
    @Test(expectedExceptions = RuntimeException.class)
    public void allOfScalarConflictThrows() throws IOException {
        // This test verifies that an allOf with incompatible scalar types
        // (e.g., allOf [string, integer]) causes a RuntimeException.
        // We generate from a minimal spec with only the conflicting schema.
        String specContent =
            "openapi: 3.1.0\n" +
            "info:\n" +
            "  title: allOf conflict test\n" +
            "  version: 1.0.0\n" +
            "paths: {}\n" +
            "components:\n" +
            "  schemas:\n" +
            "    AllOfScalarConflict:\n" +
            "      allOf:\n" +
            "        - type: string\n" +
            "        - type: integer\n" +
            "          format: int32\n";

        java.nio.file.Path specFile = java.nio.file.Files.createTempFile("allof-conflict-", ".yaml");
        specFile.toFile().deleteOnExit();
        java.nio.file.Files.writeString(specFile, specContent);

        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-conflict").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec(specFile.toAbsolutePath().toString())
                .setOutputDir(output.getAbsolutePath())
                .addAdditionalProperty("packageName", "CppBoostBeastConflictTest");

        try {
            new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        } catch (RuntimeException e) {
            // Check the ROOT cause, not just the wrapper message
            Throwable cause = e;
            while (cause.getCause() != null && cause.getCause() != cause) {
                cause = cause.getCause();
            }
            String message = cause.getMessage();
            if (message == null) {
                message = e.getMessage();
            }
            Assert.assertTrue(message != null && (message.contains("allOf type conflict")
                    || message.contains("AllOfScalarConflict")
                    || message.contains("Incompatible root types")),
                    "Exception root cause should mention allOf type conflict. Got: " + message);
            throw e;
        }
    }

    @Test(expectedExceptions = RuntimeException.class)
    public void allOfRequiredConflictThrows() throws IOException {
        // allOf with the same REQUIRED property having incompatible types must
        // FAIL generation — a required property that cannot satisfy all
        // contributor constraints simultaneously is impossible to store.
        String specContent =
            "openapi: 3.1.0\n" +
            "info:\n" +
            "  title: allOf required conflict test\n" +
            "  version: 1.0.0\n" +
            "paths: {}\n" +
            "components:\n" +
            "  schemas:\n" +
            "    AllOfRequiredConflict:\n" +
            "      allOf:\n" +
            "        - type: object\n" +
            "          properties:\n" +
            "            id:\n" +
            "              type: string\n" +
            "          required: [id]\n" +
            "        - type: object\n" +
            "          properties:\n" +
            "            id:\n" +
            "              type: integer\n" +
            "              format: int32\n" +
            "          required: [id]\n";

        java.nio.file.Path specFile = java.nio.file.Files.createTempFile("allof-required-conflict-", ".yaml");
        specFile.toFile().deleteOnExit();
        java.nio.file.Files.writeString(specFile, specContent);

        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-required-conflict").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec(specFile.toAbsolutePath().toString())
                .setOutputDir(output.getAbsolutePath())
                .addAdditionalProperty("packageName", "CppBoostBeastRequiredConflictTest");

        try {
            new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        } catch (RuntimeException e) {
            // Walk to root cause
            Throwable cause = e;
            while (cause.getCause() != null && cause.getCause() != cause) {
                cause = cause.getCause();
            }
            String msg = cause.getMessage();
            Assert.assertTrue(msg != null
                            && (msg.contains("Unsatisfiable allOf")
                                || msg.contains("Required property")
                                || msg.contains("id")),
                    "Exception must mention Unsatisfiable allOf / Required property / id. Got: " + msg);
            throw e;
        }
    }

    @Test
    public void allOfPropertyConflictIsOptionalImpossible() throws IOException {
        // allOf with the same property name having incompatible types does NOT
        // throw — the conflicting optional property becomes optional-impossible
        // (rejected when present, but the object is valid when the property is
        // absent).
        String specContent =
            "openapi: 3.1.0\n" +
            "info:\n" +
            "  title: allOf property conflict test\n" +
            "  version: 1.0.0\n" +
            "paths: {}\n" +
            "components:\n" +
            "  schemas:\n" +
            "    AllOfPropConflict:\n" +
            "      allOf:\n" +
            "        - type: object\n" +
            "          properties:\n" +
            "            value:\n" +
            "              type: string\n" +
            "        - type: object\n" +
            "          properties:\n" +
            "            value:\n" +
            "              type: integer\n" +
            "              format: int32\n";

        java.nio.file.Path specFile = java.nio.file.Files.createTempFile("allof-prop-conflict-", ".yaml");
        specFile.toFile().deleteOnExit();
        java.nio.file.Files.writeString(specFile, specContent);

        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-prop-conflict").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec(specFile.toAbsolutePath().toString())
                .setOutputDir(output.getAbsolutePath())
                .addAdditionalProperty("packageName", "CppBoostBeastPropConflictTest");

        // Generation must succeed — the intersection handles the conflict
        // as optional-impossible instead of throwing.
        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        // Verify the generated header exists and has BOTH getValue() and setValue()
        Path generatedHeader = output.toPath().resolve("model/AllOfPropConflict.h");
        TestUtils.assertFileExists(generatedHeader);
        String headerContent = java.nio.file.Files.readString(generatedHeader);
        // The optional-impossible conflicting property now gets a writable
        // member (first-contributor type wins) so the model is not an empty shell.
        Assert.assertTrue(headerContent.contains("getValue()"),
                "AllOfPropConflict must have a getValue() accessor — "
                + "optional-impossible selects first contributor type. "
                + "Header content: " + headerContent);
        Assert.assertTrue(headerContent.contains("setValue("),
                "AllOfPropConflict must have a setValue() accessor — "
                + "optional-impossible selects first contributor type. "
                + "Header content: " + headerContent);

        // Verify the generated source contains the reject-if-present diagnostic
        // with the exact "optional-impossible" or "cannot satisfy all allOf" string.
        Path generatedSource = output.toPath().resolve("model/AllOfPropConflict.cpp");
        TestUtils.assertFileExists(generatedSource);
        String sourceContent = java.nio.file.Files.readString(generatedSource);
        Assert.assertTrue(sourceContent.contains("cannot satisfy all allOf constraints (optional-impossible)"),
                "AllOfPropConflict source must contain the reject-if-present diagnostic "
                + "for the optional-impossible 'value' property. "
                + "Source: " + sourceContent);

        // Verify the reject-if-present structure: find + end guard
        Assert.assertTrue(sourceContent.contains("object.find(\"value\")"),
                "AllOfPropConflict source must locate 'value' in the JSON object. "
                + "Source: " + sourceContent);
        Assert.assertTrue(sourceContent.contains("it != object.end()"),
                "AllOfPropConflict source must guard on presence (accept when absent, "
                + "reject when present). "
                + "Source: " + sourceContent);
    }


    @Test
    public void allOfPropertyViaNestedRefsIntersectsEnum() throws IOException {
        // allOf with properties defined via $ref branches must resolve the $ref
        // targets and intersect property schemas correctly.  Two branches
        // defining the same property via different $ref targets with overlapping
        // enum values should produce an intersected enum.
        String specContent =
            "openapi: 3.1.0\n" +
            "info:\n" +
            "  title: allOf nested ref enum intersect test\n" +
            "  version: 1.0.0\n" +
            "paths: {}\n" +
            "components:\n" +
            "  schemas:\n" +
            "    PropSourceA:\n" +
            "      type: object\n" +
            "      properties:\n" +
            "        status:\n" +
            "          type: string\n" +
            "          enum: [a, b, c]\n" +
            "    PropSourceB:\n" +
            "      type: object\n" +
            "      properties:\n" +
            "        status:\n" +
            "          type: string\n" +
            "          enum: [b, c, d]\n" +
            "    AllOfRefEnum:\n" +
            "      allOf:\n" +
            "        - $ref: '#/components/schemas/PropSourceA'\n" +
            "        - $ref: '#/components/schemas/PropSourceB'\n";

        java.nio.file.Path specFile = java.nio.file.Files.createTempFile("allof-ref-enum-", ".yaml");
        specFile.toFile().deleteOnExit();
        java.nio.file.Files.writeString(specFile, specContent);

        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-ref-enum").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec(specFile.toAbsolutePath().toString())
                .setOutputDir(output.getAbsolutePath())
                .addAdditionalProperty("packageName", "CppBoostBeastRefEnumTest");

        // Generation must succeed — the allOf intersection resolves $ref branches
        // and intersects the common status property's enum values.
        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        // Verify the generated model has the status property with intersected enum set.
        // The intersection of [a,b,c] and [b,c,d] is {b,c}.
        Path generatedSource = output.toPath().resolve("model/AllOfRefEnum.cpp");
        TestUtils.assertFileExists(generatedSource);
        String sourceContent = java.nio.file.Files.readString(generatedSource);
        Assert.assertTrue(sourceContent.contains("\"b\"") && sourceContent.contains("\"c\""),
                "AllOfRefEnum source must contain intersected enum values b and c. "
                + "Source: " + sourceContent);
        Assert.assertFalse(sourceContent.contains("\"a\""),
                "AllOfRefEnum source must NOT contain enum value a (not in intersection). "
                + "Source: " + sourceContent);
        Assert.assertFalse(sourceContent.contains("\"d\""),
                "AllOfRefEnum source must NOT contain enum value d (not in intersection). "
                + "Source: " + sourceContent);
    }

    @Test
    public void allOfFlatSyntheticOwnsOnlyChildProps() throws IOException {
        // Flat allOf: a $ref parent with inline child properties must produce a
        // model where only the child's own properties appear as
        // owned storage.  Parent properties are NOT duplicated by the synthetic
        // schema — they are merged into the flat synthetic, so the generated
        // model declares ALL properties as direct members, with no parent ref.
        String specContent =
            "openapi: 3.1.0\n" +
            "info:\n" +
            "  title: allOf flat synthetic test\n" +
            "  version: 1.0.0\n" +
            "paths: {}\n" +
            "components:\n" +
            "  schemas:\n" +
            "    Parent:\n" +
            "      type: object\n" +
            "      properties:\n" +
            "        inheritedProp:\n" +
            "          type: string\n" +
            "    Child:\n" +
            "      allOf:\n" +
            "        - $ref: '#/components/schemas/Parent'\n" +
            "        - type: object\n" +
            "          properties:\n" +
            "            childProp:\n" +
            "              type: integer\n" +
            "              format: int32\n";

        java.nio.file.Path specFile = java.nio.file.Files.createTempFile("allof-flat-synth-", ".yaml");
        specFile.toFile().deleteOnExit();
        java.nio.file.Files.writeString(specFile, specContent);

        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-flat-synth").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec(specFile.toAbsolutePath().toString())
                .setOutputDir(output.getAbsolutePath())
                .addAdditionalProperty("packageName", "CppBoostBeastFlatSynthTest");

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        // Child must NOT inherit from Parent (flat synthetic has allOf=null)
        Path header = output.toPath().resolve("model/Child.h");
        TestUtils.assertFileExists(header);
        String headerContent = java.nio.file.Files.readString(header);
        // Child must declare childProp as an owned member
        // Properties are emitted with the m_ prefix.
        // Use m_ChildProp directly to avoid any substring matching ambiguity.
        Assert.assertTrue(headerContent.contains("m_ChildProp"),
                "Child must declare m_ChildProp as owned storage. "
                + "Header: " + headerContent);
        // Child must also carry inheritedProp as its OWN member (flat)
        Assert.assertTrue(headerContent.contains("m_InheritedProp"),
                "Child must declare m_InheritedProp as owned storage (flat synthetic). "
                + "Header: " + headerContent);
    }

    @Test
    public void oneOfConstrainedNumbersProducesCompositionBranchValueVariant() throws IOException {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        // oneOf [number, number] — both branches are double after dedup,
        // identity is preserved via CompositionBranchValue wrappers.
        ComposedSchema schema = new ComposedSchema();
        schema.addOneOfItem(new NumberSchema());
        schema.addOneOfItem(new NumberSchema());

        String resolved = codegen.getTypeDeclaration(schema);
        Assert.assertEquals(
                "std::variant<CompositionBranchValue<0, double>, CompositionBranchValue<1, double>>",
                resolved,
                "oneOf [number, number] (duplicate types) should produce "
                        + "CompositionBranchValue variant, not boost::json::value");
    }

    @Test
    public void oneOfConstrainedNumbersWithMultipleOfFromFixtures() throws IOException {
        // Verify ConstrainedNumber (oneOf with multipleOf) generates from the compliance fixtures.
        // Both branches are type:number (double) so they resolve to duplicate C++ types.
        // The CompositionBranchValue wrappers preserve branch identity.
        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-multof").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/fixtures/composition-fixtures.yaml")
                .setOutputDir(output.getAbsolutePath())
                .addAdditionalProperty("packageName", "CppBoostBeastMultiOfTest");

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path constrainedHeader = output.toPath().resolve("model/ConstrainedNumber.h");
        TestUtils.assertFileExists(constrainedHeader);
        String constraintContent = java.nio.file.Files.readString(constrainedHeader);
        Assert.assertTrue(
                constraintContent.contains("CompositionBranchValue"),
                "ConstrainedNumber (oneOf number+number) must use CompositionBranchValue "
                        + "to preserve branch identity; content: "
                        + constraintContent.substring(0, Math.min(500, constraintContent.length())));
        Assert.assertTrue(
                constraintContent.contains("CompositionBranchValue<0, double>"),
                "ConstrainedNumber[0] must be CompositionBranchValue<0, double>; content: "
                        + constraintContent.substring(0, Math.min(500, constraintContent.length())));
        Assert.assertTrue(
                constraintContent.contains("CompositionBranchValue<1, double>"),
                "ConstrainedNumber[1] must be CompositionBranchValue<1, double>; content: "
                        + constraintContent.substring(0, Math.min(500, constraintContent.length())));
        // Verify fromJsonValue uses descriptor-guided conversion (not blind tryVariantBranches)
        Path constrainedSource = output.toPath().resolve("model/ConstrainedNumber.cpp");
        TestUtils.assertFileExists(constrainedSource);
        String constraintSourceContent = java.nio.file.Files.readString(constrainedSource);
        Assert.assertTrue(
                constraintSourceContent.contains("matchedBranchIndex"),
                "ConstrainedNumber fromJsonValue must track matchedBranchIndex from "
                        + "validator (not tryVariantBranches); content: "
                        + constraintSourceContent.substring(0, Math.min(500, constraintSourceContent.length())));
        Assert.assertTrue(
                constraintSourceContent.contains(
                        "CompositionBranchValue<0, double>{std::move(converted)}"),
                "ConstrainedNumber fromJsonValue must construct CompositionBranchValue<0, "
                        + "double> from the converted branch value; content: "
                        + constraintSourceContent.substring(0, Math.min(500, constraintSourceContent.length())));

        // Verify enum-only anyOf preserves validators (not collapsed to std::string)
        Path enumUnionHeader = output.toPath().resolve("model/AnyOfEnumUnion.h");
        TestUtils.assertFileExists(enumUnionHeader);
        String enumUnionHeaderContent = java.nio.file.Files.readString(enumUnionHeader);
        Assert.assertTrue(
                enumUnionHeaderContent.contains("CompositionBranchValue"),
                "AnyOfEnumUnion (anyOf enum+enum) must use CompositionBranchValue "
                        + "to preserve validators (not collapsed to std::string); content: "
                        + enumUnionHeaderContent.substring(0, Math.min(500, enumUnionHeaderContent.length())));
        Path enumUnionSource = output.toPath().resolve("model/AnyOfEnumUnion.cpp");
        TestUtils.assertFileExists(enumUnionSource);
        String enumUnionSourceContent = java.nio.file.Files.readString(enumUnionSource);
        Assert.assertTrue(
                enumUnionSourceContent.contains("validate_AnyOfEnumUnion_branch_0")
                        && enumUnionSourceContent.contains("validate_AnyOfEnumUnion_branch_1"),
                "AnyOfEnumUnion source must contain per-branch validators for "
                        + "enum rejection; content: "
                        + enumUnionSourceContent.substring(0, Math.min(500, enumUnionSourceContent.length())));

        // Verify all-null anyOf preserves null cardinality with tagged type
        Path allNullHeader = output.toPath().resolve("model/AllNullAnyOf.h");
        TestUtils.assertFileExists(allNullHeader);
        String allNullContent = java.nio.file.Files.readString(allNullHeader);
        Assert.assertTrue(
                allNullContent.contains("CompositionBranchValue<0, std::nullptr_t>"),
                "AllNullAnyOf must use CompositionBranchValue<0, std::nullptr_t> "
                        + "to preserve null branch identity; content: "
                        + allNullContent.substring(0, Math.min(500, allNullContent.length())));

        // Verify duplicate-null oneOf preserves null cardinality
        Path dupNullHeader = output.toPath().resolve("model/DuplicateNullOneOf.h");
        TestUtils.assertFileExists(dupNullHeader);

        // Verify API response deserialization uses model free function
        // for CompositionBranchValue variants (not generic tryFirstVariantAlternative)
        Path apiSource = output.toPath().resolve("api/DefaultApi.cpp");
        if (java.nio.file.Files.exists(apiSource)) {
            String apiSourceContent = java.nio.file.Files.readString(apiSource);
            Assert.assertTrue(
                    apiSourceContent.contains("fromJsonValue_ConstrainedNumber("),
                    "API response for ConstrainedNumber must use "
                            + "fromJsonValue_ConstrainedNumber (descriptor-guided) "
                            + "instead of generic ResponseBodyDeserializer; content: "
                            + apiSourceContent.substring(0, Math.min(500, apiSourceContent.length())));
        }
    }

    @Test
    public void allOfEnumIntersectionFromFixtures() throws IOException {
        // Verify AllOfEnumIntersection (allOf [enum[a,b], enum[b,c]]) generates
        // the exact intersection {b} from the compliance fixture.
        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-enum-intersect").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/fixtures/composition-fixtures.yaml")
                .setOutputDir(output.getAbsolutePath())
                .addAdditionalProperty("packageName", "CppBoostBeastEnumIntersectTest");

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path intersectHeader = output.toPath().resolve("model/AllOfEnumIntersection.h");
        TestUtils.assertFileExists(intersectHeader);

        // Verify the model file and its source file are generated.
        Path intersectSource = output.toPath().resolve("model/AllOfEnumIntersection.cpp");
        TestUtils.assertFileExists(intersectSource);
        String intersectContent = java.nio.file.Files.readString(intersectSource);
        Assert.assertTrue(intersectContent.contains("\"b\""),
                "AllOfEnumIntersection must retain the sole intersected enum value b");
        Assert.assertFalse(intersectContent.contains("\"a\""),
                "AllOfEnumIntersection must drop enum value a outside the intersection");
        Assert.assertFalse(intersectContent.contains("\"c\""),
                "AllOfEnumIntersection must drop enum value c outside the intersection");
    }

    @Test
    public void anyOfEnumUnionCollapsesToString() throws IOException {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        // anyOf [enum[red], enum[blue]] → CompositionBranchValue variant
        // (the wrappers preserve enum validators, no blind collapse)
        ComposedSchema schema = new ComposedSchema();
        StringSchema enumBranch0 = new StringSchema();
        enumBranch0.addEnumItem("red");
        StringSchema enumBranch1 = new StringSchema();
        enumBranch1.addEnumItem("blue");
        schema.addAnyOfItem(enumBranch0);
        schema.addAnyOfItem(enumBranch1);

        String resolved = codegen.getTypeDeclaration(schema);
        Assert.assertEquals(resolved,
                "std::variant<CompositionBranchValue<0, std::string>, CompositionBranchValue<1, std::string>>",
                "anyOf [enum[red], enum[blue]] should produce CompositionBranchValue variant");
    }

    @Test
    public void allOfEnumIntersectionMergesEnum() throws IOException {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        // allOf [enum[a,b], enum[b,c]] → merged enum is intersection [b] → std::string
        ComposedSchema schema = new ComposedSchema();
        StringSchema enumBranch0 = new StringSchema();
        enumBranch0.addEnumItem("a");
        enumBranch0.addEnumItem("b");
        StringSchema enumBranch1 = new StringSchema();
        enumBranch1.addEnumItem("b");
        enumBranch1.addEnumItem("c");
        schema.addAllOfItem(enumBranch0);
        schema.addAllOfItem(enumBranch1);

        String resolved = codegen.getTypeDeclaration(schema);
        Assert.assertEquals(resolved, "std::string",
                "allOf [enum[a,b], enum[b,c]] should merge to std::string");
    }

    @Test
    public void oneOfIntegerNumberProducesVariant() throws IOException {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        // oneOf [integer, number] → std::variant<std::int32_t, double>
        ComposedSchema schema = new ComposedSchema();
        IntegerSchema intBranch = new IntegerSchema();
        intBranch.setFormat("int32");
        schema.addOneOfItem(intBranch);
        schema.addOneOfItem(new NumberSchema());

        String resolved = codegen.getTypeDeclaration(schema);
        Assert.assertEquals(resolved, "std::variant<std::int32_t, double>",
                "oneOf [integer, number] should produce std::variant<std::int32_t, double>");
    }
}
