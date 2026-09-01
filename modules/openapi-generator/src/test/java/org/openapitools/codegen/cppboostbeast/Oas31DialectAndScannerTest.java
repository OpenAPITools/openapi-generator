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

public class Oas31DialectAndScannerTest extends Oas31IrTestSupport {
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

        io.swagger.v3.oas.models.OpenAPI oas310 = new io.swagger.v3.oas.models.OpenAPI();
        oas310.setOpenapi("3.10.0");
        Assert.assertEquals(
                CppBoostBeastClientCodegen.OasDialect.UNSPECIFIED,
                CppBoostBeastClientCodegen.resolveDocumentDialect(oas310),
                "3.10.x must not be treated as OAS 3.1");
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
    public void scannerRejectsSchemasBeyondMaximumNesting() {
        Schema root = new ObjectSchema();
        Schema current = root;
        for (int depth = 0; depth <= 1024; depth++) {
            Schema child = new ObjectSchema();
            current.setProperties(Collections.singletonMap("child", child));
            current = child;
        }

        IllegalArgumentException exception = Assert.expectThrows(
                IllegalArgumentException.class,
                () -> Oas31KeywordScanner.scanSchemaKeywordOccurrences(
                        openApiWithSchemas("3.1.0", Collections.singletonMap("Root", root))));
        Assert.assertTrue(exception.getMessage().contains("Schema nesting exceeds maximum depth"));
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
    public void scannerRecordsEmptyEnumOccurrences() {
        Schema schema = new Schema();
        schema.setEnum(Collections.emptyList());

        Oas31KeywordScanner.KeywordOccurrenceLedger ledger =
                Oas31KeywordScanner.scanSchemaKeywordOccurrences(
                        openApiWithSchemas("3.1.0", Collections.singletonMap("Empty", schema)));
        Assert.assertTrue(ledger.hasKeyword("enum"),
                "an empty enum is a reject-all assertion and must be recorded");
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
        root.setPropertyNames(new StringSchema());
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
        // patternProperties + propertyNames suites use the shared evaluator,
        // contains family green). contentEncoding/contentMediaType/contentSchema
        // are annotation-only per 2020-12 §8.2.6.
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
        Assert.assertTrue(ledger.hasKeyword("propertyNames"),
                "propertyNames must be present in this exercised schema");
        Assert.assertTrue(ledger.forKeyword("propertyNames").stream().allMatch(
                        occurrence -> occurrence.getStatus()
                                == Oas31KeywordScanner.KeywordOccurrenceStatus.EMITTED),
                "propertyNames must be emitted rather than silently skipped");
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
}
