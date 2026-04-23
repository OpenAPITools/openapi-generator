/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.media.*;
import org.testng.annotations.Test;

import java.util.*;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Unit tests for {@link GenericSchemaScanUtils}.
 */
public class GenericSchemaScanUtilsTest {

    // -------------------------------------------------------------------------
    // Helpers
    // -------------------------------------------------------------------------

    private static OpenAPI buildOpenAPI(Map<String, Schema> schemas) {
        OpenAPI openAPI = new OpenAPI();
        Components components = new Components();
        components.setSchemas(schemas);
        openAPI.setComponents(components);
        return openAPI;
    }

    /** Returns a local $ref string. */
    private static String ref(String name) {
        return "#/components/schemas/" + name;
    }

    /** Builds a simple string-typed schema. */
    private static Schema<?> stringSchema() {
        return new StringSchema();
    }

    /** Builds a simple integer-typed schema. */
    private static Schema<?> intSchema() {
        return new IntegerSchema();
    }

    /** Builds a $ref schema. */
    private static Schema<?> refSchema(String name) {
        return new Schema<>().$ref(ref(name));
    }

    /** Builds an array schema whose items are a $ref. */
    private static Schema<?> arrayRefSchema(String itemName) {
        ArraySchema arr = new ArraySchema();
        arr.setItems(new Schema<>().$ref(ref(itemName)));
        return arr;
    }

    /**
     * Builds an "ApiResponse-style" flat-object schema:
     *   data: $ref -> refTarget
     *   status: string
     *   message: string
     * (required: data, status)
     */
    private static Schema<?> responseSchema(String dataRefTarget) {
        ObjectSchema s = new ObjectSchema();
        Map<String, Schema> props = new LinkedHashMap<>();
        props.put("data", refSchema(dataRefTarget));
        props.put("status", stringSchema());
        props.put("message", stringSchema());
        s.setProperties(props);
        s.setRequired(Arrays.asList("data", "status"));
        return s;
    }

    /**
     * Builds a flat-object "Page-style" schema:
     *   content: array of $ref -> itemRefTarget
     *   page: $ref -> PageMeta
     * (required: content, page)
     */
    private static Schema<?> pageSchemaFlat(String itemRefTarget) {
        ObjectSchema s = new ObjectSchema();
        Map<String, Schema> props = new LinkedHashMap<>();
        props.put("content", arrayRefSchema(itemRefTarget));
        props.put("page", refSchema("PageMeta"));
        s.setProperties(props);
        s.setRequired(Arrays.asList("content", "page"));
        return s;
    }

    /**
     * Builds an allOf "Page-style" schema:
     *   allOf:
     *     - $ref: PageMeta
     *     - type: object
     *       properties:
     *         content: array of $ref -> itemRefTarget
     */
    private static Schema<?> pageSchemaAllOf(String itemRefTarget) {
        ComposedSchema s = new ComposedSchema();
        Schema<?> pageMetaRef = new Schema<>().$ref(ref("PageMeta"));
        ObjectSchema inline = new ObjectSchema();
        Map<String, Schema> props = new LinkedHashMap<>();
        props.put("content", arrayRefSchema(itemRefTarget));
        inline.setProperties(props);
        s.setAllOf(Arrays.asList(pageMetaRef, inline));
        return s;
    }

    /** Builds a "LogEntry-style" schema: data -> $ref, severity: string, timestamp: string. */
    private static Schema<?> entrySchema(String dataRefTarget) {
        ObjectSchema s = new ObjectSchema();
        Map<String, Schema> props = new LinkedHashMap<>();
        props.put("data", refSchema(dataRefTarget));
        props.put("severity", stringSchema());
        props.put("timestamp", stringSchema());
        s.setProperties(props);
        return s;
    }

    /** Builds a GenericPatternConfig with suffix and slot. */
    private static GenericPatternConfig suffixSlotPattern(String suffix, String genericClass, String slot) {
        GenericPatternConfig cfg = new GenericPatternConfig();
        cfg.suffix = suffix;
        cfg.genericClass = genericClass;
        cfg.slot = slot;
        return cfg;
    }

    /** Builds a GenericPatternConfig with suffix and slotArray. */
    private static GenericPatternConfig suffixSlotArrayPattern(String suffix, String genericClass, String slotArray) {
        GenericPatternConfig cfg = new GenericPatternConfig();
        cfg.suffix = suffix;
        cfg.genericClass = genericClass;
        cfg.slotArray = slotArray;
        return cfg;
    }

    // =========================================================================
    // matchesPattern
    // =========================================================================

    @Test
    public void matchesPattern_suffixMatch_returnsTrue() {
        GenericPatternConfig cfg = new GenericPatternConfig();
        cfg.suffix = "Response";
        assertThat(GenericSchemaScanUtils.matchesPattern("UserResponse", cfg)).isTrue();
        assertThat(GenericSchemaScanUtils.matchesPattern("PetResponse", cfg)).isTrue();
    }

    @Test
    public void matchesPattern_suffixExactNameOnly_returnsFalse() {
        GenericPatternConfig cfg = new GenericPatternConfig();
        cfg.suffix = "Response";
        // Name IS the suffix (no prefix part) — should not match
        assertThat(GenericSchemaScanUtils.matchesPattern("Response", cfg)).isFalse();
    }

    @Test
    public void matchesPattern_suffixNoMatch_returnsFalse() {
        GenericPatternConfig cfg = new GenericPatternConfig();
        cfg.suffix = "Response";
        assertThat(GenericSchemaScanUtils.matchesPattern("UserResult", cfg)).isFalse();
        assertThat(GenericSchemaScanUtils.matchesPattern("responsePage", cfg)).isFalse(); // case-sensitive
    }

    @Test
    public void matchesPattern_prefixMatch_returnsTrue() {
        GenericPatternConfig cfg = new GenericPatternConfig();
        cfg.prefix = "Api";
        assertThat(GenericSchemaScanUtils.matchesPattern("ApiUser", cfg)).isTrue();
        assertThat(GenericSchemaScanUtils.matchesPattern("ApiPet", cfg)).isTrue();
    }

    @Test
    public void matchesPattern_prefixExactNameOnly_returnsFalse() {
        GenericPatternConfig cfg = new GenericPatternConfig();
        cfg.prefix = "Api";
        assertThat(GenericSchemaScanUtils.matchesPattern("Api", cfg)).isFalse();
    }

    @Test
    public void matchesPattern_noSuffixOrPrefix_returnsFalse() {
        GenericPatternConfig cfg = new GenericPatternConfig();
        assertThat(GenericSchemaScanUtils.matchesPattern("AnySchema", cfg)).isFalse();
    }

    // =========================================================================
    // buildStructuralFingerprint
    // =========================================================================

    @Test
    public void buildStructuralFingerprint_flatObject_returnsConsistentFingerprint() {
        Schema<?> logEntry = entrySchema("LogEntryData");
        Schema<?> metricsEntry = entrySchema("MetricsEntryData");

        String fp1 = GenericSchemaScanUtils.buildStructuralFingerprint(logEntry);
        String fp2 = GenericSchemaScanUtils.buildStructuralFingerprint(metricsEntry);

        assertThat(fp1).isNotNull();
        // Both have same structure (data:$ref, severity:string, timestamp:string)
        // so fingerprints should be equal despite different $ref targets
        assertThat(fp1).isEqualTo(fp2);
    }

    @Test
    public void buildStructuralFingerprint_differentStructure_returnsDifferentFingerprints() {
        Schema<?> entry = entrySchema("LogEntryData");
        Schema<?> response = responseSchema("User");

        String fp1 = GenericSchemaScanUtils.buildStructuralFingerprint(entry);
        String fp2 = GenericSchemaScanUtils.buildStructuralFingerprint(response);

        assertThat(fp1).isNotNull();
        assertThat(fp2).isNotNull();
        assertThat(fp1).isNotEqualTo(fp2);
    }

    @Test
    public void buildStructuralFingerprint_allOfSchema_returnsNull() {
        Schema<?> allOf = pageSchemaAllOf("Pet");
        assertThat(GenericSchemaScanUtils.buildStructuralFingerprint(allOf)).isNull();
    }

    @Test
    public void buildStructuralFingerprint_emptyProperties_returnsNull() {
        ObjectSchema empty = new ObjectSchema();
        assertThat(GenericSchemaScanUtils.buildStructuralFingerprint(empty)).isNull();
    }

    // =========================================================================
    // Tier 1 — scanVendorExtensions
    // =========================================================================

    @Test
    public void scanVendorExtensions_detectsXGenericClass() {
        ObjectSchema vendorSchema = new ObjectSchema();
        Map<String, Schema> props = new LinkedHashMap<>();
        props.put("payload", refSchema("User"));
        props.put("code", intSchema());
        vendorSchema.setProperties(props);
        vendorSchema.setRequired(Collections.singletonList("payload"));

        Map<String, Object> extensions = new LinkedHashMap<>();
        extensions.put("x-generic-class", "com.example.VendorResult");
        Map<String, String> args = new LinkedHashMap<>();
        args.put("payload", "User");
        extensions.put("x-generic-args", args);
        vendorSchema.setExtensions(extensions);

        Map<String, Schema> schemas = new LinkedHashMap<>();
        schemas.put("UserVendorResult", vendorSchema);
        schemas.put("User", new ObjectSchema());
        OpenAPI openAPI = buildOpenAPI(schemas);

        List<GenericSchemaScanUtils.GenericInstance> result =
                GenericSchemaScanUtils.scanVendorExtensions(openAPI);

        assertThat(result).hasSize(1);
        GenericSchemaScanUtils.GenericInstance inst = result.get(0);
        assertThat(inst.schemaName).isEqualTo("UserVendorResult");
        assertThat(inst.genericClassName).isEqualTo("VendorResult");
        assertThat(inst.genericClassFqn).isEqualTo("com.example.VendorResult");
        assertThat(inst.generateClass).isFalse(); // FQN → Mode A
        assertThat(inst.slotProperty).isEqualTo("payload");
        assertThat(inst.slotIsArray).isFalse();
        assertThat(inst.firstTypeArg()).isEqualTo("User");
    }

    @Test
    public void scanVendorExtensions_simpleNameGenericClass_isModeB() {
        ObjectSchema schema = new ObjectSchema();
        Map<String, Schema> props = new LinkedHashMap<>();
        props.put("data", refSchema("Pet"));
        schema.setProperties(props);

        Map<String, Object> extensions = new LinkedHashMap<>();
        extensions.put("x-generic-class", "MyGeneric");
        Map<String, String> args = new LinkedHashMap<>();
        args.put("data", "Pet");
        extensions.put("x-generic-args", args);
        schema.setExtensions(extensions);

        Map<String, Schema> schemas = new LinkedHashMap<>();
        schemas.put("PetMyGeneric", schema);
        schemas.put("Pet", new ObjectSchema());
        OpenAPI openAPI = buildOpenAPI(schemas);

        List<GenericSchemaScanUtils.GenericInstance> result =
                GenericSchemaScanUtils.scanVendorExtensions(openAPI);

        assertThat(result).hasSize(1);
        GenericSchemaScanUtils.GenericInstance inst = result.get(0);
        assertThat(inst.genericClassName).isEqualTo("MyGeneric");
        assertThat(inst.genericClassFqn).isNull();
        assertThat(inst.generateClass).isTrue(); // no dot → Mode B
    }

    @Test
    public void scanVendorExtensions_missingXGenericArgs_skipsSchema() {
        ObjectSchema schema = new ObjectSchema();
        Map<String, Object> extensions = new LinkedHashMap<>();
        extensions.put("x-generic-class", "com.example.Whatever");
        // no x-generic-args
        schema.setExtensions(extensions);

        Map<String, Schema> schemas = new LinkedHashMap<>();
        schemas.put("NoArgsSchema", schema);
        OpenAPI openAPI = buildOpenAPI(schemas);

        List<GenericSchemaScanUtils.GenericInstance> result =
                GenericSchemaScanUtils.scanVendorExtensions(openAPI);

        assertThat(result).isEmpty();
    }

    @Test
    public void scanVendorExtensions_noExtensions_returnsEmpty() {
        Map<String, Schema> schemas = new LinkedHashMap<>();
        schemas.put("PlainSchema", new ObjectSchema());
        OpenAPI openAPI = buildOpenAPI(schemas);

        assertThat(GenericSchemaScanUtils.scanVendorExtensions(openAPI)).isEmpty();
    }

    @Test
    public void scanVendorExtensions_emptyOpenAPI_returnsEmpty() {
        assertThat(GenericSchemaScanUtils.scanVendorExtensions(new OpenAPI())).isEmpty();
    }

    // =========================================================================
    // Tier 2 — scanWithPatterns (slot / $ref)
    // =========================================================================

    @Test
    public void scanWithPatterns_suffixSlot_matchesAllSuffix() {
        Map<String, Schema> schemas = new LinkedHashMap<>();
        schemas.put("UserResponse", responseSchema("User"));
        schemas.put("PetResponse", responseSchema("Pet"));
        schemas.put("OrderResponse", responseSchema("Order"));
        schemas.put("User", new ObjectSchema());
        schemas.put("Pet", new ObjectSchema());
        schemas.put("Order", new ObjectSchema());
        OpenAPI openAPI = buildOpenAPI(schemas);

        List<GenericPatternConfig> patterns = Collections.singletonList(
                suffixSlotPattern("Response", "ApiResponse", "data"));

        List<GenericSchemaScanUtils.GenericInstance> result =
                GenericSchemaScanUtils.scanWithPatterns(openAPI, patterns, Collections.emptySet());

        assertThat(result).hasSize(3);

        List<String> matched = new ArrayList<>();
        for (GenericSchemaScanUtils.GenericInstance inst : result) {
            matched.add(inst.schemaName);
            assertThat(inst.genericClassName).isEqualTo("ApiResponse");
            assertThat(inst.slotProperty).isEqualTo("data");
            assertThat(inst.slotIsArray).isFalse();
        }
        assertThat(matched).containsExactlyInAnyOrder("UserResponse", "PetResponse", "OrderResponse");
    }

    @Test
    public void scanWithPatterns_fqnGenericClass_isModeA() {
        Map<String, Schema> schemas = new LinkedHashMap<>();
        schemas.put("UserResponse", responseSchema("User"));
        schemas.put("User", new ObjectSchema());
        OpenAPI openAPI = buildOpenAPI(schemas);

        List<GenericPatternConfig> patterns = Collections.singletonList(
                suffixSlotPattern("Response", "com.example.ApiResponse", "data"));

        List<GenericSchemaScanUtils.GenericInstance> result =
                GenericSchemaScanUtils.scanWithPatterns(openAPI, patterns, Collections.emptySet());

        assertThat(result).hasSize(1);
        GenericSchemaScanUtils.GenericInstance inst = result.get(0);
        assertThat(inst.genericClassFqn).isEqualTo("com.example.ApiResponse");
        assertThat(inst.generateClass).isFalse();
    }

    @Test
    public void scanWithPatterns_simpleNameGenericClass_isModeB() {
        Map<String, Schema> schemas = new LinkedHashMap<>();
        schemas.put("UserResponse", responseSchema("User"));
        schemas.put("User", new ObjectSchema());
        OpenAPI openAPI = buildOpenAPI(schemas);

        List<GenericPatternConfig> patterns = Collections.singletonList(
                suffixSlotPattern("Response", "ApiResponse", "data"));

        List<GenericSchemaScanUtils.GenericInstance> result =
                GenericSchemaScanUtils.scanWithPatterns(openAPI, patterns, Collections.emptySet());

        assertThat(result).hasSize(1);
        assertThat(result.get(0).genericClassFqn).isNull();
        assertThat(result.get(0).generateClass).isTrue();
    }

    @Test
    public void scanWithPatterns_schemaNameExactlySuffix_doesNotMatch() {
        Map<String, Schema> schemas = new LinkedHashMap<>();
        schemas.put("Response", responseSchema("User")); // name == suffix, should not match
        schemas.put("User", new ObjectSchema());
        OpenAPI openAPI = buildOpenAPI(schemas);

        List<GenericPatternConfig> patterns = Collections.singletonList(
                suffixSlotPattern("Response", "ApiResponse", "data"));

        List<GenericSchemaScanUtils.GenericInstance> result =
                GenericSchemaScanUtils.scanWithPatterns(openAPI, patterns, Collections.emptySet());

        assertThat(result).isEmpty();
    }

    @Test
    public void scanWithPatterns_slotPropertyAbsent_doesNotMatch() {
        // Schema has no "data" property
        ObjectSchema noDataSchema = new ObjectSchema();
        Map<String, Schema> props = new LinkedHashMap<>();
        props.put("payload", refSchema("User")); // wrong property name
        props.put("status", stringSchema());
        noDataSchema.setProperties(props);

        Map<String, Schema> schemas = new LinkedHashMap<>();
        schemas.put("UserResponse", noDataSchema);
        schemas.put("User", new ObjectSchema());
        OpenAPI openAPI = buildOpenAPI(schemas);

        List<GenericPatternConfig> patterns = Collections.singletonList(
                suffixSlotPattern("Response", "ApiResponse", "data")); // expects "data"

        List<GenericSchemaScanUtils.GenericInstance> result =
                GenericSchemaScanUtils.scanWithPatterns(openAPI, patterns, Collections.emptySet());

        assertThat(result).isEmpty();
    }

    @Test
    public void scanWithPatterns_tier1ExcludedSchemas_areSkipped() {
        Map<String, Schema> schemas = new LinkedHashMap<>();
        schemas.put("UserResponse", responseSchema("User"));
        schemas.put("PetResponse", responseSchema("Pet"));
        schemas.put("User", new ObjectSchema());
        schemas.put("Pet", new ObjectSchema());
        OpenAPI openAPI = buildOpenAPI(schemas);

        List<GenericPatternConfig> patterns = Collections.singletonList(
                suffixSlotPattern("Response", "ApiResponse", "data"));

        // Exclude UserResponse (already handled by Tier 1)
        Set<String> tier1 = Collections.singleton("UserResponse");
        List<GenericSchemaScanUtils.GenericInstance> result =
                GenericSchemaScanUtils.scanWithPatterns(openAPI, patterns, tier1);

        assertThat(result).hasSize(1);
        assertThat(result.get(0).schemaName).isEqualTo("PetResponse");
    }

    @Test
    public void scanWithPatterns_firstMatchingPatternWins() {
        Map<String, Schema> schemas = new LinkedHashMap<>();
        schemas.put("UserResponse", responseSchema("User"));
        schemas.put("User", new ObjectSchema());
        OpenAPI openAPI = buildOpenAPI(schemas);

        List<GenericPatternConfig> patterns = Arrays.asList(
                suffixSlotPattern("Response", "FirstApiResponse", "data"),
                suffixSlotPattern("Response", "SecondApiResponse", "data")
        );

        List<GenericSchemaScanUtils.GenericInstance> result =
                GenericSchemaScanUtils.scanWithPatterns(openAPI, patterns, Collections.emptySet());

        assertThat(result).hasSize(1);
        assertThat(result.get(0).genericClassName).isEqualTo("FirstApiResponse");
    }

    // =========================================================================
    // Tier 2 — scanWithPatterns (slotArray)
    // =========================================================================

    @Test
    public void scanWithPatterns_slotArrayFlatForm_matchesArraySlot() {
        Map<String, Schema> schemas = new LinkedHashMap<>();
        schemas.put("UserPage", pageSchemaFlat("User"));
        schemas.put("PageMeta", new ObjectSchema());
        schemas.put("User", new ObjectSchema());
        OpenAPI openAPI = buildOpenAPI(schemas);

        List<GenericPatternConfig> patterns = Collections.singletonList(
                suffixSlotArrayPattern("Page", "org.springframework.data.domain.Page", "content"));

        List<GenericSchemaScanUtils.GenericInstance> result =
                GenericSchemaScanUtils.scanWithPatterns(openAPI, patterns, Collections.emptySet());

        assertThat(result).hasSize(1);
        GenericSchemaScanUtils.GenericInstance inst = result.get(0);
        assertThat(inst.schemaName).isEqualTo("UserPage");
        assertThat(inst.slotProperty).isEqualTo("content");
        assertThat(inst.slotIsArray).isTrue();
        assertThat(inst.firstTypeArg()).isEqualTo("User");
    }

    @Test
    public void scanWithPatterns_slotArrayAllOfForm_matchesArraySlot() {
        Map<String, Schema> schemas = new LinkedHashMap<>();
        schemas.put("PetPage", pageSchemaAllOf("Pet"));
        schemas.put("PageMeta", new ObjectSchema());
        schemas.put("Pet", new ObjectSchema());
        OpenAPI openAPI = buildOpenAPI(schemas);

        List<GenericPatternConfig> patterns = Collections.singletonList(
                suffixSlotArrayPattern("Page", "org.springframework.data.domain.Page", "content"));

        List<GenericSchemaScanUtils.GenericInstance> result =
                GenericSchemaScanUtils.scanWithPatterns(openAPI, patterns, Collections.emptySet());

        assertThat(result).hasSize(1);
        GenericSchemaScanUtils.GenericInstance inst = result.get(0);
        assertThat(inst.schemaName).isEqualTo("PetPage");
        assertThat(inst.slotProperty).isEqualTo("content");
        assertThat(inst.slotIsArray).isTrue();
        assertThat(inst.firstTypeArg()).isEqualTo("Pet");
    }

    @Test
    public void scanWithPatterns_slotArrayMissingContent_doesNotMatch() {
        // Schema has "items" array but not "content"
        ObjectSchema wrongSlot = new ObjectSchema();
        Map<String, Schema> props = new LinkedHashMap<>();
        props.put("items", arrayRefSchema("User")); // wrong slot name
        props.put("page", refSchema("PageMeta"));
        wrongSlot.setProperties(props);

        Map<String, Schema> schemas = new LinkedHashMap<>();
        schemas.put("UserPage", wrongSlot);
        schemas.put("PageMeta", new ObjectSchema());
        schemas.put("User", new ObjectSchema());
        OpenAPI openAPI = buildOpenAPI(schemas);

        List<GenericPatternConfig> patterns = Collections.singletonList(
                suffixSlotArrayPattern("Page", "org.springframework.data.domain.Page", "content"));

        List<GenericSchemaScanUtils.GenericInstance> result =
                GenericSchemaScanUtils.scanWithPatterns(openAPI, patterns, Collections.emptySet());

        assertThat(result).isEmpty();
    }

    @Test
    public void scanWithPatterns_emptyPatternList_returnsEmpty() {
        Map<String, Schema> schemas = new LinkedHashMap<>();
        schemas.put("UserResponse", responseSchema("User"));
        schemas.put("User", new ObjectSchema());
        OpenAPI openAPI = buildOpenAPI(schemas);

        List<GenericSchemaScanUtils.GenericInstance> result =
                GenericSchemaScanUtils.scanWithPatterns(openAPI, Collections.emptyList(), Collections.emptySet());

        assertThat(result).isEmpty();
    }

    @Test
    public void scanWithPatterns_patternWithNoGenericClass_isSkipped() {
        Map<String, Schema> schemas = new LinkedHashMap<>();
        schemas.put("UserResponse", responseSchema("User"));
        schemas.put("User", new ObjectSchema());
        OpenAPI openAPI = buildOpenAPI(schemas);

        GenericPatternConfig bad = new GenericPatternConfig();
        bad.suffix = "Response";
        // genericClass intentionally omitted

        List<GenericSchemaScanUtils.GenericInstance> result =
                GenericSchemaScanUtils.scanWithPatterns(openAPI, Collections.singletonList(bad), Collections.emptySet());

        assertThat(result).isEmpty();
    }

    // =========================================================================
    // Tier 2 — property metadata in GenericInstance
    // =========================================================================

    @Test
    public void scanWithPatterns_properties_containsSlotWithTypeParam() {
        Map<String, Schema> schemas = new LinkedHashMap<>();
        schemas.put("UserResponse", responseSchema("User"));
        schemas.put("User", new ObjectSchema());
        OpenAPI openAPI = buildOpenAPI(schemas);

        List<GenericPatternConfig> patterns = Collections.singletonList(
                suffixSlotPattern("Response", "ApiResponse", "data"));

        List<GenericSchemaScanUtils.GenericInstance> result =
                GenericSchemaScanUtils.scanWithPatterns(openAPI, patterns, Collections.emptySet());

        assertThat(result).hasSize(1);
        List<GenericSchemaScanUtils.GenericProperty> props = result.get(0).properties;

        GenericSchemaScanUtils.GenericProperty dataSlot = props.stream()
                .filter(p -> "data".equals(p.name)).findFirst().orElse(null);
        assertThat(dataSlot).isNotNull();
        assertThat(dataSlot.typeParam).isEqualTo("T");
        assertThat(dataSlot.isArray).isFalse();

        GenericSchemaScanUtils.GenericProperty status = props.stream()
                .filter(p -> "status".equals(p.name)).findFirst().orElse(null);
        assertThat(status).isNotNull();
        assertThat(status.typeParam).isNull();
        assertThat(status.openApiType).isEqualTo("string");
    }

    @Test
    public void scanWithPatterns_arraySlotProperty_hasIsArrayTrue() {
        Map<String, Schema> schemas = new LinkedHashMap<>();
        schemas.put("UserPage", pageSchemaFlat("User"));
        schemas.put("PageMeta", new ObjectSchema());
        schemas.put("User", new ObjectSchema());
        OpenAPI openAPI = buildOpenAPI(schemas);

        List<GenericPatternConfig> patterns = Collections.singletonList(
                suffixSlotArrayPattern("Page", "Page", "content"));

        List<GenericSchemaScanUtils.GenericInstance> result =
                GenericSchemaScanUtils.scanWithPatterns(openAPI, patterns, Collections.emptySet());

        assertThat(result).hasSize(1);
        GenericSchemaScanUtils.GenericProperty contentSlot = result.get(0).properties.stream()
                .filter(p -> "content".equals(p.name)).findFirst().orElse(null);
        assertThat(contentSlot).isNotNull();
        assertThat(contentSlot.typeParam).isEqualTo("T");
        assertThat(contentSlot.isArray).isTrue();
    }

    // =========================================================================
    // Tier 3 — discoverClusters
    // =========================================================================

    @Test
    public void discoverClusters_twoStructurallySimilarSchemas_returnsCluster() {
        Map<String, Schema> schemas = new LinkedHashMap<>();
        schemas.put("LogEntry", entrySchema("LogEntryData"));
        schemas.put("MetricsEntry", entrySchema("MetricsEntryData"));
        schemas.put("LogEntryData", new ObjectSchema());
        schemas.put("MetricsEntryData", new ObjectSchema());
        OpenAPI openAPI = buildOpenAPI(schemas);

        List<GenericSchemaScanUtils.ClusterSuggestion> suggestions =
                GenericSchemaScanUtils.discoverClusters(openAPI, Collections.emptySet());

        assertThat(suggestions).hasSize(1);
        GenericSchemaScanUtils.ClusterSuggestion suggestion = suggestions.get(0);
        assertThat(suggestion.schemaNames).containsExactlyInAnyOrder("LogEntry", "MetricsEntry");
        assertThat(suggestion.varyingSlotProperty).isEqualTo("data");
        assertThat(suggestion.varyingTypes).containsExactlyInAnyOrder("LogEntryData", "MetricsEntryData");
        assertThat(suggestion.suggestedConfig).isNotBlank();
    }

    @Test
    public void discoverClusters_threeStructurallySimilarSchemas_returnsOneCluster() {
        Map<String, Schema> schemas = new LinkedHashMap<>();
        schemas.put("UserResponse", responseSchema("User"));
        schemas.put("PetResponse", responseSchema("Pet"));
        schemas.put("OrderResponse", responseSchema("Order"));
        schemas.put("User", new ObjectSchema());
        schemas.put("Pet", new ObjectSchema());
        schemas.put("Order", new ObjectSchema());
        OpenAPI openAPI = buildOpenAPI(schemas);

        List<GenericSchemaScanUtils.ClusterSuggestion> suggestions =
                GenericSchemaScanUtils.discoverClusters(openAPI, Collections.emptySet());

        assertThat(suggestions).hasSize(1);
        GenericSchemaScanUtils.ClusterSuggestion s = suggestions.get(0);
        assertThat(s.schemaNames).containsExactlyInAnyOrder("UserResponse", "PetResponse", "OrderResponse");
        assertThat(s.varyingSlotProperty).isEqualTo("data");
        assertThat(s.varyingTypes).containsExactlyInAnyOrder("User", "Pet", "Order");
    }

    @Test
    public void discoverClusters_uniqueStructure_returnsEmpty() {
        // SearchResult has a unique structure — no cluster
        ObjectSchema searchResult = new ObjectSchema();
        Map<String, Schema> props = new LinkedHashMap<>();
        props.put("query", stringSchema());
        props.put("totalHits", intSchema());
        props.put("results", new ArraySchema().items(stringSchema()));
        searchResult.setProperties(props);

        Map<String, Schema> schemas = new LinkedHashMap<>();
        schemas.put("SearchResult", searchResult);
        OpenAPI openAPI = buildOpenAPI(schemas);

        List<GenericSchemaScanUtils.ClusterSuggestion> suggestions =
                GenericSchemaScanUtils.discoverClusters(openAPI, Collections.emptySet());

        assertThat(suggestions).isEmpty();
    }

    @Test
    public void discoverClusters_excludedSchemas_notIncludedInClusters() {
        Map<String, Schema> schemas = new LinkedHashMap<>();
        schemas.put("LogEntry", entrySchema("LogEntryData"));
        schemas.put("MetricsEntry", entrySchema("MetricsEntryData"));
        schemas.put("LogEntryData", new ObjectSchema());
        schemas.put("MetricsEntryData", new ObjectSchema());
        OpenAPI openAPI = buildOpenAPI(schemas);

        // Exclude both — cluster should not form
        Set<String> excluded = new HashSet<>(Arrays.asList("LogEntry", "MetricsEntry"));
        List<GenericSchemaScanUtils.ClusterSuggestion> suggestions =
                GenericSchemaScanUtils.discoverClusters(openAPI, excluded);

        assertThat(suggestions).isEmpty();
    }

    @Test
    public void discoverClusters_allOfSchemas_excludedFromClustering() {
        // allOf schemas cannot be fingerprinted
        Map<String, Schema> schemas = new LinkedHashMap<>();
        schemas.put("PetPage", pageSchemaAllOf("Pet"));
        schemas.put("UserPage", pageSchemaAllOf("User"));
        schemas.put("Pet", new ObjectSchema());
        schemas.put("User", new ObjectSchema());
        schemas.put("PageMeta", new ObjectSchema());
        OpenAPI openAPI = buildOpenAPI(schemas);

        List<GenericSchemaScanUtils.ClusterSuggestion> suggestions =
                GenericSchemaScanUtils.discoverClusters(openAPI, Collections.emptySet());

        // allOf schemas return null fingerprint — they won't cluster
        assertThat(suggestions).isEmpty();
    }

    @Test
    public void discoverClusters_suggestedConfigContainsSlotAndSuffix() {
        Map<String, Schema> schemas = new LinkedHashMap<>();
        schemas.put("LogEntry", entrySchema("LogEntryData"));
        schemas.put("MetricsEntry", entrySchema("MetricsEntryData"));
        schemas.put("LogEntryData", new ObjectSchema());
        schemas.put("MetricsEntryData", new ObjectSchema());
        OpenAPI openAPI = buildOpenAPI(schemas);

        List<GenericSchemaScanUtils.ClusterSuggestion> suggestions =
                GenericSchemaScanUtils.discoverClusters(openAPI, Collections.emptySet());

        assertThat(suggestions).hasSize(1);
        String config = suggestions.get(0).suggestedConfig;
        // Config should mention slot name and a suffix or prefix suggestion
        assertThat(config).contains("data"); // slot property
    }

    @Test
    public void discoverClusters_singleSchema_doesNotFormCluster() {
        Map<String, Schema> schemas = new LinkedHashMap<>();
        schemas.put("OnlyEntry", entrySchema("SomeData"));
        schemas.put("SomeData", new ObjectSchema());
        OpenAPI openAPI = buildOpenAPI(schemas);

        List<GenericSchemaScanUtils.ClusterSuggestion> suggestions =
                GenericSchemaScanUtils.discoverClusters(openAPI, Collections.emptySet());

        // Cluster requires >= 2 members
        assertThat(suggestions).isEmpty();
    }

    // =========================================================================
    // resolveProperties — allOf merging
    // =========================================================================

    @Test
    public void resolveProperties_allOfSchema_mergesPropertiesFromInlineEntries() {
        Schema<?> allOf = pageSchemaAllOf("Pet");
        OpenAPI openAPI = buildOpenAPI(Collections.singletonMap("Pet", new ObjectSchema()));

        Map<String, Schema> props = GenericSchemaScanUtils.resolveProperties(allOf, openAPI);

        assertThat(props).isNotNull();
        assertThat(props).containsKey("content");
    }

    @Test
    public void resolveProperties_flatSchema_returnsSchemaProperties() {
        Schema<?> flat = responseSchema("User");
        OpenAPI openAPI = buildOpenAPI(Collections.singletonMap("User", new ObjectSchema()));

        Map<String, Schema> props = GenericSchemaScanUtils.resolveProperties(flat, openAPI);

        assertThat(props).isNotNull();
        assertThat(props).containsKeys("data", "status", "message");
    }

    @Test
    public void resolveProperties_emptySchema_returnsNull() {
        assertThat(GenericSchemaScanUtils.resolveProperties(new ObjectSchema(), new OpenAPI())).isNull();
    }
}
