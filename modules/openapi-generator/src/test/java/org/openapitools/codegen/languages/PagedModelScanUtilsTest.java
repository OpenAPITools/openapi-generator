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
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.IntegerSchema;
import io.swagger.v3.oas.models.media.ObjectSchema;
import io.swagger.v3.oas.models.media.Schema;
import org.testng.annotations.Test;

import java.util.*;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Unit tests for {@link PagedModelScanUtils}.
 */
public class PagedModelScanUtilsTest {

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

    /**
     * Builds a minimal pagination-metadata schema that satisfies the heuristic
     * ({@code size}, {@code number}, {@code totalElements}, {@code totalPages}).
     */
    private static Schema<?> pageMetadataSchema() {
        ObjectSchema schema = new ObjectSchema();
        Map<String, Schema> props = new LinkedHashMap<>();
        props.put("size",          new IntegerSchema());
        props.put("number",        new IntegerSchema());
        props.put("totalElements", new IntegerSchema());
        props.put("totalPages",    new IntegerSchema());
        schema.setProperties(props);
        return schema;
    }

    /**
     * Builds a domain schema ref string for use in $ref.
     */
    private static String ref(String name) {
        return "#/components/schemas/" + name;
    }

    // -------------------------------------------------------------------------
    // isPaginationSchema
    // -------------------------------------------------------------------------

    @Test
    public void isPaginationSchema_returnsTrueForSchemaWithEnoughPaginationFields() {
        assertThat(PagedModelScanUtils.isPaginationSchema(pageMetadataSchema())).isTrue();
    }

    @Test
    public void isPaginationSchema_returnsTrueWhenOnlyTwoFieldsMatch() {
        ObjectSchema schema = new ObjectSchema();
        Map<String, Schema> props = new HashMap<>();
        props.put("size",  new IntegerSchema());
        props.put("totalElements", new IntegerSchema());
        schema.setProperties(props);
        assertThat(PagedModelScanUtils.isPaginationSchema(schema)).isTrue();
    }

    @Test
    public void isPaginationSchema_returnsFalseWhenOnlyOneFieldMatches() {
        ObjectSchema schema = new ObjectSchema();
        Map<String, Schema> props = new HashMap<>();
        props.put("size", new IntegerSchema());
        props.put("foo",  new IntegerSchema());
        schema.setProperties(props);
        assertThat(PagedModelScanUtils.isPaginationSchema(schema)).isFalse();
    }

    @Test
    public void isPaginationSchema_returnsFalseForNull() {
        assertThat(PagedModelScanUtils.isPaginationSchema(null)).isFalse();
    }

    @Test
    public void isPaginationSchema_returnsFalseForSchemaWithNoProperties() {
        assertThat(PagedModelScanUtils.isPaginationSchema(new ObjectSchema())).isFalse();
    }

    // -------------------------------------------------------------------------
    // scanPagedModels — flat-object form
    // -------------------------------------------------------------------------

    @Test
    public void scanPagedModels_detectsFlatObjectForm() {
        // Build UserPage with content array + page ref
        ArraySchema contentSchema = new ArraySchema();
        contentSchema.setItems(new Schema<>().$ref(ref("User")));

        Schema<?> pageRef = new Schema<>().$ref(ref("PageMetadata"));

        ObjectSchema userPageSchema = new ObjectSchema();
        Map<String, Schema> props = new LinkedHashMap<>();
        props.put("content", contentSchema);
        props.put("page",    pageRef);
        userPageSchema.setProperties(props);

        Map<String, Schema> schemas = new LinkedHashMap<>();
        schemas.put("PageMetadata", pageMetadataSchema());
        schemas.put("User",         new ObjectSchema());
        schemas.put("UserPage",     userPageSchema);

        OpenAPI openAPI = buildOpenAPI(schemas);
        Map<String, PagedModelScanUtils.DetectedPagedModel> result =
                PagedModelScanUtils.scanPagedModels(openAPI);

        assertThat(result).containsKey("UserPage");
        PagedModelScanUtils.DetectedPagedModel detected = result.get("UserPage");
        assertThat(detected.schemaName).isEqualTo("UserPage");
        assertThat(detected.itemSchemaName).isEqualTo("User");
        assertThat(detected.metaSchemaName).isEqualTo("PageMetadata");
    }

    @Test
    public void scanPagedModels_detectsMultiplePagesWithSharedMetaSchema() {
        ArraySchema userContent = new ArraySchema();
        userContent.setItems(new Schema<>().$ref(ref("User")));
        ObjectSchema userPage = new ObjectSchema();
        Map<String, Schema> userProps = new LinkedHashMap<>();
        userProps.put("content", userContent);
        userProps.put("page",    new Schema<>().$ref(ref("PageMetadata")));
        userPage.setProperties(userProps);

        ArraySchema orderContent = new ArraySchema();
        orderContent.setItems(new Schema<>().$ref(ref("Order")));
        ObjectSchema orderPage = new ObjectSchema();
        Map<String, Schema> orderProps = new LinkedHashMap<>();
        orderProps.put("content", orderContent);
        orderProps.put("page",    new Schema<>().$ref(ref("PageMetadata")));
        orderPage.setProperties(orderProps);

        Map<String, Schema> schemas = new LinkedHashMap<>();
        schemas.put("PageMetadata", pageMetadataSchema());
        schemas.put("User",         new ObjectSchema());
        schemas.put("Order",        new ObjectSchema());
        schemas.put("UserPage",     userPage);
        schemas.put("OrderPage",    orderPage);

        OpenAPI openAPI = buildOpenAPI(schemas);
        Map<String, PagedModelScanUtils.DetectedPagedModel> result =
                PagedModelScanUtils.scanPagedModels(openAPI);

        assertThat(result).containsKeys("UserPage", "OrderPage");
        assertThat(result.get("UserPage").itemSchemaName).isEqualTo("User");
        assertThat(result.get("OrderPage").itemSchemaName).isEqualTo("Order");
        assertThat(result.get("UserPage").metaSchemaName).isEqualTo("PageMetadata");
        assertThat(result.get("OrderPage").metaSchemaName).isEqualTo("PageMetadata");
    }

    // -------------------------------------------------------------------------
    // scanPagedModels — allOf form
    // -------------------------------------------------------------------------

    @Test
    public void scanPagedModels_detectsAllOfForm() {
        // allOf: [$ref(PageMeta), {properties: {content: array items: $ref(User)}}]
        Schema<?> metaRef = new Schema<>().$ref(ref("PageMeta"));

        ArraySchema contentSchema = new ArraySchema();
        contentSchema.setItems(new Schema<>().$ref(ref("User")));
        ObjectSchema inlineSchema = new ObjectSchema();
        Map<String, Schema> inlineProps = new LinkedHashMap<>();
        inlineProps.put("content", contentSchema);
        inlineSchema.setProperties(inlineProps);

        Schema<?> userPageSchema = new Schema<>();
        userPageSchema.setAllOf(Arrays.asList(metaRef, inlineSchema));

        Map<String, Schema> schemas = new LinkedHashMap<>();
        schemas.put("PageMeta",  pageMetadataSchema());
        schemas.put("User",      new ObjectSchema());
        schemas.put("UserPage",  userPageSchema);

        OpenAPI openAPI = buildOpenAPI(schemas);
        Map<String, PagedModelScanUtils.DetectedPagedModel> result =
                PagedModelScanUtils.scanPagedModels(openAPI);

        assertThat(result).containsKey("UserPage");
        PagedModelScanUtils.DetectedPagedModel detected = result.get("UserPage");
        assertThat(detected.itemSchemaName).isEqualTo("User");
        assertThat(detected.metaSchemaName).isEqualTo("PageMeta");
    }

    // -------------------------------------------------------------------------
    // scanPagedModels — false positives
    // -------------------------------------------------------------------------

    @Test
    public void scanPagedModels_doesNotDetectSchemaWithContentButNoPaginationProperty() {
        // content is an array but there is no pagination metadata property
        ArraySchema contentSchema = new ArraySchema();
        contentSchema.setItems(new Schema<>().$ref(ref("User")));

        ObjectSchema schema = new ObjectSchema();
        Map<String, Schema> props = new LinkedHashMap<>();
        props.put("content", contentSchema);
        props.put("name",    new Schema<>().type("string"));
        schema.setProperties(props);

        Map<String, Schema> schemas = new LinkedHashMap<>();
        schemas.put("User",        new ObjectSchema());
        schemas.put("UserResults", schema);

        OpenAPI openAPI = buildOpenAPI(schemas);
        Map<String, PagedModelScanUtils.DetectedPagedModel> result =
                PagedModelScanUtils.scanPagedModels(openAPI);

        assertThat(result).doesNotContainKey("UserResults");
    }

    @Test
    public void scanPagedModels_doesNotDetectSchemasWithNoPaginationMetadataRef() {
        // both properties are plain objects — neither is a pagination metadata $ref
        ArraySchema contentSchema = new ArraySchema();
        contentSchema.setItems(new Schema<>().$ref(ref("User")));

        // "meta" property that is an inline object (no $ref), not a pagination schema
        ObjectSchema inlineMeta = new ObjectSchema();
        Map<String, Schema> metaProps = new HashMap<>();
        metaProps.put("foo", new IntegerSchema());
        inlineMeta.setProperties(metaProps);

        ObjectSchema schema = new ObjectSchema();
        Map<String, Schema> props = new LinkedHashMap<>();
        props.put("content", contentSchema);
        props.put("meta",    inlineMeta);
        schema.setProperties(props);

        Map<String, Schema> schemas = new LinkedHashMap<>();
        schemas.put("User",     new ObjectSchema());
        schemas.put("UserList", schema);

        OpenAPI openAPI = buildOpenAPI(schemas);
        Map<String, PagedModelScanUtils.DetectedPagedModel> result =
                PagedModelScanUtils.scanPagedModels(openAPI);

        assertThat(result).doesNotContainKey("UserList");
    }

    @Test
    public void scanPagedModels_doesNotDetectSchemaWithNonArrayContent() {
        // 'content' exists but is not an array
        Schema<?> pageRef = new Schema<>().$ref(ref("PageMetadata"));

        ObjectSchema schema = new ObjectSchema();
        Map<String, Schema> props = new LinkedHashMap<>();
        props.put("content", new Schema<>().type("string"));
        props.put("page",    pageRef);
        schema.setProperties(props);

        Map<String, Schema> schemas = new LinkedHashMap<>();
        schemas.put("PageMetadata", pageMetadataSchema());
        schemas.put("StringPage",   schema);

        OpenAPI openAPI = buildOpenAPI(schemas);
        Map<String, PagedModelScanUtils.DetectedPagedModel> result =
                PagedModelScanUtils.scanPagedModels(openAPI);

        assertThat(result).doesNotContainKey("StringPage");
    }

    @Test
    public void scanPagedModels_doesNotDetectSchemaWithArrayContentButNoItemsRef() {
        // 'content' is an array but items has no $ref (inline type instead)
        ArraySchema contentSchema = new ArraySchema();
        contentSchema.setItems(new Schema<>().type("string")); // no $ref on items

        Schema<?> pageRef = new Schema<>().$ref(ref("PageMetadata"));

        ObjectSchema schema = new ObjectSchema();
        Map<String, Schema> props = new LinkedHashMap<>();
        props.put("content", contentSchema);
        props.put("page",    pageRef);
        schema.setProperties(props);

        Map<String, Schema> schemas = new LinkedHashMap<>();
        schemas.put("PageMetadata", pageMetadataSchema());
        schemas.put("StringPage",   schema);

        OpenAPI openAPI = buildOpenAPI(schemas);
        Map<String, PagedModelScanUtils.DetectedPagedModel> result =
                PagedModelScanUtils.scanPagedModels(openAPI);

        assertThat(result).doesNotContainKey("StringPage");
    }

    @Test
    public void scanPagedModels_returnsEmptyWhenNoSchemas() {
        OpenAPI openAPI = new OpenAPI();
        openAPI.setComponents(new Components());

        Map<String, PagedModelScanUtils.DetectedPagedModel> result =
                PagedModelScanUtils.scanPagedModels(openAPI);

        assertThat(result).isEmpty();
    }

    @Test
    public void scanPagedModels_returnsEmptyWhenComponentsIsNull() {
        OpenAPI openAPI = new OpenAPI();

        Map<String, PagedModelScanUtils.DetectedPagedModel> result =
                PagedModelScanUtils.scanPagedModels(openAPI);

        assertThat(result).isEmpty();
    }

    // -------------------------------------------------------------------------
    // extractSchemaNameFromRef
    // -------------------------------------------------------------------------

    @Test
    public void extractSchemaNameFromRef_extractsSimpleNameFromFullRef() {
        assertThat(PagedModelScanUtils.extractSchemaNameFromRef("#/components/schemas/User"))
                .isEqualTo("User");
    }

    @Test
    public void extractSchemaNameFromRef_returnsRefAsIsWhenNoSlash() {
        assertThat(PagedModelScanUtils.extractSchemaNameFromRef("User")).isEqualTo("User");
    }

    @Test
    public void extractSchemaNameFromRef_returnsNullForNull() {
        assertThat(PagedModelScanUtils.extractSchemaNameFromRef(null)).isNull();
    }
}
