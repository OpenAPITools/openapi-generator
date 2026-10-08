package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.Paths;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.IntegerSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import io.swagger.v3.oas.models.parameters.Parameter;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.TestUtils;
import org.testng.annotations.Test;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatCode;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

/**
 * Unit tests for {@link SpringPageableScanUtils}.
 */
public class SpringPageableScanUtilsTest {

    // -------------------------------------------------------------------------
    // Helpers
    // -------------------------------------------------------------------------

    @Test
    public void oneIndexedScanNormalizesEffectiveExclusiveBounds() {
        IntegerSchema page = new IntegerSchema();
        page.setDefault(3);
        page.setMinimum(BigDecimal.ZERO);
        page.setExclusiveMinimum(true);
        page.setMaximum(BigDecimal.TEN);
        page.setExclusiveMaximum(true);
        OpenAPI spec = buildPageableOperationWithParams(List.of(new Parameter().name("page").schema(page)));
        SpringPageableScanUtils utils = new SpringPageableScanUtils();
        utils.scanAll(spec, SpringPageableScanUtils.AutoPaginationMode.NONE, true, true, true, true);

        assertThat(utils.pageableDefaultsRegistry.get("listItems").page).isEqualTo(2);
        SpringPageableScanUtils.PageableConstraintsData bounds = utils.pageableConstraintsRegistry.get("listItems");
        assertThat(bounds.minPage).isZero();
        assertThat(bounds.maxPage).isEqualTo(8);
        assertThat(bounds.minSize).isEqualTo(-1);
        assertThat(bounds.maxSize).isEqualTo(-1);
    }

    @Test
    public void oneIndexedScanRoundsFractionalBoundsAgainstIntegerPages() {
        for (boolean exclusive : List.of(false, true)) {
            for (String minimum : List.of("0.5", "1.5")) {
                IntegerSchema page = new IntegerSchema();
                page.setMinimum(new BigDecimal(minimum));
                page.setMaximum(new BigDecimal("5.5"));
                page.setExclusiveMinimum(exclusive);
                page.setExclusiveMaximum(exclusive);
                OpenAPI spec = buildPageableOperationWithParams(List.of(new Parameter().name("page").schema(page)));
                SpringPageableScanUtils utils = new SpringPageableScanUtils();
                utils.scanAll(spec, SpringPageableScanUtils.AutoPaginationMode.NONE, true, true, true, true);

                SpringPageableScanUtils.PageableConstraintsData bounds = utils.pageableConstraintsRegistry.get("listItems");
                assertThat(bounds.minPage).isEqualTo(minimum.equals("0.5") ? 0 : 1);
                assertThat(bounds.maxPage).isEqualTo(4);
            }
        }
    }

    @Test
    public void oneIndexedScanLeavesAbsentPageBoundsUnconstrained() {
        IntegerSchema page = new IntegerSchema();
        page.setDefault(1);
        IntegerSchema size = new IntegerSchema();
        size.setMaximum(BigDecimal.TEN);
        OpenAPI spec = buildPageableOperationWithParams(List.of(
                new Parameter().name("page").schema(page), new Parameter().name("size").schema(size)));
        SpringPageableScanUtils utils = new SpringPageableScanUtils();
        utils.scanAll(spec, SpringPageableScanUtils.AutoPaginationMode.NONE, true, true, true, true);

        SpringPageableScanUtils.PageableConstraintsData bounds = utils.pageableConstraintsRegistry.get("listItems");
        assertThat(bounds.minPage).isEqualTo(-1);
        assertThat(bounds.maxPage).isEqualTo(-1);
        assertThat(bounds.maxSize).isEqualTo(10);
    }

    @Test
    public void oneIndexedScanRejectsExplicitNegativeBoundInsteadOfTreatingItAsAbsent() {
        IntegerSchema page = new IntegerSchema();
        page.setMinimum(BigDecimal.valueOf(-1));
        OpenAPI spec = buildPageableOperationWithParams(List.of(new Parameter().name("page").schema(page)));
        SpringPageableScanUtils utils = new SpringPageableScanUtils();
        assertThatThrownBy(() -> utils.scanAll(spec, SpringPageableScanUtils.AutoPaginationMode.NONE,
                false, true, true, true)).isInstanceOf(IllegalArgumentException.class)
                .hasMessageContaining("listItems").hasMessageContaining("effective minimum -1");
        assertThatCode(() -> utils.scanAll(spec, SpringPageableScanUtils.AutoPaginationMode.NONE,
                false, true, true, false)).doesNotThrowAnyException();
    }

    /**
     * Builds an OpenAPI doc with a single GET /items operation marked x-spring-paginated,
     * accepting an arbitrary list of parameters.
     */
    private static OpenAPI buildPageableOperationWithParams(List<Parameter> params) {
        Operation op = new Operation();
        op.setOperationId("listItems");
        op.addExtension("x-spring-paginated", true);
        params.forEach(op::addParametersItem);

        PathItem pathItem = new PathItem();
        pathItem.setGet(op);

        Paths paths = new Paths();
        paths.addPathItem("/items", pathItem);

        OpenAPI openAPI = new OpenAPI();
        openAPI.setPaths(paths);
        return openAPI;
    }

    // -------------------------------------------------------------------------
    // scanPageableConstraints — inclusive bounds (baseline)
    // -------------------------------------------------------------------------

    @Test
    public void scanPageableConstraints_inclusiveBounds_usedDirectly() {
        Schema<?> pageSchema = new IntegerSchema();
        pageSchema.setMaximum(BigDecimal.valueOf(100));
        pageSchema.setMinimum(BigDecimal.valueOf(0));

        Schema<?> sizeSchema = new IntegerSchema();
        sizeSchema.setMaximum(BigDecimal.valueOf(50));
        sizeSchema.setMinimum(BigDecimal.valueOf(1));

        OpenAPI openAPI = buildPageableOperationWithParams(List.of(
                new Parameter().name("page").schema(pageSchema),
                new Parameter().name("size").schema(sizeSchema)
        ));

        Map<String, SpringPageableScanUtils.PageableConstraintsData> result =
                SpringPageableScanUtils.scanPageableConstraints(openAPI, SpringPageableScanUtils.AutoPaginationMode.NONE);

        assertThat(result).containsKey("listItems");
        SpringPageableScanUtils.PageableConstraintsData data = result.get("listItems");
        assertThat(data.maxPage).isEqualTo(100);
        assertThat(data.minPage).isEqualTo(0);
        assertThat(data.maxSize).isEqualTo(50);
        assertThat(data.minSize).isEqualTo(1);
    }

    // -------------------------------------------------------------------------
    // scanPageableConstraints — exclusive bounds
    // -------------------------------------------------------------------------

    @Test
    public void scanPageableConstraints_exclusiveMaximum_subtractsOne() {
        Schema<?> pageSchema = new IntegerSchema();
        pageSchema.setMaximum(BigDecimal.valueOf(101));
        pageSchema.setExclusiveMaximum(Boolean.TRUE); // exclusive 101 → effective max = 100

        Schema<?> sizeSchema = new IntegerSchema();
        sizeSchema.setMaximum(BigDecimal.valueOf(51));
        sizeSchema.setExclusiveMaximum(Boolean.TRUE); // exclusive 51 → effective max = 50

        OpenAPI openAPI = buildPageableOperationWithParams(List.of(
                new Parameter().name("page").schema(pageSchema),
                new Parameter().name("size").schema(sizeSchema)
        ));

        Map<String, SpringPageableScanUtils.PageableConstraintsData> result =
                SpringPageableScanUtils.scanPageableConstraints(openAPI, SpringPageableScanUtils.AutoPaginationMode.NONE);

        assertThat(result).containsKey("listItems");
        SpringPageableScanUtils.PageableConstraintsData data = result.get("listItems");
        assertThat(data.maxPage).isEqualTo(100);
        assertThat(data.maxSize).isEqualTo(50);
    }

    @Test
    public void scanPageableConstraints_exclusiveMinimum_addsOne() {
        Schema<?> pageSchema = new IntegerSchema();
        pageSchema.setMinimum(BigDecimal.valueOf(-1));
        pageSchema.setExclusiveMinimum(Boolean.TRUE); // exclusive -1 → effective min = 0

        Schema<?> sizeSchema = new IntegerSchema();
        sizeSchema.setMinimum(BigDecimal.valueOf(0));
        sizeSchema.setExclusiveMinimum(Boolean.TRUE); // exclusive 0 → effective min = 1

        OpenAPI openAPI = buildPageableOperationWithParams(List.of(
                new Parameter().name("page").schema(pageSchema),
                new Parameter().name("size").schema(sizeSchema)
        ));

        Map<String, SpringPageableScanUtils.PageableConstraintsData> result =
                SpringPageableScanUtils.scanPageableConstraints(openAPI, SpringPageableScanUtils.AutoPaginationMode.NONE);

        assertThat(result).containsKey("listItems");
        SpringPageableScanUtils.PageableConstraintsData data = result.get("listItems");
        assertThat(data.minPage).isEqualTo(0);
        assertThat(data.minSize).isEqualTo(1);
    }

    @Test
    public void scanPageableConstraints_oas31NumericExclusive_subtractsOrAddsOne() {
        Schema<?> sizeSchema = new IntegerSchema();
        sizeSchema.setExclusiveMaximumValue(BigDecimal.valueOf(51)); // exclusive → effective max = 50
        sizeSchema.setExclusiveMinimumValue(BigDecimal.valueOf(0));  // exclusive → effective min = 1

        OpenAPI openAPI = buildPageableOperationWithParams(List.of(
                new Parameter().name("size").schema(sizeSchema)
        ));

        Map<String, SpringPageableScanUtils.PageableConstraintsData> result =
                SpringPageableScanUtils.scanPageableConstraints(openAPI, SpringPageableScanUtils.AutoPaginationMode.NONE);

        assertThat(result).containsKey("listItems");
        SpringPageableScanUtils.PageableConstraintsData data = result.get("listItems");
        assertThat(data.maxSize).isEqualTo(50);
        assertThat(data.minSize).isEqualTo(1);
    }

    /**
     * Builds an OpenAPI doc with a single GET /items operation marked x-spring-paginated.
     */
    private static OpenAPI buildPageableOperation(Parameter sortParam) {
        Operation op = new Operation();
        op.setOperationId("listItems");
        op.addExtension("x-spring-paginated", true);
        op.addParametersItem(sortParam);

        PathItem pathItem = new PathItem();
        pathItem.setGet(op);

        Paths paths = new Paths();
        paths.addPathItem("/items", pathItem);

        OpenAPI openAPI = new OpenAPI();
        openAPI.setPaths(paths);
        return openAPI;
    }

    // -------------------------------------------------------------------------
    // scanSortValidationEnums — NPE regression for array schema without items
    // -------------------------------------------------------------------------

    /**
     * Regression: array sort parameter with no {@code items} must not throw NPE.
     * {@code isArraySchema()} returns {@code true} but {@code schema.getItems()} returns
     * {@code null}, which would NPE on the subsequent {@code enumSchema.get$ref()} call
     * before the fix.
     *
     * <pre>
     * parameters:
     *   - name: sort
     *     in: query
     *     schema:
     *       type: array
     *       # items: intentionally absent
     * </pre>
     */
    @Test
    public void scanSortValidationEnums_arraySchemaWithNoItems_doesNotThrow_and_returnsEmptyMap() {
        // sort param: type=array but items intentionally absent
        Schema<?> sortSchema = new ArraySchema();
        // getItems() == null
        assertThat(sortSchema.getItems()).isNull();
        Parameter sortParam = new Parameter().name("sort").schema(sortSchema);
        OpenAPI openAPI = buildPageableOperation(sortParam);

        // does not throw NPE
        assertThatCode(() -> SpringPageableScanUtils.scanSortValidationEnums(openAPI, SpringPageableScanUtils.AutoPaginationMode.NONE))
                .doesNotThrowAnyException();

        // and returns empty map
        Map<String, List<String>> result = SpringPageableScanUtils.scanSortValidationEnums(openAPI, SpringPageableScanUtils.AutoPaginationMode.NONE);
        assertThat(result).isEmpty();
    }

    // -------------------------------------------------------------------------
    // scanSortValidationEnums — happy path
    // -------------------------------------------------------------------------

    /**
     * <pre>
     * parameters:
     *   - name: sort
     *     in: query
     *     schema:
     *       type: array # sort as multi-column
     *       items:
     *         type: string
     *         enum: ["name,asc", "name,desc", "id,asc"]
     * </pre>
     */
    @Test
    public void scanSortValidationEnums_arraySchemaWithEnumItems_returnsMappedEnums() {
        Schema<?> items = new StringSchema()._enum(List.of("name,asc", "name,desc", "id,asc"));
        Schema<?> sortSchema = new ArraySchema().items(items);
        Parameter sortParam = new Parameter().name("sort").schema(sortSchema);
        OpenAPI openAPI = buildPageableOperation(sortParam);

        Map<String, List<String>> result = SpringPageableScanUtils.scanSortValidationEnums(openAPI, SpringPageableScanUtils.AutoPaginationMode.NONE);
        assertThat(result)
                .containsKey("listItems")
                .satisfies(m -> assertThat(m.get("listItems"))
                        .containsExactly("name,asc", "name,desc", "id,asc"));
    }

    /**
     * <pre>
     * parameters:
     *   - name: sort
     *     in: query
     *     schema:
     *       type: string # sort as single-column
     *       enum: ["id,asc", "id,desc"]
     * </pre>
     */
    @Test
    public void scanSortValidationEnums_nonArraySortSchemaWithEnum_returnsIt() {
        Schema<?> sortSchema = new StringSchema()._enum(List.of("id,asc", "id,desc"));
        Parameter sortParam = new Parameter().name("sort").schema(sortSchema);
        OpenAPI openAPI = buildPageableOperation(sortParam);

        Map<String, List<String>> result = SpringPageableScanUtils.scanSortValidationEnums(openAPI, SpringPageableScanUtils.AutoPaginationMode.NONE);
        assertThat(result)
                .containsKey("listItems")
                .satisfies(m -> assertThat(m.get("listItems")).containsExactly("id,asc", "id,desc"));
    }

    /**
     * <pre>
     * parameters:
     *   - name: sort
     *     in: query
     *     schema:
     *       type: string # sort as single-column
     *       # enum: absent — no validation constraint
     * </pre>
     */
    @Test
    public void scanSortValidationEnums_sortSchemaWithNoEnum_returnsEmptyMap() {
        Parameter sortParam = new Parameter().name("sort").schema(new StringSchema());
        OpenAPI openAPI = buildPageableOperation(sortParam);

        assertThat(SpringPageableScanUtils.scanSortValidationEnums(openAPI, SpringPageableScanUtils.AutoPaginationMode.NONE)).isEmpty();
    }

    // -------------------------------------------------------------------------
    // applyAutoXSpringPaginatedIfNeeded
    // -------------------------------------------------------------------------

    @Test
    public void applyAutoXSpringPaginatedIfNeeded_allThreeParams_setsExtensionAndReturnsTrue() {
        Operation op = new Operation();
        op.addParametersItem(new Parameter().name("page").in("query"));
        op.addParametersItem(new Parameter().name("size").in("query"));
        op.addParametersItem(new Parameter().name("sort").in("query"));

        boolean result = SpringPageableScanUtils.applyAutoXSpringPaginatedIfNeeded(op, SpringPageableScanUtils.AutoPaginationMode.PAGE_SIZE_SORT);

        assertThat(result).isTrue();
        assertThat(op.getExtensions()).containsEntry("x-spring-paginated", Boolean.TRUE);
    }

    @Test
    public void applyAutoXSpringPaginatedIfNeeded_missingOneParam_doesNotSetExtension() {
        Operation op = new Operation();
        op.addParametersItem(new Parameter().name("page").in("query"));
        op.addParametersItem(new Parameter().name("size").in("query"));
        // 'sort' is absent

        boolean result = SpringPageableScanUtils.applyAutoXSpringPaginatedIfNeeded(op, SpringPageableScanUtils.AutoPaginationMode.PAGE_SIZE_SORT);

        assertThat(result).isFalse();
        assertThat(op.getExtensions()).isNull();
    }

    @Test
    public void applyAutoXSpringPaginatedIfNeeded_autoDisabled_doesNotSetExtension() {
        Operation op = new Operation();
        op.addParametersItem(new Parameter().name("page").in("query"));
        op.addParametersItem(new Parameter().name("size").in("query"));
        op.addParametersItem(new Parameter().name("sort").in("query"));

        boolean result = SpringPageableScanUtils.applyAutoXSpringPaginatedIfNeeded(op, SpringPageableScanUtils.AutoPaginationMode.NONE);

        assertThat(result).isFalse();
        assertThat(op.getExtensions()).isNull();
    }

    @Test
    public void applyAutoXSpringPaginatedIfNeeded_explicitlyTrue_returnsTrueWithoutMutation() {
        Operation op = new Operation();
        op.addExtension("x-spring-paginated", Boolean.TRUE);
        // No params needed — already explicitly set

        boolean result = SpringPageableScanUtils.applyAutoXSpringPaginatedIfNeeded(op, SpringPageableScanUtils.AutoPaginationMode.NONE);

        assertThat(result).isTrue();
        // Extension was already true and must remain true
        assertThat(op.getExtensions()).containsEntry("x-spring-paginated", Boolean.TRUE);
    }

    @Test
    public void applyAutoXSpringPaginatedIfNeeded_explicitlyFalse_returnsFalseAndIsNotOverridden() {
        Operation op = new Operation();
        op.addExtension("x-spring-paginated", Boolean.FALSE);
        op.addParametersItem(new Parameter().name("page").in("query"));
        op.addParametersItem(new Parameter().name("size").in("query"));
        op.addParametersItem(new Parameter().name("sort").in("query"));

        boolean result = SpringPageableScanUtils.applyAutoXSpringPaginatedIfNeeded(op, SpringPageableScanUtils.AutoPaginationMode.PAGE_SIZE_SORT);

        assertThat(result).isFalse();
        // Manual false must not be overridden by auto-detection
        assertThat(op.getExtensions()).containsEntry("x-spring-paginated", Boolean.FALSE);
    }

    @Test
    public void applyAutoXSpringPaginatedIfNeeded_noParams_doesNotSetExtension() {
        Operation op = new Operation();
        // No parameters at all

        boolean result = SpringPageableScanUtils.applyAutoXSpringPaginatedIfNeeded(op, SpringPageableScanUtils.AutoPaginationMode.PAGE_SIZE_SORT);

        assertThat(result).isFalse();
        assertThat(op.getExtensions()).isNull();
    }

    @Test
    public void referencedParameters_areResolvedForDetectionAndScans() {
        Schema<?> pageSchema = new IntegerSchema()
                .minimum(BigDecimal.ZERO)
                ._default(0);
        Schema<?> sizeSchema = new IntegerSchema()
                .minimum(BigDecimal.ONE)
                .maximum(BigDecimal.valueOf(100))
                ._default(20);
        Schema<?> sortSchema = new ArraySchema()
                .items(new StringSchema()._enum(List.of("name,asc", "name,desc")))
                ._default(List.of("name,desc"));

        Components components = new Components()
                .addParameters("Page", new Parameter().name("page").in("query").schema(pageSchema))
                .addParameters("Size", new Parameter().name("size").in("query").schema(sizeSchema))
                .addParameters("Sort", new Parameter().name("sort").in("query").schema(sortSchema));

        Operation operation = new Operation()
                .operationId("listItems")
                .addParametersItem(new Parameter().$ref("#/components/parameters/Page"))
                .addParametersItem(new Parameter().$ref("#/components/parameters/Size"))
                .addParametersItem(new Parameter().$ref("#/components/parameters/Sort"));
        OpenAPI openAPI = new OpenAPI()
                .components(components)
                .paths(new Paths().addPathItem("/items", new PathItem().get(operation)));

        assertThat(SpringPageableScanUtils.applyAutoXSpringPaginatedIfNeeded(openAPI, operation, SpringPageableScanUtils.AutoPaginationMode.PAGE_SIZE_SORT))
                .isTrue();
        assertThat(operation.getExtensions()).containsEntry("x-spring-paginated", Boolean.TRUE);

        Map<String, SpringPageableScanUtils.PageableDefaultsData> defaults =
                SpringPageableScanUtils.scanPageableDefaults(openAPI, SpringPageableScanUtils.AutoPaginationMode.PAGE_SIZE_SORT);
        assertThat(defaults).containsKey("listItems");
        assertThat(defaults.get("listItems").page).isEqualTo(0);
        assertThat(defaults.get("listItems").size).isEqualTo(20);
        assertThat(defaults.get("listItems").sortDefaults)
                .extracting(defaultValue -> defaultValue.field, defaultValue -> defaultValue.direction)
                .containsExactly(org.assertj.core.groups.Tuple.tuple("name", "DESC"));

        Map<String, SpringPageableScanUtils.PageableConstraintsData> constraints =
                SpringPageableScanUtils.scanPageableConstraints(openAPI, SpringPageableScanUtils.AutoPaginationMode.PAGE_SIZE_SORT);
        assertThat(constraints).containsKey("listItems");
        assertThat(constraints.get("listItems").minPage).isEqualTo(0);
        assertThat(constraints.get("listItems").minSize).isEqualTo(1);
        assertThat(constraints.get("listItems").maxSize).isEqualTo(100);

        assertThat(SpringPageableScanUtils.scanSortValidationEnums(openAPI, SpringPageableScanUtils.AutoPaginationMode.PAGE_SIZE_SORT))
                .containsEntry("listItems", List.of("name,asc", "name,desc"));
    }

    @Test
    public void unresolvedParameterReferences_doNotTriggerAutoDetection() {
        Operation operation = new Operation()
                .addParametersItem(new Parameter().$ref("#/components/parameters/Page"))
                .addParametersItem(new Parameter().$ref("#/components/parameters/Size"))
                .addParametersItem(new Parameter().$ref("#/components/parameters/Sort"));
        OpenAPI openAPI = new OpenAPI().components(new Components());

        assertThat(SpringPageableScanUtils.applyAutoXSpringPaginatedIfNeeded(openAPI, operation, SpringPageableScanUtils.AutoPaginationMode.PAGE_SIZE_SORT))
                .isFalse();
        assertThat(operation.getExtensions()).isNull();
    }

    // -------------------------------------------------------------------------
    // resolveAutoPaginationMode
    // -------------------------------------------------------------------------

    @Test
    public void resolveAutoPaginationMode_canonicalValues() {
        assertThat(SpringPageableScanUtils.resolveAutoPaginationMode("none"))
                .isEqualTo(SpringPageableScanUtils.AutoPaginationMode.NONE);
        assertThat(SpringPageableScanUtils.resolveAutoPaginationMode("page-size-sort"))
                .isEqualTo(SpringPageableScanUtils.AutoPaginationMode.PAGE_SIZE_SORT);
        assertThat(SpringPageableScanUtils.resolveAutoPaginationMode("page-size"))
                .isEqualTo(SpringPageableScanUtils.AutoPaginationMode.PAGE_SIZE);
    }

    @Test
    public void resolveAutoPaginationMode_caseInsensitiveAndTrimmed() {
        assertThat(SpringPageableScanUtils.resolveAutoPaginationMode("  PAGE-SIZE  "))
                .isEqualTo(SpringPageableScanUtils.AutoPaginationMode.PAGE_SIZE);
    }

    @Test
    public void resolveAutoPaginationMode_legacyAliases() {
        assertThat(SpringPageableScanUtils.resolveAutoPaginationMode("true"))
                .isEqualTo(SpringPageableScanUtils.AutoPaginationMode.PAGE_SIZE_SORT);
        assertThat(SpringPageableScanUtils.resolveAutoPaginationMode("false"))
                .isEqualTo(SpringPageableScanUtils.AutoPaginationMode.NONE);
    }

    @Test
    public void resolveAutoPaginationMode_invalidValue_throwsIllegalArgumentExceptionListingAcceptedValues() {
        assertThatThrownBy(() -> SpringPageableScanUtils.resolveAutoPaginationMode("bogus"))
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessageContaining("bogus")
                .hasMessageContaining("none")
                .hasMessageContaining("page-size-sort")
                .hasMessageContaining("page-size");
    }

    // -------------------------------------------------------------------------
    // warnIfDeprecatedAutoPaginationValue
    // -------------------------------------------------------------------------

    @Test
    public void warnIfDeprecatedAutoPaginationValue_legacyTrue_warnsWithMigrationHint() {
        SpringPageableScanUtils utils = new SpringPageableScanUtils();
        List<String> messages = TestUtils.captureLogMessages(SpringPageableScanUtils.class,
                () -> utils.warnIfDeprecatedAutoPaginationValue("true"));
        assertThat(messages).singleElement(org.assertj.core.api.InstanceOfAssertFactories.STRING)
                .contains("'true' is deprecated").contains("'page-size-sort'");
    }

    @Test
    public void warnIfDeprecatedAutoPaginationValue_legacyFalse_warnsWithMigrationHint() {
        SpringPageableScanUtils utils = new SpringPageableScanUtils();
        List<String> messages = TestUtils.captureLogMessages(SpringPageableScanUtils.class,
                () -> utils.warnIfDeprecatedAutoPaginationValue("false"));
        assertThat(messages).singleElement(org.assertj.core.api.InstanceOfAssertFactories.STRING)
                .contains("'false' is deprecated").contains("'none'");
    }

    @Test
    public void warnIfDeprecatedAutoPaginationValue_normalizesLikeResolve() {
        SpringPageableScanUtils utils = new SpringPageableScanUtils();
        List<String> messages = TestUtils.captureLogMessages(SpringPageableScanUtils.class,
                () -> utils.warnIfDeprecatedAutoPaginationValue(" TRUE "));
        assertThat(messages).singleElement(org.assertj.core.api.InstanceOfAssertFactories.STRING)
                .contains("'true' is deprecated");
    }

    @Test
    public void warnIfDeprecatedAutoPaginationValue_nonLegacyValues_doNotWarn() {
        SpringPageableScanUtils utils = new SpringPageableScanUtils();
        List<String> messages = TestUtils.captureLogMessages(SpringPageableScanUtils.class, () -> {
            utils.warnIfDeprecatedAutoPaginationValue("none");
            utils.warnIfDeprecatedAutoPaginationValue("page-size-sort");
            utils.warnIfDeprecatedAutoPaginationValue("page-size");
            utils.warnIfDeprecatedAutoPaginationValue("bogus");
            utils.warnIfDeprecatedAutoPaginationValue(null);
        });
        assertThat(messages).isEmpty();
    }

    // -------------------------------------------------------------------------
    // willBePageable / detection with AutoPaginationMode.PAGE_SIZE
    // -------------------------------------------------------------------------

    @Test
    public void willBePageable_pageSizeMode_detectsPageAndSizeOnlyOperation() {
        Operation op = new Operation();
        op.addParametersItem(new Parameter().name("page").in("query"));
        op.addParametersItem(new Parameter().name("size").in("query"));
        // no 'sort' parameter present

        assertThat(SpringPageableScanUtils.willBePageable(op, SpringPageableScanUtils.AutoPaginationMode.PAGE_SIZE))
                .isTrue();
    }

    @Test
    public void willBePageable_pageSizeMode_alsoDetectsPageSizeAndSortOperation() {
        Operation op = new Operation();
        op.addParametersItem(new Parameter().name("page").in("query"));
        op.addParametersItem(new Parameter().name("size").in("query"));
        op.addParametersItem(new Parameter().name("sort").in("query"));

        assertThat(SpringPageableScanUtils.willBePageable(op, SpringPageableScanUtils.AutoPaginationMode.PAGE_SIZE))
                .isTrue();
    }

    @Test
    public void willBePageable_pageSizeMode_doesNotDetectPathAndHeaderParamsNamedPageAndSize() {
        Operation op = new Operation();
        op.addParametersItem(new Parameter().name("page").in("path"));
        op.addParametersItem(new Parameter().name("size").in("header"));
        // 'page'/'size' exist but are not query parameters — must NOT trigger auto-detection

        assertThat(SpringPageableScanUtils.willBePageable(op, SpringPageableScanUtils.AutoPaginationMode.PAGE_SIZE))
                .isFalse();
    }

    @Test
    public void willBePageable_pageSizeSortMode_doesNotDetectPageAndSizeOnlyOperation() {
        Operation op = new Operation();
        op.addParametersItem(new Parameter().name("page").in("query"));
        op.addParametersItem(new Parameter().name("size").in("query"));
        // no 'sort' parameter present — PAGE_SIZE_SORT must NOT detect this (regression guard)

        assertThat(SpringPageableScanUtils.willBePageable(op, SpringPageableScanUtils.AutoPaginationMode.PAGE_SIZE_SORT))
                .isFalse();
    }

    @Test
    public void willBePageable_pageSizeSortMode_doesNotDetectPathParamNamedSort() {
        Operation op = new Operation();
        op.addParametersItem(new Parameter().name("page").in("query"));
        op.addParametersItem(new Parameter().name("size").in("query"));
        op.addParametersItem(new Parameter().name("sort").in("path"));
        // 'sort' exists but is a path parameter, not a query parameter — before the query-location
        // filter was introduced this incorrectly returned true (regression guard)

        assertThat(SpringPageableScanUtils.willBePageable(op, SpringPageableScanUtils.AutoPaginationMode.PAGE_SIZE_SORT))
                .isFalse();
    }

    // -------------------------------------------------------------------------
    // applyPageableAnnotations
    // -------------------------------------------------------------------------

    private static CodegenOperation minimalOp(String operationId) {
        CodegenOperation op = new CodegenOperation();
        op.operationId = operationId;
        return op;
    }

    @Test
    public void applyPageableAnnotations_validPageable_java_formatsWithAttrs() {
        CodegenOperation op = minimalOp("listItems");
        SpringPageableScanUtils.PageableConstraintsData constraints =
                new SpringPageableScanUtils.PageableConstraintsData(100, 50, 0, 1);
        Map<String, SpringPageableScanUtils.PageableConstraintsData> registry =
                Collections.singletonMap("listItems", constraints);

        SpringPageableScanUtils.applyPageableAnnotations(op, true, true, registry,
                false, Collections.emptyMap(), Collections.emptyMap(),
                SpringPageableScanUtils.AnnotationSyntax.JAVA);

        assertThat(op.vendorExtensions).containsKey("x-pageable-extra-annotation");
        List<String> annotations = (List<String>) op.vendorExtensions.get("x-pageable-extra-annotation");
        assertThat(annotations).hasSize(1);
        assertThat(annotations.get(0))
                .startsWith("@ValidPageable(")
                .contains("maxPage = 100")
                .contains("maxSize = 50")
                .contains("minPage = 0")
                .contains("minSize = 1");
        assertThat(op.imports).contains("ValidPageable");
    }

    @Test
    public void applyPageableAnnotations_validSort_javaSyntax_usesCurlyBraces() {
        CodegenOperation op = minimalOp("listItems");
        Map<String, List<String>> sortEnums = Collections.singletonMap("listItems",
                List.of("name,asc", "name,desc"));

        SpringPageableScanUtils.applyPageableAnnotations(op, false, true, Collections.emptyMap(),
                true, sortEnums, Collections.emptyMap(),
                SpringPageableScanUtils.AnnotationSyntax.JAVA);

        List<String> annotations = (List<String>) op.vendorExtensions.get("x-pageable-extra-annotation");
        assertThat(annotations).hasSize(1);
        assertThat(annotations.get(0))
                .isEqualTo("@ValidSort(allowedValues = {\"name,asc\", \"name,desc\"})");
        assertThat(op.imports).contains("ValidSort");
    }

    @Test
    public void applyPageableAnnotations_validSort_kotlinSyntax_usesSquareBrackets() {
        CodegenOperation op = minimalOp("listItems");
        Map<String, List<String>> sortEnums = Collections.singletonMap("listItems",
                List.of("name,asc", "name,desc"));

        SpringPageableScanUtils.applyPageableAnnotations(op, false, true, Collections.emptyMap(),
                true, sortEnums, Collections.emptyMap(),
                SpringPageableScanUtils.AnnotationSyntax.KOTLIN);

        List<String> annotations = (List<String>) op.vendorExtensions.get("x-pageable-extra-annotation");
        assertThat(annotations).hasSize(1);
        assertThat(annotations.get(0))
                .isEqualTo("@ValidSort(allowedValues = [\"name,asc\", \"name,desc\"])");
    }

    @Test
    public void applyPageableAnnotations_pageableDefault_pageAndSize() {
        CodegenOperation op = minimalOp("listItems");
        SpringPageableScanUtils.PageableDefaultsData defaults =
                new SpringPageableScanUtils.PageableDefaultsData(0, 20, Collections.emptyList());
        Map<String, SpringPageableScanUtils.PageableDefaultsData> registry =
                Collections.singletonMap("listItems", defaults);

        SpringPageableScanUtils.applyPageableAnnotations(op, false, false, Collections.emptyMap(),
                false, Collections.emptyMap(), registry,
                SpringPageableScanUtils.AnnotationSyntax.JAVA);

        List<String> annotations = (List<String>) op.vendorExtensions.get("x-pageable-extra-annotation");
        assertThat(annotations).hasSize(1);
        assertThat(annotations.get(0)).isEqualTo("@PageableDefault(page = 0, size = 20)");
        assertThat(op.imports).contains("PageableDefault");
    }

    @Test
    public void applyPageableAnnotations_sortDefault_javaSyntax() {
        CodegenOperation op = minimalOp("listItems");
        List<SpringPageableScanUtils.SortFieldDefault> sortFields = List.of(
                new SpringPageableScanUtils.SortFieldDefault("name", "ASC"),
                new SpringPageableScanUtils.SortFieldDefault("id", "DESC")
        );
        SpringPageableScanUtils.PageableDefaultsData defaults =
                new SpringPageableScanUtils.PageableDefaultsData(null, null, sortFields);
        Map<String, SpringPageableScanUtils.PageableDefaultsData> registry =
                Collections.singletonMap("listItems", defaults);

        SpringPageableScanUtils.applyPageableAnnotations(op, false, false, Collections.emptyMap(),
                false, Collections.emptyMap(), registry,
                SpringPageableScanUtils.AnnotationSyntax.JAVA);

        List<String> annotations = (List<String>) op.vendorExtensions.get("x-pageable-extra-annotation");
        assertThat(annotations).hasSize(1);
        assertThat(annotations.get(0))
                .isEqualTo("@SortDefault.SortDefaults({" +
                        "@SortDefault(sort = {\"name\"}, direction = Sort.Direction.ASC), " +
                        "@SortDefault(sort = {\"id\"}, direction = Sort.Direction.DESC)})");
        assertThat(op.imports).containsAll(List.of("SortDefault", "Sort"));
    }

    @Test
    public void applyPageableAnnotations_sortDefault_kotlinSyntax() {
        CodegenOperation op = minimalOp("listItems");
        List<SpringPageableScanUtils.SortFieldDefault> sortFields = List.of(
                new SpringPageableScanUtils.SortFieldDefault("name", "ASC")
        );
        SpringPageableScanUtils.PageableDefaultsData defaults =
                new SpringPageableScanUtils.PageableDefaultsData(null, null, sortFields);
        Map<String, SpringPageableScanUtils.PageableDefaultsData> registry =
                Collections.singletonMap("listItems", defaults);

        SpringPageableScanUtils.applyPageableAnnotations(op, false, false, Collections.emptyMap(),
                false, Collections.emptyMap(), registry,
                SpringPageableScanUtils.AnnotationSyntax.KOTLIN);

        List<String> annotations = (List<String>) op.vendorExtensions.get("x-pageable-extra-annotation");
        assertThat(annotations).hasSize(1);
        assertThat(annotations.get(0))
                .isEqualTo("@SortDefault.SortDefaults(SortDefault(sort = [\"name\"], direction = Sort.Direction.ASC))");
    }

    @Test
    public void applyPageableAnnotations_usesOriginalOperationIdForRegistryLookup() {
        CodegenOperation op = minimalOp("listItems");
        op.operationIdOriginal = "list-items";

        Map<String, SpringPageableScanUtils.PageableConstraintsData> constraintsRegistry =
                Collections.singletonMap("list-items", new SpringPageableScanUtils.PageableConstraintsData(50, 100, -1, -1));
        Map<String, List<String>> sortValidationRegistry =
                Collections.singletonMap("list-items", List.of("id,asc", "name,desc"));
        Map<String, SpringPageableScanUtils.PageableDefaultsData> defaultsRegistry =
                Collections.singletonMap("list-items", new SpringPageableScanUtils.PageableDefaultsData(
                        0, 25, List.of(new SpringPageableScanUtils.SortFieldDefault("name", "DESC"))));

        SpringPageableScanUtils.applyPageableAnnotations(op, true, true, constraintsRegistry,
                true, sortValidationRegistry, defaultsRegistry,
                SpringPageableScanUtils.AnnotationSyntax.JAVA);

        List<String> annotations = (List<String>) op.vendorExtensions.get("x-pageable-extra-annotation");
        assertThat(annotations).containsExactly(
                "@ValidPageable(maxSize = 100, maxPage = 50)",
                "@ValidSort(allowedValues = {\"id,asc\", \"name,desc\"})",
                "@PageableDefault(page = 0, size = 25)",
                "@SortDefault.SortDefaults({@SortDefault(sort = {\"name\"}, direction = Sort.Direction.DESC)})");
    }

    @Test
    public void applyPageableAnnotations_noMatchingRegistryEntries_noAnnotationsAdded() {
        CodegenOperation op = minimalOp("someOtherOp");

        SpringPageableScanUtils.applyPageableAnnotations(op, true, true,
                Collections.singletonMap("differentOp", new SpringPageableScanUtils.PageableConstraintsData(10, 5, 0, 1)),
                true,
                Collections.singletonMap("differentOp", List.of("id,asc")),
                Collections.singletonMap("differentOp", new SpringPageableScanUtils.PageableDefaultsData(0, 10, Collections.emptyList())),
                SpringPageableScanUtils.AnnotationSyntax.JAVA);

        assertThat(op.vendorExtensions).doesNotContainKey("x-pageable-extra-annotation");
        assertThat(op.imports).isEmpty();
    }

    // -------------------------------------------------------------------------
    // applySpringDocPageableAnnotation
    // -------------------------------------------------------------------------

    @Test
    public void applySpringDocPageableAnnotation_javaSyntax_springDoc_addsParameterObjectImport() {
        CodegenOperation op = minimalOp("listItems");

        SpringPageableScanUtils.applySpringDocPageableAnnotation(
                op, SpringPageableScanUtils.AnnotationSyntax.JAVA, true);

        assertThat(op.imports).contains("ParameterObject");
        assertThat(op.imports).doesNotContain("PageableAsQueryParam");
        assertThat(op.vendorExtensions).doesNotContainKey("x-operation-extra-annotation");
    }

    @Test
    public void applySpringDocPageableAnnotation_kotlinSyntax_springDoc_addsImportAndPrependsAnnotation() {
        CodegenOperation op = minimalOp("listItems");

        SpringPageableScanUtils.applySpringDocPageableAnnotation(
                op, SpringPageableScanUtils.AnnotationSyntax.KOTLIN, true);

        assertThat(op.imports).contains("PageableAsQueryParam");
        assertThat(op.imports).doesNotContain("ParameterObject");
        List<String> extraAnnotations = (List<String>) op.vendorExtensions.get("x-operation-extra-annotation");
        assertThat(extraAnnotations).containsExactly("@PageableAsQueryParam");
    }

    @Test
    public void applySpringDocPageableAnnotation_kotlinSyntax_springDoc_prependsToExistingAnnotations() {
        CodegenOperation op = minimalOp("listItems");
        List<String> existing = new ArrayList<>();
        existing.add("@PreAuthorize(\"hasRole('ADMIN')\")");
        op.vendorExtensions.put("x-operation-extra-annotation", existing);

        SpringPageableScanUtils.applySpringDocPageableAnnotation(
                op, SpringPageableScanUtils.AnnotationSyntax.KOTLIN, true);

        List<String> extraAnnotations = (List<String>) op.vendorExtensions.get("x-operation-extra-annotation");
        assertThat(extraAnnotations).containsExactly("@PageableAsQueryParam", "@PreAuthorize(\"hasRole('ADMIN')\")");
    }

    @Test
    public void applySpringDocPageableAnnotation_notSpringDoc_isNoOp() {
        CodegenOperation op = minimalOp("listItems");

        SpringPageableScanUtils.applySpringDocPageableAnnotation(
                op, SpringPageableScanUtils.AnnotationSyntax.KOTLIN, false);

        assertThat(op.imports).isEmpty();
        assertThat(op.vendorExtensions).doesNotContainKey("x-operation-extra-annotation");
    }

    // -------------------------------------------------------------------------
    // Instance: scanAll + applyPageableAnnotations
    // -------------------------------------------------------------------------

    @Test
    public void scanAll_populatesInstanceMaps() {
        Parameter pageParam = new Parameter().name("page").schema(new IntegerSchema());
        Parameter sizeParam = new Parameter().name("size").schema(new IntegerSchema());
        Parameter sortParam = new Parameter().name("sort").schema(
                new StringSchema().addEnumItem("name,asc").addEnumItem("name,desc"));
        OpenAPI openAPI = buildPageableOperationWithParams(List.of(pageParam, sizeParam, sortParam));

        SpringPageableScanUtils utils = new SpringPageableScanUtils();
        utils.scanAll(openAPI, SpringPageableScanUtils.AutoPaginationMode.NONE); // auto-detect disabled; x-spring-paginated already set

        assertThat(utils.sortValidationEnums).containsKey("listItems");
        assertThat(utils.sortValidationEnums.get("listItems")).containsExactly("name,asc", "name,desc");
        // No page/size defaults or constraints in this spec
        assertThat(utils.pageableDefaultsRegistry).doesNotContainKey("listItems");
        assertThat(utils.pageableConstraintsRegistry).doesNotContainKey("listItems");
    }

    @Test
    public void instanceApplyPageableAnnotations_usesStoredMaps() {
        CodegenOperation op = minimalOp("listItems");
        Map<String, List<String>> sortEnums = Collections.singletonMap("listItems", List.of("id,asc"));

        SpringPageableScanUtils utils = new SpringPageableScanUtils();
        utils.sortValidationEnums = sortEnums;

        utils.applyPageableAnnotations(op, false, true, true, SpringPageableScanUtils.AnnotationSyntax.JAVA);

        List<String> annotations = (List<String>) op.vendorExtensions.get("x-pageable-extra-annotation");
        assertThat(annotations).hasSize(1);
        assertThat(annotations.get(0)).isEqualTo("@ValidSort(allowedValues = {\"id,asc\"})");
        assertThat(op.imports).contains("ValidSort");
    }
    @Test
    public void zeroIndexedPageBoundsRoundAgainstIntegerDomain() {
        for (boolean exclusiveMinimum : List.of(false, true)) {
            for (boolean exclusiveMaximum : List.of(false, true)) {
                IntegerSchema page = new IntegerSchema();
                page.setMinimum(new BigDecimal("0.5"));
                page.setMaximum(new BigDecimal("2.5"));
                page.setExclusiveMinimum(exclusiveMinimum);
                page.setExclusiveMaximum(exclusiveMaximum);
                OpenAPI spec = buildPageableOperationWithParams(List.of(new Parameter().name("page").schema(page)));
                SpringPageableScanUtils.PageableConstraintsData bounds = SpringPageableScanUtils
                        .scanPageableConstraints(spec, SpringPageableScanUtils.AutoPaginationMode.NONE).get("listItems");
                assertThat(bounds.minPage).isEqualTo(1);
                assertThat(bounds.maxPage).isEqualTo(2);
            }
        }
    }

    @Test
    public void sizeBoundsPreserveIntegerDomainRegardlessOfIndexing() {
        for (boolean oneIndexed : List.of(false, true)) {
            for (boolean exclusiveMinimum : List.of(false, true)) {
                for (boolean exclusiveMaximum : List.of(false, true)) {
                    for (boolean fractional : List.of(false, true)) {
                        IntegerSchema size = new IntegerSchema();
                        size.setMinimum(new BigDecimal(fractional ? "1.5" : "1"));
                        size.setMaximum(new BigDecimal(fractional ? "5.5" : "5"));
                        size.setExclusiveMinimum(exclusiveMinimum);
                        size.setExclusiveMaximum(exclusiveMaximum);
                        OpenAPI spec = buildPageableOperationWithParams(List.of(new Parameter().name("size").schema(size)));
                        SpringPageableScanUtils utils = new SpringPageableScanUtils();
                        utils.scanAll(spec, SpringPageableScanUtils.AutoPaginationMode.NONE,
                                true, oneIndexed, true, true);
                        SpringPageableScanUtils.PageableConstraintsData bounds = utils.pageableConstraintsRegistry.get("listItems");
                        assertThat(bounds.minSize).isEqualTo(fractional || exclusiveMinimum ? 2 : 1);
                        assertThat(bounds.maxSize).isEqualTo(!fractional && exclusiveMaximum ? 4 : 5);
                        assertThat(bounds.minPage).isEqualTo(-1);
                        assertThat(bounds.maxPage).isEqualTo(-1);
                    }
                }
            }
        }
    }

    @Test
    public void absentSizeBoundsRemainUnconstrained() {
        OpenAPI spec = buildPageableOperationWithParams(List.of(new Parameter().name("size").schema(new IntegerSchema())));
        for (boolean oneIndexed : List.of(false, true)) {
            SpringPageableScanUtils utils = new SpringPageableScanUtils();
            utils.scanAll(spec, SpringPageableScanUtils.AutoPaginationMode.NONE, true, oneIndexed, true, true);
            assertThat(utils.pageableConstraintsRegistry).isEmpty();
        }
    }

    @Test
    public void overflowingSizeBoundsIdentifyOperationParameterAndAttribute() {
        for (boolean maximum : List.of(false, true)) {
            IntegerSchema size = new IntegerSchema();
            if (maximum) {
                size.setMaximum(new BigDecimal("2147483648"));
            } else {
                size.setMinimum(new BigDecimal("2147483648"));
            }
            OpenAPI spec = buildPageableOperationWithParams(List.of(new Parameter().name("size").schema(size)));
            for (boolean oneIndexed : List.of(false, true)) {
                SpringPageableScanUtils utils = new SpringPageableScanUtils();
                assertThatThrownBy(() -> utils.scanAll(spec, SpringPageableScanUtils.AutoPaginationMode.NONE,
                        true, oneIndexed, true, true)).isInstanceOf(IllegalArgumentException.class)
                        .hasMessageContaining("listItems").hasMessageContaining("size")
                        .hasMessageContaining(maximum ? "maximum" : "minimum")
                        .hasMessageContaining("2147483648").hasMessageContaining("integer range");
            }
        }
    }

    @Test
    public void overflowingPageBoundsIdentifyOperationAndAttribute() {
        for (boolean maximum : List.of(false, true)) {
            IntegerSchema page = new IntegerSchema();
            if (maximum) {
                page.setMaximum(new BigDecimal("2147483648"));
            } else {
                page.setMinimum(new BigDecimal("2147483648"));
            }
            OpenAPI spec = buildPageableOperationWithParams(List.of(new Parameter().name("page").schema(page)));
            for (boolean oneIndexed : List.of(false, true)) {
                SpringPageableScanUtils utils = new SpringPageableScanUtils();
                assertThatThrownBy(() -> utils.scanAll(spec, SpringPageableScanUtils.AutoPaginationMode.NONE,
                        true, oneIndexed, true, true)).isInstanceOf(IllegalArgumentException.class)
                        .hasMessageContaining("listItems")
                        .hasMessageContaining(maximum ? "maximum" : "minimum")
                        .hasMessageContaining("2147483648").hasMessageContaining("integer range");
            }
        }
    }
}
