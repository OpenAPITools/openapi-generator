package org.openapitools.codegen.languages;

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
import org.testng.annotations.Test;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatCode;

/**
 * Unit tests for {@link SpringPageableScanUtils}.
 */
public class SpringPageableScanUtilsTest {

    // -------------------------------------------------------------------------
    // Helpers
    // -------------------------------------------------------------------------

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
                SpringPageableScanUtils.scanPageableConstraints(openAPI, false);

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
                SpringPageableScanUtils.scanPageableConstraints(openAPI, false);

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
                SpringPageableScanUtils.scanPageableConstraints(openAPI, false);

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
                SpringPageableScanUtils.scanPageableConstraints(openAPI, false);

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
        assertThatCode(() -> SpringPageableScanUtils.scanSortValidationEnums(openAPI, false))
                .doesNotThrowAnyException();

        // and returns empty map
        Map<String, List<String>> result = SpringPageableScanUtils.scanSortValidationEnums(openAPI, false);
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

        Map<String, List<String>> result = SpringPageableScanUtils.scanSortValidationEnums(openAPI, false);
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

        Map<String, List<String>> result = SpringPageableScanUtils.scanSortValidationEnums(openAPI, false);
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

        assertThat(SpringPageableScanUtils.scanSortValidationEnums(openAPI, false)).isEmpty();
    }

    // -------------------------------------------------------------------------
    // applyAutoXSpringPaginatedIfNeeded
    // -------------------------------------------------------------------------

    @Test
    public void applyAutoXSpringPaginatedIfNeeded_allThreeParams_setsExtensionAndReturnsTrue() {
        Operation op = new Operation();
        op.addParametersItem(new Parameter().name("page"));
        op.addParametersItem(new Parameter().name("size"));
        op.addParametersItem(new Parameter().name("sort"));

        boolean result = SpringPageableScanUtils.applyAutoXSpringPaginatedIfNeeded(op, true);

        assertThat(result).isTrue();
        assertThat(op.getExtensions()).containsEntry("x-spring-paginated", Boolean.TRUE);
    }

    @Test
    public void applyAutoXSpringPaginatedIfNeeded_missingOneParam_doesNotSetExtension() {
        Operation op = new Operation();
        op.addParametersItem(new Parameter().name("page"));
        op.addParametersItem(new Parameter().name("size"));
        // 'sort' is absent

        boolean result = SpringPageableScanUtils.applyAutoXSpringPaginatedIfNeeded(op, true);

        assertThat(result).isFalse();
        assertThat(op.getExtensions()).isNull();
    }

    @Test
    public void applyAutoXSpringPaginatedIfNeeded_autoDisabled_doesNotSetExtension() {
        Operation op = new Operation();
        op.addParametersItem(new Parameter().name("page"));
        op.addParametersItem(new Parameter().name("size"));
        op.addParametersItem(new Parameter().name("sort"));

        boolean result = SpringPageableScanUtils.applyAutoXSpringPaginatedIfNeeded(op, false);

        assertThat(result).isFalse();
        assertThat(op.getExtensions()).isNull();
    }

    @Test
    public void applyAutoXSpringPaginatedIfNeeded_explicitlyTrue_returnsTrueWithoutMutation() {
        Operation op = new Operation();
        op.addExtension("x-spring-paginated", Boolean.TRUE);
        // No params needed — already explicitly set

        boolean result = SpringPageableScanUtils.applyAutoXSpringPaginatedIfNeeded(op, false);

        assertThat(result).isTrue();
        // Extension was already true and must remain true
        assertThat(op.getExtensions()).containsEntry("x-spring-paginated", Boolean.TRUE);
    }

    @Test
    public void applyAutoXSpringPaginatedIfNeeded_explicitlyFalse_returnsFalseAndIsNotOverridden() {
        Operation op = new Operation();
        op.addExtension("x-spring-paginated", Boolean.FALSE);
        op.addParametersItem(new Parameter().name("page"));
        op.addParametersItem(new Parameter().name("size"));
        op.addParametersItem(new Parameter().name("sort"));

        boolean result = SpringPageableScanUtils.applyAutoXSpringPaginatedIfNeeded(op, true);

        assertThat(result).isFalse();
        // Manual false must not be overridden by auto-detection
        assertThat(op.getExtensions()).containsEntry("x-spring-paginated", Boolean.FALSE);
    }

    @Test
    public void applyAutoXSpringPaginatedIfNeeded_noParams_doesNotSetExtension() {
        Operation op = new Operation();
        // No parameters at all

        boolean result = SpringPageableScanUtils.applyAutoXSpringPaginatedIfNeeded(op, true);

        assertThat(result).isFalse();
        assertThat(op.getExtensions()).isNull();
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
        utils.scanAll(openAPI, false); // auto-detect disabled; x-spring-paginated already set

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
}
