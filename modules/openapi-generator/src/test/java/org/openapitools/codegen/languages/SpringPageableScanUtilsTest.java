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
import org.testng.annotations.Test;

import java.math.BigDecimal;
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
}
