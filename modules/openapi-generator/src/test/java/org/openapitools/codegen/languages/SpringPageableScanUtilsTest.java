package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.Paths;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import io.swagger.v3.oas.models.parameters.Parameter;
import org.testng.annotations.Test;

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
