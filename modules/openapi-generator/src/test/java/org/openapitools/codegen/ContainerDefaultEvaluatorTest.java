package org.openapitools.codegen;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.ByteArraySchema;
import io.swagger.v3.oas.models.media.MapSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import org.openapitools.codegen.languages.JavaClientCodegen;
import org.testng.annotations.Test;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;

public class ContainerDefaultEvaluatorTest {

    @Test
    public void compatibility_7_5_0_shortcut() {
        testAll("7.5.0", ContainerDefaultEvaluator.DEFAULT_7_5_0);
    }

    @Test
    public void compatibility_7_5_0_shortcut_and_override() {
        testAll("7.5.0 | !required & !nullable", true, true, false, true, false, false);
    }


    @Test
    public void compatibility_7_5_0() {
        testAll("!required&nullable | required&nullable", ContainerDefaultEvaluator.DEFAULT_7_5_0);
    }

    @Test
    public void compatibility_7_4_0() {
        testAll("!required&!nullable | !required&nullable | required&nullable | !required&?nullable", ContainerDefaultEvaluator.DEFAULT_7_4_0);
    }

    @Test
    public void compatibility_map_7_5_array_7_4_set_all() {
        testAll("map: 7.5.0 ; array: 7.4.0 ; set: all", Map.of(
                ContainerDefaultEvaluator.ContainerType.map, ContainerDefaultEvaluator.DEFAULT_7_5_0,
                ContainerDefaultEvaluator.ContainerType.array, ContainerDefaultEvaluator.DEFAULT_7_4_0,
                ContainerDefaultEvaluator.ContainerType.set, ContainerDefaultEvaluator.ALL));
    }

    @Test
    public void compatibility_only_array() {
        testAll("array: !required&!nullable | !required&nullable | required&nullable | !required&?nullable", Map.of(
                ContainerDefaultEvaluator.ContainerType.array, new boolean[]{true, true, false, true, true, false},
                ContainerDefaultEvaluator.ContainerType.set, ContainerDefaultEvaluator.DEFAULT_7_5_0,
                ContainerDefaultEvaluator.ContainerType.map, ContainerDefaultEvaluator.DEFAULT_7_5_0));
    }

    @Test
    public void compatibility_only_arrayAndSet() {
        testAll("array|set: !required&!nullable | !required&nullable | required&nullable | !required&?nullable", Map.of(
                ContainerDefaultEvaluator.ContainerType.array, new boolean[]{true, true, false, true, true, false},
                ContainerDefaultEvaluator.ContainerType.set, new boolean[]{true, true, false, true, true, false},
                ContainerDefaultEvaluator.ContainerType.map, ContainerDefaultEvaluator.DEFAULT_7_5_0));
    }

    @Test
    public void testMapDefault_DefaultSetting() {
        MapSchema mapSchema = new MapSchema().type("string");
        mapSchema.setAdditionalProperties(new StringSchema());
        assertThat(getArrayDefaultValue(mapSchema, null,null, false, false)).isEqualTo("new HashMap<>()");
        assertThat(getArrayDefaultValue(mapSchema,null,null, false, true)).isNull();
        assertThat(getArrayDefaultValue(mapSchema,null,null, true, false)).isEqualTo("new HashMap<>()");
        assertThat(getArrayDefaultValue(mapSchema,null,null, true, true)).isNull();
        assertThat(getArrayDefaultValue(mapSchema,null,null, false, null)).isEqualTo("new HashMap<>()");
        assertThat(getArrayDefaultValue(mapSchema,null,null, true, null)).isEqualTo("new HashMap<>()");
    }


    @Test
    public void testArrayDefault_SetContainerToNullable() {
        assertThat(getArrayDefaultValue(new ArraySchema(), null,"array|set|map", false, false)).isEqualTo("new ArrayList<>()");
        assertThat(getArrayDefaultValue(new ArraySchema(), null,"array|set|map", false, true)).isNull();
        assertThat(getArrayDefaultValue(new ArraySchema(), null,"array|set|map", true, false)).isEqualTo("new ArrayList<>()");
        assertThat(getArrayDefaultValue(new ArraySchema(), null,"array|set|map", true, true)).isNull();
        assertThat(getArrayDefaultValue(new ArraySchema(), null,"array|set|map", false, null)).isNull();
        assertThat(getArrayDefaultValue(new ArraySchema(), null,"array|set|map", true, null)).isNull();
    }

    @Test
    public void testArrayDefault_ContainerDefaultToNullTrue() {
        assertThat(getArrayDefaultValue(new ArraySchema(), "true",null, false, false)).isNull();
        assertThat(getArrayDefaultValue(new ArraySchema(), "true",null, false, true)).isNull();
        assertThat(getArrayDefaultValue(new ArraySchema(), "true",null, true, false)).isNull();
        assertThat(getArrayDefaultValue(new ArraySchema(), "true",null, true, true)).isNull();
        assertThat(getArrayDefaultValue(new ArraySchema(), "true",null, false, null)).isNull();
        assertThat(getArrayDefaultValue(new ArraySchema(), "true",null, true, null)).isNull();
    }

    @Test
    public void testArrayDefault_7_4_0() {
        assertThat(getArrayDefaultValue(new ArraySchema(), "7.4.0",null, false, false)).isNull();
        assertThat(getArrayDefaultValue(new ArraySchema(), "7.4.0",null, false, true)).isNull();
        assertThat(getArrayDefaultValue(new ArraySchema(), "7.4.0",null, true, false)).isEqualTo("new ArrayList<>()");
        assertThat(getArrayDefaultValue(new ArraySchema(), "7.4.0",null, true, true)).isNull();
        assertThat(getArrayDefaultValue(new ArraySchema(), "7.4.0",null, false, null)).isNull();
        assertThat(getArrayDefaultValue(new ArraySchema(), "7.4.0",null, true, null)).isEqualTo("new ArrayList<>()");
    }


    @Test
    public void testMapDefault_7_4_0() {
        MapSchema mapSchema = new MapSchema().type("string");
        mapSchema.setAdditionalProperties(new StringSchema());
        assertThat(getArrayDefaultValue(mapSchema,"7.4.0",null, false, false)).isEqualTo(null);
        assertThat(getArrayDefaultValue(mapSchema,"7.4.0",null, false, true)).isNull();
        assertThat(getArrayDefaultValue(mapSchema,"7.4.0",null, true, false)).isEqualTo("new HashMap<>()");
        assertThat(getArrayDefaultValue(mapSchema,"7.4.0", null, true, true)).isNull();
        assertThat(getArrayDefaultValue(mapSchema,"7.4.0", null, false, null)).isEqualTo(null);
        assertThat(getArrayDefaultValue(mapSchema,"7.4.0", null, true, null)).isEqualTo("new HashMap<>()");
    }

    @Test
    public void testArrayDefault_DefaultSetting() {
        assertThat(getArrayDefaultValue(new ArraySchema(), null,null, false, false)).isEqualTo("new ArrayList<>()");
        assertThat(getArrayDefaultValue(new ArraySchema(), null,null, false, true)).isNull();
        assertThat(getArrayDefaultValue(new ArraySchema(), null,null, true, false)).isEqualTo("new ArrayList<>()");
        assertThat(getArrayDefaultValue(new ArraySchema(), null,null, true, true)).isNull();
        assertThat(getArrayDefaultValue(new ArraySchema(), null,null, false, null)).isEqualTo("new ArrayList<>()");
        assertThat(getArrayDefaultValue(new ArraySchema(), null,null, true, null)).isEqualTo("new ArrayList<>()");
    }

    String getArrayDefaultValue(Schema containerSchema, String containerDefaultToNull,String setContainerToNullable, boolean required, Boolean nullable) {

        final Schema schema = new Schema()
                .addProperties("container", containerSchema);

        if (required) {
            schema.setRequired(Arrays.asList("container"));
        }
        containerSchema.setNullable(nullable);

        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        if (setContainerToNullable != null) {
            Map<String, String> options = new HashMap<>();
            options.put("SET_CONTAINER_TO_NULLABLE", setContainerToNullable);
            OpenAPINormalizer openAPINormalizer = new OpenAPINormalizer(openAPI, options);
            openAPINormalizer.normalize();
        }
        final JavaClientCodegen codegen = new JavaClientCodegen();
        if (containerDefaultToNull != null) {
            codegen.setContainerDefaultToNull(containerDefaultToNull);
        }
        codegen.processOpts();
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        final CodegenProperty property = cm.vars.get(0);
        return codegen.toDefaultValue(property, containerSchema);
    }

    private void testAll(String expression, Map<ContainerDefaultEvaluator.ContainerType, boolean[]> expected) {
        Map<ContainerDefaultEvaluator.ContainerType, boolean[]> map = ContainerDefaultEvaluator.evaluate(expression);
        expected.forEach((containerType, booleans) -> assertThat(map.get(containerType)).describedAs("containerType %s", containerType).isEqualTo(booleans));

    }


    private void testAll(String expression, boolean... booleans) {
        Map<ContainerDefaultEvaluator.ContainerType, boolean[]> map = ContainerDefaultEvaluator.evaluate(expression);
        assertThat(map.get(ContainerDefaultEvaluator.ContainerType.array)).isEqualTo(booleans);
    }


}
