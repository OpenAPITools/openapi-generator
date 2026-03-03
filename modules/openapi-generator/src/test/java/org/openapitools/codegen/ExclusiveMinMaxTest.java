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

package org.openapitools.codegen;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertNotNull;
import static org.testng.Assert.assertTrue;

import java.util.Map;

import org.testng.annotations.Test;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.Schema;

public class ExclusiveMinMaxTest {

    @Test
    public void testCodegen31ExclusiveMinMaxNumericOnly() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_1/exclusive-min-max.yaml");

        OpenAPINormalizer n = new OpenAPINormalizer(openAPI, Map.of("NORMALIZE_31SPEC", "true"));
        n.normalize();

        DefaultCodegen config = new DefaultCodegen();
        config.setOpenAPI(openAPI);

        Schema<?> schema = openAPI.getPaths().get("/x").getGet().getParameters().get(0).getSchema();

        CodegenProperty cp = config.fromProperty("price", schema);

        // exclusiveMinimum: 0
        assertEquals(cp.getMinimum(), "0");
        assertTrue(cp.getExclusiveMinimum());

        // exclusiveMaximum: 10
        assertEquals(cp.getMaximum(), "10");
        assertTrue(cp.getExclusiveMaximum());
    }

    @Test
    public void testCodegen31ExclusiveMinMaxStricterThanMinMax() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_1/exclusive-min-max.yaml");

        OpenAPINormalizer n = new OpenAPINormalizer(openAPI, Map.of("NORMALIZE_31SPEC", "true"));
        n.normalize();

        DefaultCodegen config = new DefaultCodegen();
        config.setOpenAPI(openAPI);

        Schema<?> schema = openAPI.getPaths().get("/foo").getGet().getParameters().get(0).getSchema();
        CodegenProperty cp = config.fromProperty("foo", schema);

        assertEquals(cp.getMinimum(), "1");
        assertTrue(cp.getExclusiveMinimum());

        assertEquals(cp.getMaximum(), "10");
        assertTrue(cp.getExclusiveMaximum());
    }

    @Test
    public void testCodegen31ExclusiveMinMaxEqualToMinMax() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_1/exclusive-min-max.yaml");

        OpenAPINormalizer n = new OpenAPINormalizer(openAPI, Map.of("NORMALIZE_31SPEC", "true"));
        n.normalize();

        DefaultCodegen config = new DefaultCodegen();
        config.setOpenAPI(openAPI);

        Schema<?> schema = openAPI.getPaths().get("/bar").getGet().getParameters().get(0).getSchema();
        CodegenProperty cp = config.fromProperty("bar", schema);

        // minimum: 0 + exclusiveMinimum: 0 → must remain exclusive
        assertEquals(cp.getMinimum(), "0");
        assertTrue(cp.getExclusiveMinimum());

        // maximum: 10 + exclusiveMaximum: 10 → must remain exclusive
        assertEquals(cp.getMaximum(), "10");
        assertTrue(cp.getExclusiveMaximum());
    }

    @Test
    public void testCodegen31ExclusiveMinMaxInclusiveStricterThanExclusiveValue() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_1/exclusive-min-max.yaml");

        OpenAPINormalizer n = new OpenAPINormalizer(openAPI, Map.of("NORMALIZE_31SPEC", "true"));
        n.normalize();

        DefaultCodegen config = new DefaultCodegen();
        config.setOpenAPI(openAPI);

        Schema<?> schema = openAPI.getPaths().get("/baz").getGet().getParameters().get(0).getSchema();
        CodegenProperty cp = config.fromProperty("baz", schema);

        // minimum: 5 is stricter than exclusiveMinimum: 0 (x >= 5 dominates x > 0)
        assertEquals(cp.getMinimum(), "5");
        assertFalse(Boolean.TRUE.equals(cp.getExclusiveMinimum()));

        // maximum: 10 is stricter than exclusiveMaximum: 11 (x <= 10 dominates x < 11)
        assertEquals(cp.getMaximum(), "10");
        assertFalse(Boolean.TRUE.equals(cp.getExclusiveMaximum()));
    }

    @Test
    public void testCodegen31ExclusiveMinMaxBooleanExclusiveAlreadySet() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_1/exclusive-min-max.yaml");
        OpenAPINormalizer n = new OpenAPINormalizer(openAPI, Map.of("NORMALIZE_31SPEC", "true"));
        n.normalize();

        DefaultCodegen config = new DefaultCodegen();
        config.setOpenAPI(openAPI);

        Schema<?> schema = openAPI.getPaths().get("/old").getGet().getParameters().get(0).getSchema();
        CodegenProperty cp = config.fromProperty("old", schema);

        // 3.0-style boolean exclusive flags should remain intact
        assertEquals(cp.getMinimum(), "0");
        assertFalse(Boolean.TRUE.equals(cp.getExclusiveMinimum()));

        assertEquals(cp.getMaximum(), "10");
        assertFalse(Boolean.TRUE.equals(cp.getExclusiveMaximum()));
    }

    @Test
    public void testCodegenModelWithAllOfOas30() {
        // Load OAS 3.0 spec (using standard boolean syntax)
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/exclusive-min-max.yaml");

        // Normalize (should not affect existing 3.0 boolean flags)
        OpenAPINormalizer n = new OpenAPINormalizer(openAPI, Map.of());
        n.normalize();

        final Schema<?> priceDto3Schema = openAPI.getComponents().getSchemas().get("PriceDto3");
        final CodegenConfig config = new DefaultCodegen();
        config.setOpenAPI(openAPI);
        final CodegenModel cm = config.fromModel("PriceDto3", priceDto3Schema);

        final CodegenProperty historyPrice2Prop = cm
                .getVars()
                .stream()
                .filter(p -> "historyPrice2".equals(p.baseName))
                .findFirst()
                .orElse(null);

        assertNotNull(historyPrice2Prop, "OAS 3.0: PriceDto3 should contain historyPrice2");

        // Verify that under OAS 3.0, allOf composition correctly preserves the
        // exclusiveMinimum flag
        assertEquals(historyPrice2Prop.getMinimum(), "3001");
        assertTrue(historyPrice2Prop.getExclusiveMinimum(), "OAS 3.0: historyPrice2 exclusiveMinimum should be true");
    }

    @Test
    public void testCodegenModelWithAllOfNormalization() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_1/exclusive-min-max.yaml");

        // 1. Run normalization
        OpenAPINormalizer n = new OpenAPINormalizer(openAPI, Map.of("NORMALIZE_31SPEC", "true"));
        n.normalize();

        // 2. Simulate Codegen processing PriceDto3 (which uses allOf to reference
        // PriceDto2)
        final Schema<?> priceDto3Schema = openAPI.getComponents().getSchemas().get("PriceDto3");
        final CodegenConfig config = new DefaultCodegen();
        config.setOpenAPI(openAPI);
        final CodegenModel cm = config.fromModel("PriceDto3", priceDto3Schema);

        // 3. Check historyPrice2 property inherited from PriceDto2
        final CodegenProperty historyPrice2Prop = cm
                .getVars()
                .stream()
                .filter(p -> "historyPrice2".equals(p.baseName))
                .findFirst()
                .orElse(null);

        assertNotNull(historyPrice2Prop, "PriceDto3 should contain inherited historyPrice2 property from PriceDto2");

        // Verify that CodegenProperty received normalized values and flags
        assertEquals(historyPrice2Prop.getMinimum(), "3101", "historyPrice2 minimum should be 3101");
        assertTrue(historyPrice2Prop.getExclusiveMinimum(), "historyPrice2 exclusiveMinimum should be true");
    }

}
