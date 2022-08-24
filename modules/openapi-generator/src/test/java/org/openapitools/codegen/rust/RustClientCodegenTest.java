/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
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

package org.openapitools.codegen.rust;

import io.swagger.v3.oas.models.media.IntegerSchema;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.languages.RustClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.math.BigDecimal;

public class RustClientCodegenTest {

    @Test
    public void testInitialConfigValues() throws Exception {
        final RustClientCodegen codegen = new RustClientCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertTrue(codegen.isHideGenerationTimestamp());

        Assert.assertEquals(codegen.additionalProperties().get(RustClientCodegen.PREFER_UNSIGNED_INT), Boolean.FALSE);
        Assert.assertFalse(codegen.getPreferUnsignedInt());

        Assert.assertEquals(codegen.additionalProperties().get(RustClientCodegen.BEST_FIT_INT), Boolean.FALSE);
        Assert.assertFalse(codegen.getBestFitInt());
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final RustClientCodegen codegen = new RustClientCodegen();
        codegen.setHideGenerationTimestamp(false);
        codegen.setPreferUnsignedInt(true);
        codegen.setBestFitInt(true);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertFalse(codegen.isHideGenerationTimestamp());

        Assert.assertEquals(codegen.additionalProperties().get(RustClientCodegen.PREFER_UNSIGNED_INT), Boolean.TRUE);
        Assert.assertTrue(codegen.getPreferUnsignedInt());

        Assert.assertEquals(codegen.additionalProperties().get(RustClientCodegen.BEST_FIT_INT), Boolean.TRUE);
        Assert.assertTrue(codegen.getBestFitInt());
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final RustClientCodegen codegen = new RustClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertFalse(codegen.isHideGenerationTimestamp());
    }

    @Test
    public void testLowercaseParameterName() throws Exception {
        final RustClientCodegen codegen = new RustClientCodegen();

        Assert.assertEquals(codegen.toParamName("TESTING"), "testing");
    }

    @Test
    public void testGetSchemaTypeIntegerNoBounds() {
        final IntegerSchema s = new IntegerSchema();
        final RustClientCodegen codegen = new RustClientCodegen();
        codegen.setBestFitInt(true);
        codegen.processOpts();

        s.setType("i32");

        // min and max are null
        Assert.assertEquals(codegen.getSchemaType(s), "i32");

        s.setMinimum(BigDecimal.valueOf(Short.MIN_VALUE));

        // max is null
        Assert.assertEquals(codegen.getSchemaType(s), "i32");

        s.setMaximum(BigDecimal.valueOf(Short.MAX_VALUE));
        s.setMinimum(null);

        // min is null
        Assert.assertEquals(codegen.getSchemaType(s), "i32");
    }

    @Test
    public void testGetSchemaTypeI64() {
        final IntegerSchema s = new IntegerSchema();
        final RustClientCodegen codegen = new RustClientCodegen();
        codegen.setBestFitInt(true);
        codegen.processOpts();

        s.setType("i64");
        s.setMinimum(BigDecimal.valueOf(Long.MIN_VALUE));
        s.setMaximum(BigDecimal.valueOf(Long.MAX_VALUE));

        Assert.assertEquals(codegen.getSchemaType(s), "i64");
    }

    @Test
    public void testGetSchemaTypeI32() {
        final IntegerSchema s = new IntegerSchema();
        final RustClientCodegen codegen = new RustClientCodegen();
        codegen.setBestFitInt(true);
        codegen.processOpts();

        s.setType("i32");
        s.setMinimum(BigDecimal.valueOf(Integer.MIN_VALUE));
        s.setMaximum(BigDecimal.valueOf(Integer.MAX_VALUE));

        Assert.assertEquals(codegen.getSchemaType(s), "i32");
    }

    @Test
    public void testGetSchemaTypeI16() {
        final IntegerSchema s = new IntegerSchema();
        final RustClientCodegen codegen = new RustClientCodegen();
        codegen.setBestFitInt(true);
        codegen.processOpts();

        s.setType("i32");
        s.setMinimum(BigDecimal.valueOf(Short.MIN_VALUE));
        s.setMaximum(BigDecimal.valueOf(Short.MAX_VALUE));

        Assert.assertEquals(codegen.getSchemaType(s), "i16");
    }

    @Test
    public void testGetSchemaTypeI8() {
        final IntegerSchema s = new IntegerSchema();
        final RustClientCodegen codegen = new RustClientCodegen();
        codegen.setBestFitInt(true);
        codegen.processOpts();

        s.setType("i32");
        s.setMinimum(BigDecimal.valueOf(-128));
        s.setMaximum(BigDecimal.valueOf(127));

        Assert.assertEquals(codegen.getSchemaType(s), "i8");
    }

    @Test
    public void testGetSchemaTypeU64() {
        final IntegerSchema s = new IntegerSchema();
        final RustClientCodegen codegen = new RustClientCodegen();
        codegen.setPreferUnsignedInt(true);
        codegen.processOpts();

        s.setType("i64");
        s.setMinimum(BigDecimal.ZERO);

        Assert.assertEquals(codegen.getSchemaType(s), "u64");

        s.setMaximum(BigDecimal.valueOf(Long.MAX_VALUE).add(BigDecimal.valueOf(Long.MAX_VALUE)));

        Assert.assertEquals(codegen.getSchemaType(s), "u64");

        s.setMinimum(null);
        s.setMaximum(null);

        Assert.assertEquals(codegen.getSchemaType(s), "i64");
    }

    @Test
    public void testGetSchemaTypeU32() {
        final IntegerSchema s = new IntegerSchema();
        final RustClientCodegen codegen = new RustClientCodegen();
        codegen.setPreferUnsignedInt(true);
        codegen.processOpts();

        s.setType("i32");
        s.setMinimum(BigDecimal.ZERO);

        Assert.assertEquals(codegen.getSchemaType(s), "u32");

        s.setMaximum(BigDecimal.valueOf(65535));

        Assert.assertEquals(codegen.getSchemaType(s), "u32");
    }

    @Test
    public void testGetSchemaTypeU16() {
        final IntegerSchema s = new IntegerSchema();
        final RustClientCodegen codegen = new RustClientCodegen();
        codegen.setBestFitInt(true);
        codegen.setPreferUnsignedInt(true);
        codegen.processOpts();

        s.setType("i32");
        s.setMinimum(BigDecimal.ZERO);
        s.setMaximum(BigDecimal.valueOf(65535));

        Assert.assertEquals(codegen.getSchemaType(s), "u16");
    }

    @Test
    public void testGetSchemaTypeU8() {
        final IntegerSchema s = new IntegerSchema();
        final RustClientCodegen codegen = new RustClientCodegen();
        codegen.setBestFitInt(true);
        codegen.setPreferUnsignedInt(true);
        codegen.processOpts();

        s.setType("i32");
        s.setMinimum(BigDecimal.ZERO);
        s.setMaximum(BigDecimal.valueOf(255));

        Assert.assertEquals(codegen.getSchemaType(s), "u8");
    }
}
