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

package org.openapitools.codegen.go;

import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.IntegerSchema;
import io.swagger.v3.oas.models.media.MapSchema;
import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.java.AbstractJavaCodegenTest;
import org.openapitools.codegen.languages.AbstractGoCodegen;
import org.openapitools.codegen.utils.ModelUtils;
import org.testng.Assert;
import org.testng.annotations.Test;

public class AbstractGoCodegenTest {

    @Test
    public void testInitialConfigValues() throws Exception {
        final AbstractGoCodegen codegen = new P_AbstractGoCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final AbstractGoCodegen codegen = new P_AbstractGoCodegen();
        codegen.setHideGenerationTimestamp(true);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final AbstractGoCodegen codegen = new P_AbstractGoCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, true);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
    }

    @Test
    public void getTypeDeclarationTest() {
        final AbstractGoCodegen codegen = new P_AbstractGoCodegen();

        // Create an alias to an array schema
        Schema<?> nestedArraySchema = new ArraySchema().items(new IntegerSchema().format("int32"));
        codegen.setOpenAPI(new OpenAPI().components(new Components().addSchemas("NestedArray", nestedArraySchema)));

        // Create an array schema with item type set to the array alias
        Schema<?> schema = new ArraySchema().items(new Schema().$ref("#/components/schemas/NestedArray"));

        ModelUtils.setGenerateAliasAsModel(false);
        String defaultValue = codegen.getTypeDeclaration(schema);
        Assert.assertEquals(defaultValue, "[][]int32");

        ModelUtils.setGenerateAliasAsModel(true);
        defaultValue = codegen.getTypeDeclaration(schema);
        Assert.assertEquals(defaultValue, "[]NestedArray");

        // Create a map schema with additionalProperties type set to array alias
        schema = new MapSchema().additionalProperties(new Schema().$ref("#/components/schemas/NestedArray"));

        ModelUtils.setGenerateAliasAsModel(false);
        defaultValue = codegen.getTypeDeclaration(schema);
        Assert.assertEquals(defaultValue, "map[string][]int32");

        ModelUtils.setGenerateAliasAsModel(true);
        defaultValue = codegen.getTypeDeclaration(schema);
        Assert.assertEquals(defaultValue, "map[string]NestedArray");
    }

    private static class P_AbstractGoCodegen extends AbstractGoCodegen {
        @Override
        public CodegenType getTag() {
            return null;
        }

        @Override
        public String getName() {
            return null;
        }

        @Override
        public String getHelp() {
            return null;
        }
    }
}
