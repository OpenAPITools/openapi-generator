/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.swift4;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.Swift4Codegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.Arrays;

@SuppressWarnings("static-method")
public class Swift4ModelEnumTest {
    @Test(description = "convert a java model with an enum and a default value")
    public void converterTest() {
        final StringSchema enumSchema = new StringSchema();
        enumSchema.setEnum(Arrays.asList("VALUE1", "VALUE2", "VALUE3"));
        enumSchema.setDefault("VALUE2");
        final Schema model = new Schema().type("object").addProperties("name", enumSchema);

        final DefaultCodegen codegen = new Swift4Codegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty enumVar = cm.vars.get(0);
        Assert.assertEquals(enumVar.baseName, "name");
        Assert.assertEquals(enumVar.dataType, "String");
        Assert.assertEquals(enumVar.datatypeWithEnum, "Name");
        Assert.assertEquals(enumVar.name, "name");
        Assert.assertEquals(enumVar.defaultValue, ".VALUE2");
        Assert.assertEquals(enumVar.baseType, "String");
        Assert.assertTrue(enumVar.isEnum);
    }
}
