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

package org.openapitools.codegen.html;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.IntegerSchema;
import io.swagger.v3.oas.models.media.ObjectSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.StaticHtmlGenerator;
import org.testng.Assert;
import org.testng.annotations.Test;

public class StaticHtmlGeneratorTest {

    @Test
    public void testAdditionalPropertiesFalse() {
        OpenAPI openAPI = TestUtils.createOpenAPI();
        final StaticHtmlGenerator codegen = new StaticHtmlGenerator();

        Schema schema = new ObjectSchema()
                .additionalProperties(false)
                .addProperties("id", new IntegerSchema())
                .addProperties("name", new StringSchema());
        codegen.setOpenAPI(openAPI);
        CodegenModel cm = codegen.fromModel("test", schema);
        Assert.assertNotNull(cm);
    }

    @Test
    public void testSpecWithoutSchema() throws Exception {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/ping.yaml");

        final StaticHtmlGenerator codegen = new StaticHtmlGenerator();
        codegen.preprocessOpenAPI(openAPI);

        Assert.assertEquals(openAPI.getInfo().getTitle(), "ping test");
    }

    @Test(description = "ensure that snake_case propery names wont be converted to snakeUnderscorecase")
    public void testFromPropertyWithUnderscores() {
        final Schema schema = new Schema()
            .description("a sample model with property containing an underscore")
            .addProperties("favorite_food", new StringSchema());
        final OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("UnderscoreTest", schema);
        final DefaultCodegen codegen = new StaticHtmlGenerator();
        codegen.setOpenAPI(openAPI);

        CodegenProperty property = codegen.fromProperty("favorite_food", (Schema) openAPI.getComponents().getSchemas().get("UnderscoreTest").getProperties().get("favorite_food"));

        Assert.assertEquals(property.baseName, "favorite_food");
        Assert.assertEquals(property.name, "favorite_food");
    }
}
