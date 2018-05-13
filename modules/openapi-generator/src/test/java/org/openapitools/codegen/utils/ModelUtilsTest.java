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

package org.openapitools.codegen.utils;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.parser.core.models.ParseOptions;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.List;

public class ModelUtilsTest {

    @Test
    public void testEnsureNoDuplicateProduces() {
        final OpenAPI openAPI = new OpenAPIParser().readLocation("src/test/resources/3_0/ping.yaml", null, new ParseOptions()).getOpenAPI();
        List<String> unusedSchemas = ModelUtils.getUnusedSchemas(openAPI);
        Assert.assertEquals(unusedSchemas.size(), 0);
    }
}
