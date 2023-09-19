/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech) Licensed under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy
 * of the License at https://www.apache.org/licenses/LICENSE-2.0 Unless required by applicable law or agreed to in
 * writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS
 * OF ANY KIND, either express or implied. See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.java;

import java.util.HashSet;
import java.util.List;
import java.util.Map;

import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.JavaClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.parameters.Parameter;

public class JavaIsMatrixParameterTest {

    @Test(description = "test if path param flag isMatrix is correctly set")
    public void testIsMatrixFlag() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("3_1/matrix-path-params-spec.json");
        final DefaultCodegen codegen = new JavaClientCodegen();
        codegen.setOpenAPI(openAPI);

        final Map<String, PathItem> paths = openAPI.getPaths();
        final PathItem pathItem = paths.get("/plainMatrixParamFlat{matrixParam}/{simpleParam}");
        Assert.assertNotNull(pathItem);

        final List<Parameter> parameters = pathItem.getParameters();
        Assert.assertEquals(pathItem.getParameters().size(), 2);

        final CodegenParameter matrixParameter = codegen.fromParameter(parameters.get(0), new HashSet<>());
        Assert.assertTrue(matrixParameter.isMatrix);

        final CodegenParameter simpleParameter = codegen.fromParameter(parameters.get(1), new HashSet<>());
        Assert.assertFalse(simpleParameter.isMatrix);
    }
}
