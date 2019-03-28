/*
 * Copyright 2019 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2019 SmartBear Software
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

package org.openapitools.codegen.kotlin;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.responses.ApiResponses;
import io.swagger.v3.parser.util.SchemaTypeUtil;
import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.JavaClientCodegen;
import org.openapitools.codegen.languages.KotlinKtorClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.HashSet;

public class KotlinKtorClientCodegenTest {

    @Test
    public void addsAllResponseTypes() {
        final OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/2_0/multiple-return-types.yaml");
        final DefaultCodegen codegen = new KotlinKtorClientCodegen();
        codegen.setOpenAPI(openAPI);

        final String path = "/lookup/person";
        Operation findPersonOperation = openAPI.getPaths().get(path).getPost();
        CodegenOperation op = codegen.fromOperation(path, "post", findPersonOperation, null);

        int operationsWithResponseType = 0;
        for (CodegenResponse response : op.responses) {
            if (response.dataType != null) {
                operationsWithResponseType += 1;
            }
        }

        Assert.assertEquals(operationsWithResponseType, 3);
        Assert.assertEquals(op.uniquedResponses.size(), 2);
        Assert.assertEquals(op.uniquedResponses.get(0).dataType, "Person");
        Assert.assertEquals(op.uniquedResponses.get(1).dataType, "PersonLookupError");
    }

    @Test
    public void testResponseWithMapBaseType() {
        MapSchema mapSchema = new MapSchema();
        mapSchema.setAdditionalProperties(new IntegerSchema());
        ApiResponse response = new ApiResponse()
                .content(new Content().addMediaType("application/json", new MediaType().schema(mapSchema)));

        OpenAPI openAPI = TestUtils.createOpenAPI();
        Operation operation1 = new Operation().operationId("op1").responses(new ApiResponses().addApiResponse("200", response));
        openAPI.path("/here", new PathItem().get(operation1));
        final DefaultCodegen codegen = new KotlinKtorClientCodegen();
        codegen.setOpenAPI(openAPI);

        CodegenResponse cr = codegen.fromResponse("200", response);

        Assert.assertEquals(cr.baseType, "kotlin.Int");
    }

    @Test
    public void givenAnArrayParameter_whenPostProcessingIt_thenTheBaseTypeWillBeTheInternalType() throws Exception {
        ////////////////////////// for complex types

        OpenAPI openAPI = TestUtils.createOpenAPI();
        final DefaultCodegen codegen = new KotlinKtorClientCodegen();
        codegen.setOpenAPI(openAPI);

        CodegenProperty property = new CodegenProperty();
        property.dataType = "User";

        CodegenParameter codegenParameter = new CodegenParameter();
        codegenParameter.isListContainer = true;
        codegenParameter.dataType = "kotlin.collections.List<User>";
        codegenParameter.baseType = "kotlin.collections.List";
        codegenParameter.items = property;

        codegen.postProcessParameter(codegenParameter);
        Assert.assertEquals(codegenParameter.dataType, "kotlin.collections.List<User>");
        Assert.assertEquals(codegenParameter.baseType, "User");

        //////////////////////////// for language primitive types

        CodegenProperty property2 = new CodegenProperty();
        property2.dataType = "kotlin.String";

        CodegenParameter codegenParameter2 = new CodegenParameter();
        codegenParameter2.isListContainer = true;
        codegenParameter2.dataType = "kotlin.collections.List<kotlin.String>";
        codegenParameter2.baseType = "kotlin.collections.List";
        codegenParameter2.items = property2;

        codegen.postProcessParameter(codegenParameter2);
        Assert.assertEquals(codegenParameter2.dataType, "kotlin.collections.List<kotlin.String>");
        Assert.assertEquals(codegenParameter2.baseType, "kotlin.String");
    }
}