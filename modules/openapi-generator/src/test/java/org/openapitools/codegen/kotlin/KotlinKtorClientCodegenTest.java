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
import io.swagger.v3.oas.models.media.*;
import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.KotlinKtorClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

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
        Assert.assertEquals(op.allResponseDataTypes.size(), 2);
        Assert.assertEquals(op.allResponseDataTypes.get(0).get("dataType"), "Person");
        Assert.assertEquals(op.allResponseDataTypes.get(1).get("dataType"), "PersonLookupError");
    }
}