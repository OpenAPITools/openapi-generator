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

package org.openapitools.codegen.csharp;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;

import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.AbstractCSharpCodegen;
import org.openapitools.codegen.languages.AspNetCoreServerCodegen;
import org.openapitools.codegen.languages.CSharpNetCoreClientCodegen;
import org.testng.annotations.Test;

import static org.testng.Assert.*;

public class CSharpOperationTest {

    @Test
    public void assertMethodOptionalParameterDataType() {
        assertEquals(getOperationOptionalParameterDataType(new AspNetCoreServerCodegen(), 2, false), "System.IO.Stream");
        assertEquals(getOperationOptionalParameterDataType(new AspNetCoreServerCodegen(), 2, true), "System.IO.Stream?");
        assertEquals(getOperationOptionalParameterDataType(new AspNetCoreServerCodegen(), 3, false), "System.IO.Stream");
        assertEquals(getOperationOptionalParameterDataType(new AspNetCoreServerCodegen(), 3, true), "System.IO.Stream?");

        assertEquals(getOperationOptionalParameterDataType(new CSharpNetCoreClientCodegen(), 2, false), "System.IO.Stream");
        assertEquals(getOperationOptionalParameterDataType(new CSharpNetCoreClientCodegen(), 2, true), "System.IO.Stream?");
        assertEquals(getOperationOptionalParameterDataType(new CSharpNetCoreClientCodegen(), 3, false), "System.IO.Stream");
        assertEquals(getOperationOptionalParameterDataType(new CSharpNetCoreClientCodegen(), 3, true), "System.IO.Stream?");
    }

    public String getOperationOptionalParameterDataType(final AbstractCSharpCodegen codegen, final int openApiVersion, final Boolean nullableReferenceTypes){
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/" + openApiVersion + "_0/petstore-with-fake-endpoints-models-for-testing.yaml");
        codegen.setNullableReferenceTypes(nullableReferenceTypes);
        codegen.setOpenAPI(openAPI);

        final String path = "/pet/{petId}/uploadImage";
        final Operation postOperation = openAPI.getPaths().get(path).getPost();
        final CodegenOperation codegenOperation = codegen.fromOperation(path, "post", postOperation, null);

        return codegenOperation.allParams.get(2).dataType;
    }
}
