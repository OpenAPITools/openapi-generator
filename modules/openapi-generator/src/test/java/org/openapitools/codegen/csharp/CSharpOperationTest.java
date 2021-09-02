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

package org.openapitools.codegen;

import com.google.common.collect.Sets;
import com.samskivert.mustache.Mustache.Lambda;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.headers.Header;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.oas.models.parameters.QueryParameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.responses.ApiResponses;
import io.swagger.v3.oas.models.security.SecurityScheme;
import io.swagger.v3.parser.core.models.ParseOptions;

import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.config.GlobalSettings;
import org.openapitools.codegen.languages.AbstractCSharpCodegen;
import org.openapitools.codegen.languages.AspNetCoreServerCodegen;
import org.openapitools.codegen.languages.CSharpNetCoreClientCodegen;
import org.openapitools.codegen.templating.mustache.CamelCaseLambda;
import org.openapitools.codegen.templating.mustache.IndentedLambda;
import org.openapitools.codegen.templating.mustache.LowercaseLambda;
import org.openapitools.codegen.templating.mustache.TitlecaseLambda;
import org.openapitools.codegen.templating.mustache.UppercaseLambda;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.SemVer;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.nio.file.Files;
import java.util.*;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import javax.accessibility.AccessibleAttributeSequence;

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
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/" + Integer.toString(openApiVersion) + "_0/petstore-with-fake-endpoints-models-for-testing.yaml");
        codegen.setNullableReferenceTypes(nullableReferenceTypes);
        codegen.setOpenAPI(openAPI);

        final String path = "/pet/{petId}/uploadImage";
        final Operation postOperation = openAPI.getPaths().get(path).getPost();
        final CodegenOperation codegenOperation = codegen.fromOperation(path, "post", postOperation, null);

        return codegenOperation.allParams.get(2).dataType;
    }
}
