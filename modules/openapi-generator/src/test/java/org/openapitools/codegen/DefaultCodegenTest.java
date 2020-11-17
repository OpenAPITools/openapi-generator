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

import org.openapitools.codegen.config.CodegenConfigurator;
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
import java.util.stream.Collectors;

import static org.testng.Assert.*;


public class DefaultCodegenTest {

    @Test
    public void testAdditionalPropertiesPresentInModels() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/issue_7613.yaml");
        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setDisallowAdditionalPropertiesIfNotPresent(false);

        String modelName;
        Schema sc;
        CodegenModel cm;
        CodegenProperty anyTypeSchema = codegen.fromProperty("", new Schema());

        modelName = "AdditionalPropertiesUnset";
        sc = openAPI.getComponents().getSchemas().get(modelName);
        cm = codegen.fromModel(modelName, sc);
        assertEquals(cm.getAdditionalProperties(), anyTypeSchema);

        modelName = "AdditionalPropertiesTrue";
        sc = openAPI.getComponents().getSchemas().get(modelName);
        cm = codegen.fromModel(modelName, sc);
        assertEquals(cm.getAdditionalProperties(), anyTypeSchema);

        modelName = "AdditionalPropertiesFalse";
        sc = openAPI.getComponents().getSchemas().get(modelName);
        cm = codegen.fromModel(modelName, sc);
        assertEquals(cm.getAdditionalProperties(), null);

        modelName = "AdditionalPropertiesSchema";
        sc = openAPI.getComponents().getSchemas().get(modelName);
        cm = codegen.fromModel(modelName, sc);
        CodegenProperty stringCp = codegen.fromProperty("", new Schema().type("string"));
        assertEquals(cm.getAdditionalProperties(), stringCp);
    }

    @Test
    public void testAdditionalPropertiesPresentInModelProperties() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/issue_7613.yaml");
        final DefaultCodegen codegen = new DefaultCodegen();
        java.util.logging.Logger.getLogger("DefaultCodegen").setLevel(java.util.logging.Level.ALL);
        codegen.setOpenAPI(openAPI);
        codegen.setDisallowAdditionalPropertiesIfNotPresent(false);

        String modelName;
        Schema sc;
        CodegenModel cm;
        CodegenProperty anyTypeSchema = codegen.fromProperty("", new Schema());
        CodegenProperty stringCp = codegen.fromProperty("", new Schema().type("string"));
        CodegenProperty mapWithAddPropsUnset;
        CodegenProperty mapWithAddPropsTrue;
        CodegenProperty mapWithAddPropsFalse;
        CodegenProperty mapWithAddPropsSchema;

        modelName = "ObjectModelWithRefAddPropsInProps";
        sc = openAPI.getComponents().getSchemas().get(modelName);
        cm = codegen.fromModel(modelName, sc);
        mapWithAddPropsUnset = cm.getVars().get(0);
        assertEquals(mapWithAddPropsUnset.getAdditionalProperties(), anyTypeSchema);
        mapWithAddPropsTrue = cm.getVars().get(1);
        assertEquals(mapWithAddPropsTrue.getAdditionalProperties(), anyTypeSchema);
        mapWithAddPropsFalse = cm.getVars().get(2);
        assertEquals(mapWithAddPropsFalse.getAdditionalProperties(), null);
        mapWithAddPropsSchema = cm.getVars().get(3);
        assertEquals(mapWithAddPropsSchema.getAdditionalProperties(), stringCp);

        modelName = "ObjectModelWithAddPropsInProps";
        sc = openAPI.getComponents().getSchemas().get(modelName);
        cm = codegen.fromModel(modelName, sc);
        mapWithAddPropsUnset = cm.getVars().get(0);
        assertEquals(mapWithAddPropsUnset.getAdditionalProperties(), anyTypeSchema);
        mapWithAddPropsTrue = cm.getVars().get(1);
        assertEquals(mapWithAddPropsTrue.getAdditionalProperties(), anyTypeSchema);
        mapWithAddPropsFalse = cm.getVars().get(2);
        assertEquals(mapWithAddPropsFalse.getAdditionalProperties(), null);
        mapWithAddPropsSchema = cm.getVars().get(3);
        assertEquals(mapWithAddPropsSchema.getAdditionalProperties(), stringCp);
    }

//    @Test
//    public void testAdditionalPropertiesPresentInParameters() {
//        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/issue_7613.yaml");
//        final DefaultCodegen codegen = new DefaultCodegen();
//        codegen.setOpenAPI(openAPI);
//        codegen.setDisallowAdditionalPropertiesIfNotPresent(false);
//
//        String path;
//        Operation operation;
//        CodegenOperation co;
//
//        CodegenProperty anyTypeSchema = codegen.fromProperty("", new Schema());
//        CodegenProperty stringCp = codegen.fromProperty("", new Schema().type("string"));
//        CodegenParameter mapWithAddPropsUnset;
//        CodegenParameter mapWithAddPropsTrue;
//        CodegenParameter mapWithAddPropsFalse;
//        CodegenParameter mapWithAddPropsSchema;
//
//        path = "/ref_additional_properties/";
//        operation = openAPI.getPaths().get(path).getPost();
//        co = codegen.fromOperation(path, "POST", operation, null);
//        mapWithAddPropsUnset = co.queryParams.get(0);
//        assertEquals(mapWithAddPropsUnset.getAdditionalProperties(), anyTypeSchema);
//        mapWithAddPropsTrue = co.queryParams.get(1);
//        assertEquals(mapWithAddPropsTrue.getAdditionalProperties(), anyTypeSchema);
//        mapWithAddPropsFalse = co.queryParams.get(2);
//        assertEquals(mapWithAddPropsFalse.getAdditionalProperties(), null);
//        mapWithAddPropsSchema = co.queryParams.get(3);
//        assertEquals(mapWithAddPropsSchema.getAdditionalProperties(), stringCp);
//
//        path = "/additional_properties/";
//        operation = openAPI.getPaths().get(path).getPost();
//        co = codegen.fromOperation(path, "POST", operation, null);
//        mapWithAddPropsUnset = co.queryParams.get(0);
//        assertEquals(mapWithAddPropsUnset.getAdditionalProperties(), anyTypeSchema);
//        mapWithAddPropsTrue = co.queryParams.get(1);
//        assertEquals(mapWithAddPropsTrue.getAdditionalProperties(), anyTypeSchema);
//        mapWithAddPropsFalse = co.queryParams.get(2);
//        assertEquals(mapWithAddPropsFalse.getAdditionalProperties(), null);
//        mapWithAddPropsSchema = co.queryParams.get(3);
//        assertEquals(mapWithAddPropsSchema.getAdditionalProperties(), stringCp);
//    }
//
//    @Test
//    public void testAdditionalPropertiesPresentInResponses() {
//        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/issue_7613.yaml");
//        final DefaultCodegen codegen = new DefaultCodegen();
//        codegen.setOpenAPI(openAPI);
//        codegen.setDisallowAdditionalPropertiesIfNotPresent(false);
//
//        String path;
//        Operation operation;
//        CodegenOperation co;
//
//        CodegenProperty anyTypeSchema = codegen.fromProperty("", new Schema());
//        CodegenProperty stringCp = codegen.fromProperty("", new Schema().type("string"));
//        CodegenResponse mapWithAddPropsUnset;
//        CodegenResponse mapWithAddPropsTrue;
//        CodegenResponse mapWithAddPropsFalse;
//        CodegenResponse mapWithAddPropsSchema;
//
//        path = "/ref_additional_properties/";
//        operation = openAPI.getPaths().get(path).getPost();
//        co = codegen.fromOperation(path, "POST", operation, null);
//        mapWithAddPropsUnset = co.responses.get(0);
//        assertEquals(mapWithAddPropsUnset.getAdditionalProperties(), anyTypeSchema);
//        mapWithAddPropsTrue = co.responses.get(1);
//        assertEquals(mapWithAddPropsTrue.getAdditionalProperties(), anyTypeSchema);
//        mapWithAddPropsFalse = co.responses.get(2);
//        assertEquals(mapWithAddPropsFalse.getAdditionalProperties(), null);
//        mapWithAddPropsSchema = co.responses.get(3);
//        assertEquals(mapWithAddPropsSchema.getAdditionalProperties(), stringCp);
//
//        path = "/additional_properties/";
//        operation = openAPI.getPaths().get(path).getPost();
//        co = codegen.fromOperation(path, "POST", operation, null);
//        mapWithAddPropsUnset = co.responses.get(0);
//        assertEquals(mapWithAddPropsUnset.getAdditionalProperties(), anyTypeSchema);
//        mapWithAddPropsTrue = co.responses.get(1);
//        assertEquals(mapWithAddPropsTrue.getAdditionalProperties(), anyTypeSchema);
//        mapWithAddPropsFalse = co.responses.get(2);
//        assertEquals(mapWithAddPropsFalse.getAdditionalProperties(), null);
//        mapWithAddPropsSchema = co.responses.get(3);
//        assertEquals(mapWithAddPropsSchema.getAdditionalProperties(), stringCp);
//    }

//    @Test
//    public void testIsXPresence() {
//        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/issue_7651.yaml");
//        final DefaultCodegen codegen = new DefaultCodegen();
//        codegen.setOpenAPI(openAPI);
//
//        String modelName;
//        Schema sc;
//        CodegenModel cm;
//
//        modelName = "DateWithValidation";
//        sc = openAPI.getComponents().getSchemas().get(modelName);
//        cm = codegen.fromModel(modelName, sc);
//        assertEquals(cm.isString, false);
//        assertEquals(cm.isDate, true);
//
//        modelName = "ObjectWithDateWithValidation";
//        sc = openAPI.getComponents().getSchemas().get(modelName);
//        cm = codegen.fromModel(modelName, sc);
//        assertEquals(cm.getVars().get(0).isString, false);
//        assertEquals(cm.getVars().get(0).isDate, true);
//
//        String path;
//        Operation operation;
//        CodegenOperation co;
//
//        path = "/ref_date_with_validation/{date}";
//        operation = openAPI.getPaths().get(path).getPost();
//        co = codegen.fromOperation(path, "POST", operation, null);
//        assertEquals(co.pathParams.get(0).isString, false);
//        assertEquals(co.pathParams.get(0).isDate, true);
//        assertEquals(co.bodyParams.get(0).isString, false);
//        assertEquals(co.bodyParams.get(0).isDate, true);
//        assertEquals(co.responses.get(0).isString, false);
//        assertEquals(co.responses.get(0).isDate, true);
//
//        path = "/date_with_validation/{date}";
//        operation = openAPI.getPaths().get(path).getPost();
//        co = codegen.fromOperation(path, "POST", operation, null);
//        assertEquals(co.pathParams.get(0).isString, false);
//        assertEquals(co.pathParams.get(0).isDate, true);
//        assertEquals(co.bodyParams.get(0).isString, false);
//        assertEquals(co.bodyParams.get(0).isDate, true);
//        assertEquals(co.responses.get(0).isString, false);
//        assertEquals(co.responses.get(0).isDate, true);
//
//        modelName = "DateTimeWithValidation";
//        sc = openAPI.getComponents().getSchemas().get(modelName);
//        cm = codegen.fromModel(modelName, sc);
//        assertEquals(cm.isString, false);
//        assertEquals(cm.isDateTime, true);
//
//        modelName = "ObjectWithDateTimeWithValidation";
//        sc = openAPI.getComponents().getSchemas().get(modelName);
//        cm = codegen.fromModel(modelName, sc);
//        assertEquals(cm.getVars().get(0).isString, false);
//        assertEquals(cm.getVars().get(0).isDateTime, true);
//
//        path = "/ref_date_time_with_validation/{dateTime}";
//        operation = openAPI.getPaths().get(path).getPost();
//        co = codegen.fromOperation(path, "POST", operation, null);
//        assertEquals(co.pathParams.get(0).isString, false);
//        assertEquals(co.pathParams.get(0).isDateTime, true);
//        assertEquals(co.bodyParams.get(0).isString, false);
//        assertEquals(co.bodyParams.get(0).isDateTime, true);
//        assertEquals(co.responses.get(0).isString, false);
//        assertEquals(co.responses.get(0).isDateTime, true);
//
//        path = "/date_time_with_validation/{dateTime}";
//        operation = openAPI.getPaths().get(path).getPost();
//        co = codegen.fromOperation(path, "POST", operation, null);
//        assertEquals(co.pathParams.get(0).isString, false);
//        assertEquals(co.pathParams.get(0).isDateTime, true);
//        assertEquals(co.bodyParams.get(0).isString, false);
//        assertEquals(co.bodyParams.get(0).isDateTime, true);
//        assertEquals(co.responses.get(0).isString, false);
//        assertEquals(co.responses.get(0).isDateTime, true);
//    }
//
//    @Test
//    public void testVarsAndRequiredVarsPresent() {
//        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/issue_7613.yaml");
//        final DefaultCodegen codegen = new DefaultCodegen();
//        codegen.setOpenAPI(openAPI);
//        codegen.setDisallowAdditionalPropertiesIfNotPresent(false);
//
//        String modelName;
//        Schema sc;
//        CodegenModel cm;
//        CodegenProperty propA = codegen.fromProperty("a", new Schema().type("string").minLength(1));
//        propA.setRequired(true);
//        CodegenProperty propB = codegen.fromProperty("b", new Schema().type("string").minLength(1));
//        propB.setRequired(true);
//        CodegenProperty propC = codegen.fromProperty("c", new Schema().type("string").minLength(1));
//        propC.setRequired(false);
//
//        List<CodegenProperty> vars = new ArrayList<>(Arrays.asList(propA, propB, propC));
//        List<CodegenProperty> requiredVars = new ArrayList<>(Arrays.asList(propA, propB));
//
//        modelName = "ObjectWithOptionalAndRequiredProps";
//        sc = openAPI.getComponents().getSchemas().get(modelName);
//        cm = codegen.fromModel(modelName, sc);
//        assertEquals(cm.vars, vars);
//        assertEquals(cm.requiredVars, requiredVars);
//
//        String path;
//        Operation operation;
//        CodegenOperation co;
//
//        path = "/object_with_optional_and_required_props/{objectData}";
//        operation = openAPI.getPaths().get(path).getPost();
//        co = codegen.fromOperation(path, "POST", operation, null);
//        assertEquals(co.pathParams.get(0).vars, vars);
//        assertEquals(co.pathParams.get(0).requiredVars, requiredVars);
//        assertEquals(co.bodyParams.get(0).vars, vars);
//        assertEquals(co.bodyParams.get(0).requiredVars, requiredVars);
//
//        // CodegenOperation puts the inline schema into schemas and refs it
//        assertEquals(co.responses.get(0).isModel, true);
//        assertEquals(co.responses.get(0).baseType, "objectData");
//        modelName = "objectData";
//        sc = openAPI.getComponents().getSchemas().get(modelName);
//        cm = codegen.fromModel(modelName, sc);
//        assertEquals(cm.vars, vars);
//        assertEquals(cm.requiredVars, requiredVars);
//
//        // CodegenProperty puts the inline schema into schemas and refs it
//        modelName = "ObjectPropContainsProps";
//        sc = openAPI.getComponents().getSchemas().get(modelName);
//        cm = codegen.fromModel(modelName, sc);
//        CodegenProperty cp = cm.getVars().get(0);
//        assertEquals(cp.isModel, true);
//        assertEquals(cp.complexType, "objectData");
//    }
}
