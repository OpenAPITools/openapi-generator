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

package org.openapitools.codegen.swift6;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.parser.util.SchemaTypeUtil;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.Swift6ClientCodegen;
import org.openapitools.codegen.utils.ModelUtils;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.Map;

@SuppressWarnings("static-method")
public class Swift6ClientCodegenModelTest {

    @Test(description = "convert a simple java model", enabled = true)
    public void simpleModelTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperty("id", new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperty("name", new StringSchema())
                .addProperty("createdAt", new DateTimeSchema())
                .addProperty("binary", new BinarySchema())
                .addProperty("byte", new ByteArraySchema())
                .addProperty("uuid", new UUIDSchema())
                .addProperty("dateOfBirth", new DateSchema())
                .addRequiredItem("id")
                .addRequiredItem("name")
                .discriminator(new Discriminator().propertyName("test"));
        final DefaultCodegen codegen = new Swift6ClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 7);
        Assert.assertEquals(cm.getDiscriminatorName(), "test");

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "id");
        Assert.assertEquals(property1.dataType, "Int64");
        Assert.assertEquals(property1.name, "id");
        Assert.assertNull(property1.defaultValue);
        Assert.assertEquals(property1.baseType, "Int64");
        Assert.assertTrue(property1.required);
        Assert.assertTrue(property1.isPrimitiveType);
        Assert.assertFalse(property1.isContainer);

        final CodegenProperty property2 = cm.vars.get(1);
        Assert.assertEquals(property2.baseName, "name");
        Assert.assertEquals(property2.dataType, "String");
        Assert.assertEquals(property2.name, "name");
        Assert.assertNull(property2.defaultValue);
        Assert.assertEquals(property2.baseType, "String");
        Assert.assertTrue(property2.required);
        Assert.assertTrue(property2.isPrimitiveType);
        Assert.assertFalse(property2.isContainer);

        final CodegenProperty property3 = cm.vars.get(2);
        Assert.assertEquals(property3.baseName, "createdAt");
        Assert.assertEquals(property3.dataType, "Date");
        Assert.assertEquals(property3.name, "createdAt");
        Assert.assertNull(property3.defaultValue);
        Assert.assertEquals(property3.baseType, "Date");
        Assert.assertFalse(property3.required);
        Assert.assertFalse(property3.isContainer);

        final CodegenProperty property4 = cm.vars.get(3);
        Assert.assertEquals(property4.baseName, "binary");
        Assert.assertEquals(property4.dataType, "URL");
        Assert.assertEquals(property4.name, "binary");
        Assert.assertNull(property4.defaultValue);
        Assert.assertEquals(property4.baseType, "URL");
        Assert.assertFalse(property4.required);
        Assert.assertFalse(property4.isContainer);

        final CodegenProperty property5 = cm.vars.get(4);
        Assert.assertEquals(property5.baseName, "byte");
        Assert.assertEquals(property5.dataType, "Data");
        Assert.assertEquals(property5.name, "byte");
        Assert.assertNull(property5.defaultValue);
        Assert.assertEquals(property5.baseType, "Data");
        Assert.assertFalse(property5.required);
        Assert.assertFalse(property5.isContainer);

        final CodegenProperty property6 = cm.vars.get(5);
        Assert.assertEquals(property6.baseName, "uuid");
        Assert.assertEquals(property6.dataType, "UUID");
        Assert.assertEquals(property6.name, "uuid");
        Assert.assertNull(property6.defaultValue);
        Assert.assertEquals(property6.baseType, "UUID");
        Assert.assertFalse(property6.required);
        Assert.assertFalse(property6.isContainer);

        final CodegenProperty property7 = cm.vars.get(6);
        Assert.assertEquals(property7.baseName, "dateOfBirth");
        Assert.assertEquals(property7.dataType, "Date");
        Assert.assertEquals(property7.name, "dateOfBirth");
        Assert.assertNull(property7.defaultValue);
        Assert.assertEquals(property7.baseType, "Date");
        Assert.assertFalse(property7.required);
        Assert.assertFalse(property7.isContainer);
    }

    @Test(description = "convert a simple java model", enabled = true)
    public void useCustomDateTimeTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperty("id", new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperty("name", new StringSchema())
                .addProperty("createdAt", new DateTimeSchema())
                .addProperty("binary", new BinarySchema())
                .addProperty("byte", new ByteArraySchema())
                .addProperty("uuid", new UUIDSchema())
                .addProperty("dateOfBirth", new DateSchema())
                .addRequiredItem("id")
                .addRequiredItem("name")
                .discriminator(new Discriminator().propertyName("test"));
        final DefaultCodegen codegen = new Swift6ClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        codegen.additionalProperties().put(Swift6ClientCodegen.USE_CUSTOM_DATE_WITHOUT_TIME, true);
        codegen.processOpts();

        final CodegenModel cm = codegen.fromModel("sample", schema);
        final CodegenProperty property7 = cm.vars.get(6);

        final CodegenProperty property3 = cm.vars.get(2);
        Assert.assertEquals(property3.baseName, "createdAt");
        Assert.assertEquals(property3.dataType, "Date");
        Assert.assertEquals(property3.name, "createdAt");
        Assert.assertNull(property3.defaultValue);
        Assert.assertEquals(property3.baseType, "Date");
        Assert.assertFalse(property3.required);
        Assert.assertFalse(property3.isContainer);

        Assert.assertEquals(property7.baseName, "dateOfBirth");
        Assert.assertEquals(property7.dataType, "OpenAPIDateWithoutTime");
        Assert.assertEquals(property7.name, "dateOfBirth");
        Assert.assertNull(property7.defaultValue);
        Assert.assertEquals(property7.baseType, "OpenAPIDateWithoutTime");
        Assert.assertFalse(property7.required);
        Assert.assertFalse(property7.isContainer);
    }

    @Test(description = "anyOf with different required fields should use intersection for required", enabled = true)
    public void anyOfRequiredFieldsIntersectionTest() {
        // VoteResponse requires: status, voteId
        // APIError requires: status, reason, code
        // Intersection: only "status" should be required in the merged model
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/swift6_anyof_required.yaml");
        final Swift6ClientCodegen codegen = new Swift6ClientCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.processOpts();

        // The inline model resolver creates a model for the anyOf response.
        // Find the composed schema that merges VoteResponse and APIError.
        Map<String, Schema> schemas = ModelUtils.getSchemas(openAPI);
        Schema composedSchema = null;
        String composedName = null;
        for (Map.Entry<String, Schema> entry : schemas.entrySet()) {
            Schema s = entry.getValue();
            if (s.getAnyOf() != null && !s.getAnyOf().isEmpty()) {
                composedSchema = s;
                composedName = entry.getKey();
                break;
            }
        }
        Assert.assertNotNull(composedSchema, "Should find an anyOf composed schema");

        final CodegenModel cm = codegen.fromModel(composedName, composedSchema);

        // "status" is required in BOTH VoteResponse and APIError -> should be required
        CodegenProperty statusProp = cm.vars.stream()
                .filter(p -> p.baseName.equals("status"))
                .findFirst().orElse(null);
        Assert.assertNotNull(statusProp, "status property should exist");
        Assert.assertTrue(statusProp.required, "status should be required (present in all anyOf members)");

        // "voteId" is required only in VoteResponse, not in APIError -> should NOT be required
        CodegenProperty voteIdProp = cm.vars.stream()
                .filter(p -> p.baseName.equals("voteId"))
                .findFirst().orElse(null);
        Assert.assertNotNull(voteIdProp, "voteId property should exist");
        Assert.assertFalse(voteIdProp.required, "voteId should NOT be required (only in VoteResponse, not APIError)");

        // "reason" is required only in APIError, not in VoteResponse -> should NOT be required
        CodegenProperty reasonProp = cm.vars.stream()
                .filter(p -> p.baseName.equals("reason"))
                .findFirst().orElse(null);
        Assert.assertNotNull(reasonProp, "reason property should exist");
        Assert.assertFalse(reasonProp.required, "reason should NOT be required (only in APIError, not VoteResponse)");

        // "code" is required only in APIError, not in VoteResponse -> should NOT be required
        CodegenProperty codeProp = cm.vars.stream()
                .filter(p -> p.baseName.equals("code"))
                .findFirst().orElse(null);
        Assert.assertNotNull(codeProp, "code property should exist");
        Assert.assertFalse(codeProp.required, "code should NOT be required (only in APIError, not VoteResponse)");

        // "isVerified" is optional in VoteResponse, not in APIError -> should NOT be required
        CodegenProperty isVerifiedProp = cm.vars.stream()
                .filter(p -> p.baseName.equals("isVerified"))
                .findFirst().orElse(null);
        Assert.assertNotNull(isVerifiedProp, "isVerified property should exist");
        Assert.assertFalse(isVerifiedProp.required, "isVerified should NOT be required");

        // "secondaryCode" is optional in APIError -> should NOT be required
        CodegenProperty secondaryCodeProp = cm.vars.stream()
                .filter(p -> p.baseName.equals("secondaryCode"))
                .findFirst().orElse(null);
        Assert.assertNotNull(secondaryCodeProp, "secondaryCode property should exist");
        Assert.assertFalse(secondaryCodeProp.required, "secondaryCode should NOT be required");
    }

}
