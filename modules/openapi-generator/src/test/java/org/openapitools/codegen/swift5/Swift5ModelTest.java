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

package org.openapitools.codegen.swift5;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.parser.util.SchemaTypeUtil;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.Swift5ClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

@SuppressWarnings("static-method")
public class Swift5ModelTest {

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
        final DefaultCodegen codegen = new Swift5ClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 7);
        Assertions.assertEquals(cm.getDiscriminatorName(),"test");

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "id");
        Assertions.assertEquals(property1.dataType, "Int64");
        Assertions.assertEquals(property1.name, "id");
        Assertions.assertNull(property1.defaultValue);
        Assertions.assertEquals(property1.baseType, "Int64");
        Assertions.assertTrue(property1.required);
        Assertions.assertTrue(property1.isPrimitiveType);
        Assertions.assertFalse(property1.isContainer);

        final CodegenProperty property2 = cm.vars.get(1);
        Assertions.assertEquals(property2.baseName, "name");
        Assertions.assertEquals(property2.dataType, "String");
        Assertions.assertEquals(property2.name, "name");
        Assertions.assertNull(property2.defaultValue);
        Assertions.assertEquals(property2.baseType, "String");
        Assertions.assertTrue(property2.required);
        Assertions.assertTrue(property2.isPrimitiveType);
        Assertions.assertFalse(property2.isContainer);

        final CodegenProperty property3 = cm.vars.get(2);
        Assertions.assertEquals(property3.baseName, "createdAt");
        Assertions.assertEquals(property3.dataType, "Date");
        Assertions.assertEquals(property3.name, "createdAt");
        Assertions.assertNull(property3.defaultValue);
        Assertions.assertEquals(property3.baseType, "Date");
        Assertions.assertFalse(property3.required);
        Assertions.assertFalse(property3.isContainer);

        final CodegenProperty property4 = cm.vars.get(3);
        Assertions.assertEquals(property4.baseName, "binary");
        Assertions.assertEquals(property4.dataType, "URL");
        Assertions.assertEquals(property4.name, "binary");
        Assertions.assertNull(property4.defaultValue);
        Assertions.assertEquals(property4.baseType, "URL");
        Assertions.assertFalse(property4.required);
        Assertions.assertFalse(property4.isContainer);

        final CodegenProperty property5 = cm.vars.get(4);
        Assertions.assertEquals(property5.baseName, "byte");
        Assertions.assertEquals(property5.dataType, "Data");
        Assertions.assertEquals(property5.name, "byte");
        Assertions.assertNull(property5.defaultValue);
        Assertions.assertEquals(property5.baseType, "Data");
        Assertions.assertFalse(property5.required);
        Assertions.assertFalse(property5.isContainer);

        final CodegenProperty property6 = cm.vars.get(5);
        Assertions.assertEquals(property6.baseName, "uuid");
        Assertions.assertEquals(property6.dataType, "UUID");
        Assertions.assertEquals(property6.name, "uuid");
        Assertions.assertNull(property6.defaultValue);
        Assertions.assertEquals(property6.baseType, "UUID");
        Assertions.assertFalse(property6.required);
        Assertions.assertFalse(property6.isContainer);

        final CodegenProperty property7 = cm.vars.get(6);
        Assertions.assertEquals(property7.baseName, "dateOfBirth");
        Assertions.assertEquals(property7.dataType, "Date");
        Assertions.assertEquals(property7.name, "dateOfBirth");
        Assertions.assertNull(property7.defaultValue);
        Assertions.assertEquals(property7.baseType, "Date");
        Assertions.assertFalse(property7.required);
        Assertions.assertFalse(property7.isContainer);
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
        final DefaultCodegen codegen = new Swift5ClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        codegen.additionalProperties().put(Swift5ClientCodegen.USE_CUSTOM_DATE_WITHOUT_TIME, true);
        codegen.processOpts();

        final CodegenModel cm = codegen.fromModel("sample", schema);
        final CodegenProperty property7 = cm.vars.get(6);

        final CodegenProperty property3 = cm.vars.get(2);
        Assertions.assertEquals(property3.baseName, "createdAt");
        Assertions.assertEquals(property3.dataType, "Date");
        Assertions.assertEquals(property3.name, "createdAt");
        Assertions.assertNull(property3.defaultValue);
        Assertions.assertEquals(property3.baseType, "Date");
        Assertions.assertFalse(property3.required);
        Assertions.assertFalse(property3.isContainer);

        Assertions.assertEquals(property7.baseName, "dateOfBirth");
        Assertions.assertEquals(property7.dataType, "OpenAPIDateWithoutTime");
        Assertions.assertEquals(property7.name, "dateOfBirth");
        Assertions.assertNull(property7.defaultValue);
        Assertions.assertEquals(property7.baseType, "OpenAPIDateWithoutTime");
        Assertions.assertFalse(property7.required);
        Assertions.assertFalse(property7.isContainer);
    }

}
