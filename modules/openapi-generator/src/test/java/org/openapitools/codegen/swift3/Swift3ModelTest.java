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

package org.openapitools.codegen.swift3;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.parser.util.SchemaTypeUtil;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.Swift3Codegen;
import org.testng.Assert;
import org.testng.annotations.Test;

@SuppressWarnings("static-method")
public class Swift3ModelTest {

    @Test(description = "convert a simple java model")
    public void simpleModelTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("id", new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperties("name", new StringSchema())
                .addProperties("createdAt", new DateTimeSchema())
                .addProperties("binary", new BinarySchema())
                .addProperties("byte", new ByteArraySchema())
                .addProperties("uuid", new UUIDSchema())
                .addProperties("dateOfBirth", new DateSchema())
                .addRequiredItem("id")
                .addRequiredItem("name")
                .discriminator(new Discriminator().propertyName("test"));

        final DefaultCodegen codegen = new Swift3Codegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 7);
        Assert.assertEquals(cm.getDiscriminatorName(),"test");

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "id");
        Assert.assertEquals(property1.dataType, "Int64");
        Assert.assertEquals(property1.name, "id");
        Assert.assertNull(property1.defaultValue);
        Assert.assertEquals(property1.baseType, "Int64");
        Assert.assertTrue(property1.hasMore);
        Assert.assertTrue(property1.required);
        Assert.assertTrue(property1.isPrimitiveType);
        Assert.assertFalse(property1.isContainer);

        final CodegenProperty property2 = cm.vars.get(1);
        Assert.assertEquals(property2.baseName, "name");
        Assert.assertEquals(property2.dataType, "String");
        Assert.assertEquals(property2.name, "name");
        Assert.assertNull(property2.defaultValue);
        Assert.assertEquals(property2.baseType, "String");
        Assert.assertTrue(property2.hasMore);
        Assert.assertTrue(property2.required);
        Assert.assertTrue(property2.isPrimitiveType);
        Assert.assertFalse(property2.isContainer);

        final CodegenProperty property3 = cm.vars.get(2);
        Assert.assertEquals(property3.baseName, "createdAt");
        Assert.assertEquals(property3.dataType, "Date");
        Assert.assertEquals(property3.name, "createdAt");
        Assert.assertNull(property3.defaultValue);
        Assert.assertEquals(property3.baseType, "Date");
        Assert.assertTrue(property3.hasMore);
        Assert.assertFalse(property3.required);
        Assert.assertFalse(property3.isContainer);

        final CodegenProperty property4 = cm.vars.get(3);
        Assert.assertEquals(property4.baseName, "binary");
        Assert.assertEquals(property4.dataType, "URL");
        Assert.assertEquals(property4.name, "binary");
        Assert.assertNull(property4.defaultValue);
        Assert.assertEquals(property4.baseType, "URL");
        Assert.assertTrue(property4.hasMore);
        Assert.assertFalse(property4.required);
        Assert.assertFalse(property4.isContainer);

        final CodegenProperty property5 = cm.vars.get(4);
        Assert.assertEquals(property5.baseName, "byte");
        Assert.assertEquals(property5.dataType, "Data");
        Assert.assertEquals(property5.name, "byte");
        Assert.assertNull(property5.defaultValue);
        Assert.assertEquals(property5.baseType, "Data");
        Assert.assertTrue(property5.hasMore);
        Assert.assertFalse(property5.required);
        Assert.assertFalse(property5.isContainer);

        final CodegenProperty property6 = cm.vars.get(5);
        Assert.assertEquals(property6.baseName, "uuid");
        Assert.assertEquals(property6.dataType, "UUID");
        Assert.assertEquals(property6.name, "uuid");
        Assert.assertNull(property6.defaultValue);
        Assert.assertEquals(property6.baseType, "UUID");
        Assert.assertTrue(property6.hasMore);
        Assert.assertFalse(property6.required);
        Assert.assertFalse(property6.isContainer);

        final CodegenProperty property7 = cm.vars.get(6);
        Assert.assertEquals(property7.baseName, "dateOfBirth");
        Assert.assertEquals(property7.dataType, "ISOFullDate");
        Assert.assertEquals(property7.name, "dateOfBirth");
        Assert.assertNull(property7.defaultValue);
        Assert.assertEquals(property7.baseType, "ISOFullDate");
        Assert.assertFalse(property7.hasMore);
        Assert.assertFalse(property7.required);
        Assert.assertFalse(property7.isContainer);
    }

}
