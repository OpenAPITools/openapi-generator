/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

package org.openapitools.codegen.ktorm;

import static org.openapitools.codegen.TestUtils.createCodegenModelWrapper;

import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.KtormSchemaCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.parser.util.SchemaTypeUtil;

import java.util.Map;

public class KtormSchemaCodegenTest {

    private CodegenModel getModel(Schema schema, String pkName, Boolean surrogateKey) {
        final KtormSchemaCodegen codegen = new KtormSchemaCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setAddSurrogateKey(surrogateKey);
        codegen.setPrimaryKeyConvention(pkName);
        codegen.setOpenAPI(openAPI);
        CodegenModel cm = codegen.fromModel("sample", schema);
        codegen.postProcessModels(createCodegenModelWrapper(cm));
        return cm;
    }

    private Map<String, Object> getExtension(CodegenProperty property) {
        return (Map<String, Object>)
            property.vendorExtensions.get(KtormSchemaCodegen.VENDOR_EXTENSION_SCHEMA);
    }

    private Map<String, Object> getColumnDefinition(Map<String, Object> schema) {
        return (Map<String, Object>)
            schema.get("columnDefinition");
    }

    private Map<String, Object> getRelationDefinition(Map<String, Object> schema) {
        return (Map<String, Object>)
            schema.get("relationDefinition");
    }

    private Map<String, Object> getKtormSchema(Schema propertySchema) {
        final Schema schema = new Schema()
            .description("a sample model")
            .addProperties("key", propertySchema)
            .addRequiredItem("key");
        final CodegenModel cm = getModel(schema, "id", false);
        final CodegenProperty prop = cm.vars.get(0);
        return getExtension(prop);
    }

    private String getMatchedColType(Schema propertySchema) {
        Map<String, Object> ktormSchema = getColumnDefinition(getKtormSchema(propertySchema));
        return (String) ktormSchema.get("colType");
    }

    private String getMatchedKotlinType(Schema propertySchema) {
        Map<String, Object> ktormSchema = getColumnDefinition(getKtormSchema(propertySchema));
        return (String) ktormSchema.get("colKotlinType");
    }

    private String getMatchedRelation(Schema propertySchema) {
        Map<String, Object> ktormSchema = getRelationDefinition(getKtormSchema(propertySchema));
        if (ktormSchema == null) return null;
        return (String) ktormSchema.get("fkName");
    }

    @Test
    public void testMatchedColType() {
        Assertions.assertEquals(getMatchedColType(new StringSchema()), "text");
        Assertions.assertEquals(getMatchedColType(new StringSchema().type("char")), "text");
        Assertions.assertEquals(getMatchedColType(new StringSchema().format("char")), "text");
        Assertions.assertEquals(getMatchedColType(new BooleanSchema()), "boolean");
        Assertions.assertEquals(getMatchedColType(new IntegerSchema().type(SchemaTypeUtil.BYTE_FORMAT)), "int");
        Assertions.assertEquals(getMatchedColType(new IntegerSchema().format(SchemaTypeUtil.BYTE_FORMAT)), "int");
        Assertions.assertEquals(getMatchedColType(new IntegerSchema().type("short")), "int");
        Assertions.assertEquals(getMatchedColType(new IntegerSchema().format("short")), "int");
        Assertions.assertEquals(getMatchedColType(new IntegerSchema()), "int");
        Assertions.assertEquals(getMatchedColType(new IntegerSchema().type("integer")), "int");
        Assertions.assertEquals(getMatchedColType(new IntegerSchema().format("integer")), "int");
        Assertions.assertEquals(getMatchedColType(new IntegerSchema().format(SchemaTypeUtil.INTEGER32_FORMAT)), "int");
        Assertions.assertEquals(getMatchedColType(new IntegerSchema().type("long")), "long");
        Assertions.assertEquals(getMatchedColType(new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT)), "long");
        Assertions.assertEquals(getMatchedColType(new ObjectSchema().type(SchemaTypeUtil.FLOAT_FORMAT)), "float");
        Assertions.assertEquals(getMatchedColType(new NumberSchema().format(SchemaTypeUtil.FLOAT_FORMAT)), "float");
        Assertions.assertEquals(getMatchedColType(new ObjectSchema().type(SchemaTypeUtil.DOUBLE_FORMAT)), "double");
        Assertions.assertEquals(getMatchedColType(new NumberSchema().format(SchemaTypeUtil.DOUBLE_FORMAT)), "double");
        Assertions.assertEquals(getMatchedColType(new ObjectSchema().type(SchemaTypeUtil.FLOAT_FORMAT).format(SchemaTypeUtil.DOUBLE_FORMAT)), "float");
        Assertions.assertEquals(getMatchedColType(new ObjectSchema().type(SchemaTypeUtil.DOUBLE_FORMAT).format(SchemaTypeUtil.FLOAT_FORMAT)), "double");
        Assertions.assertEquals(getMatchedColType(new ObjectSchema().type("real")), "double");
        Assertions.assertEquals(getMatchedColType(new NumberSchema().format("real")), "decimal");
        Assertions.assertEquals(getMatchedColType(new NumberSchema().type(SchemaTypeUtil.NUMBER_TYPE)), "decimal");
        Assertions.assertEquals(getMatchedColType(new NumberSchema().type("decimal")), "decimal");
        Assertions.assertEquals(getMatchedColType(new NumberSchema().type("BigDecimal")), "decimal");
        Assertions.assertEquals(getMatchedColType(new ByteArraySchema()), "blob");
        Assertions.assertEquals(getMatchedColType(new ArraySchema().items(new IntegerSchema().type(SchemaTypeUtil.BYTE_FORMAT))), "blob");
        Assertions.assertEquals(getMatchedColType(new ArraySchema().items(new IntegerSchema().format(SchemaTypeUtil.BYTE_FORMAT))), "blob");
        Assertions.assertEquals(getMatchedColType(new ArraySchema()), "blob");
        Assertions.assertEquals(getMatchedColType(new ObjectSchema().type("list")), "blob");
        Assertions.assertEquals(getMatchedColType(new ObjectSchema().type("set")), "blob");
        Assertions.assertEquals(getMatchedColType(new ObjectSchema().type("map")), "blob");
        Assertions.assertEquals(getMatchedColType(new ObjectSchema()), "blob");
        Assertions.assertEquals(getMatchedColType(new ObjectSchema().type("binary")), "blob");
        Assertions.assertEquals(getMatchedColType(new ObjectSchema().type("AnyType")), "blob");
        Assertions.assertEquals(getMatchedColType(new BinarySchema()), "blob");
        Assertions.assertEquals(getMatchedColType(new FileSchema()), "blob");
        Assertions.assertEquals(getMatchedColType(new DateSchema()), "date");
        Assertions.assertEquals(getMatchedColType(new DateTimeSchema()), "datetime");
        Assertions.assertEquals(getMatchedColType(new UUIDSchema()), "text");
        Assertions.assertEquals(getMatchedColType(new ObjectSchema().type("UUID")), "text");
        Assertions.assertEquals(getMatchedColType(new StringSchema().format("URI")), "text");
        Assertions.assertEquals(getMatchedColType(new ObjectSchema().type("URI")), "text");
        Assertions.assertEquals(getMatchedColType(new StringSchema().format("password")), "text");
        Assertions.assertEquals(getMatchedColType(new StringSchema().type("password")), "text");
    }

    @Test
    public void testMatchedColKotlinType() {
        // *1 - format specifiers aren't used
        Assertions.assertEquals(getMatchedKotlinType(new StringSchema()), "kotlin.String");
        Assertions.assertEquals(getMatchedKotlinType(new StringSchema().type("char")), "kotlin.String");
        Assertions.assertEquals(getMatchedKotlinType(new StringSchema().format("char")), "kotlin.String");
        Assertions.assertEquals(getMatchedKotlinType(new BooleanSchema()), "kotlin.Boolean");
        Assertions.assertEquals(getMatchedKotlinType(new IntegerSchema().type(SchemaTypeUtil.BYTE_FORMAT)), "kotlin.Byte");
        Assertions.assertEquals(getMatchedKotlinType(new IntegerSchema().format(SchemaTypeUtil.BYTE_FORMAT)), "kotlin.Int"); //*1
        Assertions.assertEquals(getMatchedKotlinType(new IntegerSchema().type("short")), "kotlin.Short");
        Assertions.assertEquals(getMatchedKotlinType(new IntegerSchema().format("short")), "kotlin.Int"); //*1
        Assertions.assertEquals(getMatchedKotlinType(new IntegerSchema()), "kotlin.Int");
        Assertions.assertEquals(getMatchedKotlinType(new IntegerSchema().type("integer")), "kotlin.Int");
        Assertions.assertEquals(getMatchedKotlinType(new IntegerSchema().format("integer")), "kotlin.Int"); //*1
        Assertions.assertEquals(getMatchedKotlinType(new IntegerSchema().format(SchemaTypeUtil.INTEGER32_FORMAT)), "kotlin.Int");
        Assertions.assertEquals(getMatchedKotlinType(new IntegerSchema().type("long")), "kotlin.Long");
        Assertions.assertEquals(getMatchedKotlinType(new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT)), "kotlin.Long");
        Assertions.assertEquals(getMatchedKotlinType(new ObjectSchema().type(SchemaTypeUtil.FLOAT_FORMAT)), "kotlin.Float");
        Assertions.assertEquals(getMatchedKotlinType(new NumberSchema().format(SchemaTypeUtil.FLOAT_FORMAT)), "kotlin.Float");
        Assertions.assertEquals(getMatchedKotlinType(new ObjectSchema().type(SchemaTypeUtil.DOUBLE_FORMAT)), "kotlin.Double");
        Assertions.assertEquals(getMatchedKotlinType(new NumberSchema().format(SchemaTypeUtil.DOUBLE_FORMAT)), "kotlin.Double");
        Assertions.assertEquals(getMatchedKotlinType(new ObjectSchema().type(SchemaTypeUtil.FLOAT_FORMAT).format(SchemaTypeUtil.DOUBLE_FORMAT)), "kotlin.Float"); //*1
        Assertions.assertEquals(getMatchedKotlinType(new ObjectSchema().type(SchemaTypeUtil.DOUBLE_FORMAT).format(SchemaTypeUtil.FLOAT_FORMAT)), "kotlin.Double"); //*1
        Assertions.assertEquals(getMatchedKotlinType(new ObjectSchema().type("real")), "kotlin.Double");
        Assertions.assertEquals(getMatchedKotlinType(new NumberSchema().format("real")), "java.math.BigDecimal"); //*1
        Assertions.assertEquals(getMatchedKotlinType(new NumberSchema().type(SchemaTypeUtil.NUMBER_TYPE)), "java.math.BigDecimal");
        Assertions.assertEquals(getMatchedKotlinType(new NumberSchema().type("decimal")), "java.math.BigDecimal");
        Assertions.assertEquals(getMatchedKotlinType(new NumberSchema().type("BigDecimal")), "java.math.BigDecimal");
        Assertions.assertEquals(getMatchedKotlinType(new ByteArraySchema()), "kotlin.ByteArray");
        Assertions.assertEquals(getMatchedKotlinType(new ArraySchema().items(new IntegerSchema().type(SchemaTypeUtil.BYTE_FORMAT))), "kotlin.Array<kotlin.Byte>");
        Assertions.assertEquals(getMatchedKotlinType(new ArraySchema().items(new IntegerSchema().format(SchemaTypeUtil.BYTE_FORMAT))), "kotlin.Array<kotlin.Int>"); //*1
        Assertions.assertEquals(getMatchedKotlinType(new ArraySchema()), "kotlin.Array<kotlin.String>");
        Assertions.assertEquals(getMatchedKotlinType(new ObjectSchema().type("list")), "kotlin.collections.List");
        Assertions.assertEquals(getMatchedKotlinType(new ObjectSchema().type("set")), "kotlin.collections.Set");
        Assertions.assertEquals(getMatchedKotlinType(new ObjectSchema().type("map")), "kotlin.collections.Map");
        Assertions.assertEquals(getMatchedKotlinType(new ObjectSchema()), "kotlin.Any");
        Assertions.assertEquals(getMatchedKotlinType(new ObjectSchema().type("binary")), "kotlin.ByteArray");
        Assertions.assertEquals(getMatchedKotlinType(new ObjectSchema().type("AnyType")), "kotlin.Any");
        Assertions.assertEquals(getMatchedKotlinType(new BinarySchema()), "java.io.File"); //looks like a bug
        Assertions.assertEquals(getMatchedKotlinType(new FileSchema()), "java.io.File");
        Assertions.assertEquals(getMatchedKotlinType(new DateSchema()), "java.time.LocalDate");
        Assertions.assertEquals(getMatchedKotlinType(new DateTimeSchema()), "java.time.LocalDateTime");
        Assertions.assertEquals(getMatchedKotlinType(new UUIDSchema()), "java.util.UUID");
        Assertions.assertEquals(getMatchedKotlinType(new ObjectSchema().type("UUID")), "java.util.UUID");
        Assertions.assertEquals(getMatchedKotlinType(new StringSchema().format("URI")), "java.net.URI");
        Assertions.assertEquals(getMatchedKotlinType(new ObjectSchema().type("URI")), "java.net.URI");
        Assertions.assertEquals(getMatchedKotlinType(new StringSchema().format("password")), "kotlin.String");
        Assertions.assertEquals(getMatchedKotlinType(new StringSchema().type("password")), "kotlin.String");
    }

    @Test
    public void testNonMatchedRelation() {
        Assertions.assertEquals(getMatchedRelation(new StringSchema()), null);
        Assertions.assertEquals(getMatchedRelation(new StringSchema().type("char")), null);
        Assertions.assertEquals(getMatchedRelation(new StringSchema().format("char")), null);
        Assertions.assertEquals(getMatchedRelation(new BooleanSchema()), null);
        Assertions.assertEquals(getMatchedRelation(new IntegerSchema().type(SchemaTypeUtil.BYTE_FORMAT)), null);
        Assertions.assertEquals(getMatchedRelation(new IntegerSchema().format(SchemaTypeUtil.BYTE_FORMAT)), null);
        Assertions.assertEquals(getMatchedRelation(new IntegerSchema().type("short")), null);
        Assertions.assertEquals(getMatchedRelation(new IntegerSchema().format("short")), null);
        Assertions.assertEquals(getMatchedRelation(new IntegerSchema()), null);
        Assertions.assertEquals(getMatchedRelation(new IntegerSchema().type("integer")), null);
        Assertions.assertEquals(getMatchedRelation(new IntegerSchema().format("integer")), null);
        Assertions.assertEquals(getMatchedRelation(new IntegerSchema().format(SchemaTypeUtil.INTEGER32_FORMAT)), null);
        Assertions.assertEquals(getMatchedRelation(new IntegerSchema().type("long")), null);
        Assertions.assertEquals(getMatchedRelation(new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT)), null);
        Assertions.assertEquals(getMatchedRelation(new ObjectSchema().type(SchemaTypeUtil.FLOAT_FORMAT)), null);
        Assertions.assertEquals(getMatchedRelation(new NumberSchema().format(SchemaTypeUtil.FLOAT_FORMAT)), null);
        Assertions.assertEquals(getMatchedRelation(new ObjectSchema().type(SchemaTypeUtil.DOUBLE_FORMAT)), null);
        Assertions.assertEquals(getMatchedRelation(new NumberSchema().format(SchemaTypeUtil.DOUBLE_FORMAT)), null);
        Assertions.assertEquals(getMatchedRelation(new ObjectSchema().type(SchemaTypeUtil.FLOAT_FORMAT).format(SchemaTypeUtil.DOUBLE_FORMAT)), null);
        Assertions.assertEquals(getMatchedRelation(new ObjectSchema().type(SchemaTypeUtil.DOUBLE_FORMAT).format(SchemaTypeUtil.FLOAT_FORMAT)), null);
        Assertions.assertEquals(getMatchedRelation(new ObjectSchema().type("real")), null);
        Assertions.assertEquals(getMatchedRelation(new NumberSchema().format("real")), null);
        Assertions.assertEquals(getMatchedRelation(new NumberSchema().type(SchemaTypeUtil.NUMBER_TYPE)), null);
        Assertions.assertEquals(getMatchedRelation(new NumberSchema().type("decimal")), null);
        Assertions.assertEquals(getMatchedRelation(new NumberSchema().type("BigDecimal")), null);
        Assertions.assertEquals(getMatchedRelation(new ByteArraySchema()), null);
        Assertions.assertEquals(getMatchedRelation(new ObjectSchema().type("list")), null);
        Assertions.assertEquals(getMatchedRelation(new ObjectSchema().type("set")), null);
        Assertions.assertEquals(getMatchedRelation(new ObjectSchema().type("map")), null);
        Assertions.assertEquals(getMatchedRelation(new ObjectSchema()), null);
        Assertions.assertEquals(getMatchedRelation(new ObjectSchema().type("binary")), null);
        Assertions.assertEquals(getMatchedRelation(new ObjectSchema().type("AnyType")), null);
        Assertions.assertEquals(getMatchedRelation(new BinarySchema()), null);
        Assertions.assertEquals(getMatchedRelation(new FileSchema()), null);
        Assertions.assertEquals(getMatchedRelation(new DateSchema()), null);
        Assertions.assertEquals(getMatchedRelation(new DateTimeSchema()), null);
        Assertions.assertEquals(getMatchedRelation(new UUIDSchema()), null);
        Assertions.assertEquals(getMatchedRelation(new ObjectSchema().type("UUID")), null);
        Assertions.assertEquals(getMatchedRelation(new StringSchema().format("URI")), null);
        Assertions.assertEquals(getMatchedRelation(new ObjectSchema().type("URI")), null);
        Assertions.assertEquals(getMatchedRelation(new StringSchema().format("password")), null);
        Assertions.assertEquals(getMatchedRelation(new StringSchema().type("password")), null);
    }

    @Test
    public void testMatchedRelation() {
        //foreign keys
        Assertions.assertEquals(getMatchedRelation(new ObjectSchema().type("Something")), "something");
        Assertions.assertEquals(getMatchedColType(new ObjectSchema().type("Something")), "long");
        Assertions.assertEquals(getMatchedRelation(new ObjectSchema().type("UserNamespace.UserClass")), "userNamespaceUserClass");
        Assertions.assertEquals(getMatchedColType(new ObjectSchema().type("UserNamespace.UserClass")), "long");
        //arrays are special case, we convert them to 1:N relations
        Assertions.assertEquals(getMatchedRelation(new ArraySchema()), "key");
        Assertions.assertEquals(getMatchedRelation(new ArraySchema().items(new ObjectSchema().type("Something"))), "something");
        Assertions.assertEquals(getMatchedRelation(new ArraySchema().items(new ObjectSchema().type("UserNamespace.UserClass"))), "userNamespaceUserClass");
        Assertions.assertEquals(getMatchedRelation(new ArraySchema().items(new IntegerSchema().type(SchemaTypeUtil.BYTE_FORMAT))), "key");
        Assertions.assertEquals(getMatchedRelation(new ArraySchema().items(new StringSchema())), "key");
        //blob will be the default type, the template shouldn't include those fields
        Assertions.assertEquals(getMatchedColType(new ArraySchema()), "blob");
        Assertions.assertEquals(getMatchedColType(new ArraySchema().items(new ObjectSchema().type("Something"))), "blob");
        Assertions.assertEquals(getMatchedColType(new ArraySchema().items(new ObjectSchema().type("UserNamespace.UserClass"))), "blob");
        Assertions.assertEquals(getMatchedColType(new ArraySchema().items(new IntegerSchema().type(SchemaTypeUtil.BYTE_FORMAT))), "blob");
        Assertions.assertEquals(getMatchedColType(new ArraySchema().items(new StringSchema())), "blob");
    }

    @Test
    public void testDefinePrimaryKey() {
        final Schema schema = new Schema()
            .description("a sample model")
            .addProperties("key" , new IntegerSchema())
            .addRequiredItem("key");
        CodegenModel cm = getModel(schema, "key", false);
        Assertions.assertEquals(cm.vars.size(), 1);
        CodegenProperty prop = cm.vars.get(0);
        Map<String, Object>  propSchema = getColumnDefinition(getExtension(prop));
        Assertions.assertEquals(propSchema.get("colPrimaryKey"), true);
        Assertions.assertEquals(propSchema.get("colType"), "int");
    }

    @Test
    public void testDontAddSurrogateKey() {
        final Schema schema = new Schema()
            .description("a sample model")
            .addProperties("key" , new IntegerSchema())
            .addRequiredItem("key");
        CodegenModel cm = getModel(schema, "id", false);
        Assertions.assertEquals(cm.vars.size(), 1);
        CodegenProperty prop = cm.vars.get(0);
        Map<String, Object>  propSchema = getColumnDefinition(getExtension(prop));
        Assertions.assertEquals(propSchema.get("colPrimaryKey"), false);
        Assertions.assertEquals(propSchema.get("colType"), "int");
    }

    @Test
    public void testAddSurrogateKey() {
        final Schema schema = new Schema()
            .description("a sample model")
            .addProperties("key", new IntegerSchema());
        CodegenModel cm = getModel(schema, "id", true);
        Assertions.assertEquals(cm.vars.size(), 2);
        CodegenProperty prop = cm.vars.get(0);
        Map<String, Object>  propSchema = getColumnDefinition(getExtension(prop));
        Assertions.assertEquals(propSchema.get("colNotNull"), true);
        Assertions.assertEquals(propSchema.get("colPrimaryKey"), true);
        Assertions.assertEquals(propSchema.get("colName"), "id");
        Assertions.assertEquals(propSchema.get("colType"), "long"); //by default
        CodegenProperty prop2 = cm.vars.get(1);
        Map<String, Object>  propSchema2 = getColumnDefinition(getExtension(prop2));
        Assertions.assertEquals(propSchema2.get("colNotNull"), false);
        Assertions.assertEquals(propSchema2.get("colPrimaryKey"), false);
        Assertions.assertEquals(propSchema2.get("colName"), "key");
        Assertions.assertEquals(propSchema2.get("colType"), "int");

    }

}
