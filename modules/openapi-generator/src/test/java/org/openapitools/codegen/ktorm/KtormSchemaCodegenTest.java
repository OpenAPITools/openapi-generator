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

import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.KtormSchemaCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.parser.util.SchemaTypeUtil;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class KtormSchemaCodegenTest {

    private Map<String, Object> toObjs(CodegenModel cm) {
        Map<String, Object> objs = new HashMap<String, Object>();
        List<Object> models = new ArrayList<Object>();
        Map<String, Object> model = new HashMap<>();
        model.put("model", cm);
        models.add(model);
        objs.put("models", models);
        return objs;
    }

    private CodegenModel getModel(Schema schema, String pkName, Boolean surrogateKey) {
        final KtormSchemaCodegen codegen = new KtormSchemaCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setAddSurrogateKey(surrogateKey);
        codegen.setPrimaryKeyConvention(pkName);
        codegen.setOpenAPI(openAPI);
        CodegenModel cm = codegen.fromModel("sample", schema);
        codegen.postProcessModels(toObjs(cm));
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
        Assert.assertEquals(getMatchedColType(new StringSchema()), "text");
        Assert.assertEquals(getMatchedColType(new StringSchema().type("char")), "text");
        Assert.assertEquals(getMatchedColType(new StringSchema().format("char")), "text");
        Assert.assertEquals(getMatchedColType(new BooleanSchema()), "boolean");
        Assert.assertEquals(getMatchedColType(new IntegerSchema().type(SchemaTypeUtil.BYTE_FORMAT)), "int");
        Assert.assertEquals(getMatchedColType(new IntegerSchema().format(SchemaTypeUtil.BYTE_FORMAT)), "int");
        Assert.assertEquals(getMatchedColType(new IntegerSchema().type("short")), "int");
        Assert.assertEquals(getMatchedColType(new IntegerSchema().format("short")), "int");
        Assert.assertEquals(getMatchedColType(new IntegerSchema()), "int");
        Assert.assertEquals(getMatchedColType(new IntegerSchema().type("integer")), "int");
        Assert.assertEquals(getMatchedColType(new IntegerSchema().format("integer")), "int");
        Assert.assertEquals(getMatchedColType(new IntegerSchema().format(SchemaTypeUtil.INTEGER32_FORMAT)), "int");
        Assert.assertEquals(getMatchedColType(new IntegerSchema().type("long")), "long");
        Assert.assertEquals(getMatchedColType(new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT)), "long");
        Assert.assertEquals(getMatchedColType(new ObjectSchema().type(SchemaTypeUtil.FLOAT_FORMAT)), "float");
        Assert.assertEquals(getMatchedColType(new NumberSchema().format(SchemaTypeUtil.FLOAT_FORMAT)), "float");
        Assert.assertEquals(getMatchedColType(new ObjectSchema().type(SchemaTypeUtil.DOUBLE_FORMAT)), "double");
        Assert.assertEquals(getMatchedColType(new NumberSchema().format(SchemaTypeUtil.DOUBLE_FORMAT)), "double");
        Assert.assertEquals(getMatchedColType(new ObjectSchema().type(SchemaTypeUtil.FLOAT_FORMAT).format(SchemaTypeUtil.DOUBLE_FORMAT)), "float");
        Assert.assertEquals(getMatchedColType(new ObjectSchema().type(SchemaTypeUtil.DOUBLE_FORMAT).format(SchemaTypeUtil.FLOAT_FORMAT)), "double");
        Assert.assertEquals(getMatchedColType(new ObjectSchema().type("real")), "double");
        Assert.assertEquals(getMatchedColType(new NumberSchema().format("real")), "decimal");
        Assert.assertEquals(getMatchedColType(new NumberSchema().type(SchemaTypeUtil.NUMBER_TYPE)), "decimal");
        Assert.assertEquals(getMatchedColType(new NumberSchema().type("decimal")), "decimal");
        Assert.assertEquals(getMatchedColType(new NumberSchema().type("BigDecimal")), "decimal");
        Assert.assertEquals(getMatchedColType(new ByteArraySchema()), "blob");
        Assert.assertEquals(getMatchedColType(new ArraySchema().items(new IntegerSchema().type(SchemaTypeUtil.BYTE_FORMAT))), "blob");
        Assert.assertEquals(getMatchedColType(new ArraySchema().items(new IntegerSchema().format(SchemaTypeUtil.BYTE_FORMAT))), "blob");
        Assert.assertEquals(getMatchedColType(new ArraySchema()), "blob");
        Assert.assertEquals(getMatchedColType(new ObjectSchema().type("list")), "blob");
        Assert.assertEquals(getMatchedColType(new ObjectSchema().type("set")), "blob");
        Assert.assertEquals(getMatchedColType(new ObjectSchema().type("map")), "blob");
        Assert.assertEquals(getMatchedColType(new ObjectSchema()), "blob");
        Assert.assertEquals(getMatchedColType(new ObjectSchema().type("binary")), "blob");
        Assert.assertEquals(getMatchedColType(new ObjectSchema().type("AnyType")), "blob");
        Assert.assertEquals(getMatchedColType(new BinarySchema()), "blob");
        Assert.assertEquals(getMatchedColType(new FileSchema()), "blob");
        Assert.assertEquals(getMatchedColType(new DateSchema()), "date");
        Assert.assertEquals(getMatchedColType(new DateTimeSchema()), "datetime");
        Assert.assertEquals(getMatchedColType(new UUIDSchema()), "text");
        Assert.assertEquals(getMatchedColType(new ObjectSchema().type("UUID")), "text");
        Assert.assertEquals(getMatchedColType(new StringSchema().format("URI")), "text");
        Assert.assertEquals(getMatchedColType(new ObjectSchema().type("URI")), "text");
        Assert.assertEquals(getMatchedColType(new StringSchema().format("password")), "text");
        Assert.assertEquals(getMatchedColType(new StringSchema().type("password")), "text");
    }

    @Test
    public void testMatchedColKotlinType() {
        // *1 - format specifiers aren't used
        Assert.assertEquals(getMatchedKotlinType(new StringSchema()), "kotlin.String");
        Assert.assertEquals(getMatchedKotlinType(new StringSchema().type("char")), "kotlin.String");
        Assert.assertEquals(getMatchedKotlinType(new StringSchema().format("char")), "kotlin.String");
        Assert.assertEquals(getMatchedKotlinType(new BooleanSchema()), "kotlin.Boolean");
        Assert.assertEquals(getMatchedKotlinType(new IntegerSchema().type(SchemaTypeUtil.BYTE_FORMAT)), "kotlin.Byte");
        Assert.assertEquals(getMatchedKotlinType(new IntegerSchema().format(SchemaTypeUtil.BYTE_FORMAT)), "kotlin.Int"); //*1
        Assert.assertEquals(getMatchedKotlinType(new IntegerSchema().type("short")), "kotlin.Short");
        Assert.assertEquals(getMatchedKotlinType(new IntegerSchema().format("short")), "kotlin.Int"); //*1
        Assert.assertEquals(getMatchedKotlinType(new IntegerSchema()), "kotlin.Int");
        Assert.assertEquals(getMatchedKotlinType(new IntegerSchema().type("integer")), "kotlin.Int");
        Assert.assertEquals(getMatchedKotlinType(new IntegerSchema().format("integer")), "kotlin.Int"); //*1
        Assert.assertEquals(getMatchedKotlinType(new IntegerSchema().format(SchemaTypeUtil.INTEGER32_FORMAT)), "kotlin.Int");
        Assert.assertEquals(getMatchedKotlinType(new IntegerSchema().type("long")), "kotlin.Long");
        Assert.assertEquals(getMatchedKotlinType(new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT)), "kotlin.Long");
        Assert.assertEquals(getMatchedKotlinType(new ObjectSchema().type(SchemaTypeUtil.FLOAT_FORMAT)), "kotlin.Float");
        Assert.assertEquals(getMatchedKotlinType(new NumberSchema().format(SchemaTypeUtil.FLOAT_FORMAT)), "kotlin.Float");
        Assert.assertEquals(getMatchedKotlinType(new ObjectSchema().type(SchemaTypeUtil.DOUBLE_FORMAT)), "kotlin.Double");
        Assert.assertEquals(getMatchedKotlinType(new NumberSchema().format(SchemaTypeUtil.DOUBLE_FORMAT)), "kotlin.Double");
        Assert.assertEquals(getMatchedKotlinType(new ObjectSchema().type(SchemaTypeUtil.FLOAT_FORMAT).format(SchemaTypeUtil.DOUBLE_FORMAT)), "kotlin.Float"); //*1
        Assert.assertEquals(getMatchedKotlinType(new ObjectSchema().type(SchemaTypeUtil.DOUBLE_FORMAT).format(SchemaTypeUtil.FLOAT_FORMAT)), "kotlin.Double"); //*1
        Assert.assertEquals(getMatchedKotlinType(new ObjectSchema().type("real")), "kotlin.Double");
        Assert.assertEquals(getMatchedKotlinType(new NumberSchema().format("real")), "java.math.BigDecimal"); //*1
        Assert.assertEquals(getMatchedKotlinType(new NumberSchema().type(SchemaTypeUtil.NUMBER_TYPE)), "java.math.BigDecimal");
        Assert.assertEquals(getMatchedKotlinType(new NumberSchema().type("decimal")), "java.math.BigDecimal");
        Assert.assertEquals(getMatchedKotlinType(new NumberSchema().type("BigDecimal")), "java.math.BigDecimal");
        Assert.assertEquals(getMatchedKotlinType(new ByteArraySchema()), "kotlin.ByteArray");
        Assert.assertEquals(getMatchedKotlinType(new ArraySchema().items(new IntegerSchema().type(SchemaTypeUtil.BYTE_FORMAT))), "kotlin.Array<kotlin.Byte>");
        Assert.assertEquals(getMatchedKotlinType(new ArraySchema().items(new IntegerSchema().format(SchemaTypeUtil.BYTE_FORMAT))), "kotlin.Array<kotlin.Int>"); //*1
        Assert.assertEquals(getMatchedKotlinType(new ArraySchema()), "kotlin.Array<kotlin.String>");
        Assert.assertEquals(getMatchedKotlinType(new ObjectSchema().type("list")), "kotlin.collections.List");
        Assert.assertEquals(getMatchedKotlinType(new ObjectSchema().type("set")), "kotlin.collections.Set");
        Assert.assertEquals(getMatchedKotlinType(new ObjectSchema().type("map")), "kotlin.collections.Map");
        Assert.assertEquals(getMatchedKotlinType(new ObjectSchema()), "kotlin.Any");
        Assert.assertEquals(getMatchedKotlinType(new ObjectSchema().type("binary")), "kotlin.ByteArray");
        Assert.assertEquals(getMatchedKotlinType(new ObjectSchema().type("AnyType")), "kotlin.Any");
        Assert.assertEquals(getMatchedKotlinType(new BinarySchema()), "java.io.File"); //looks like a bug
        Assert.assertEquals(getMatchedKotlinType(new FileSchema()), "java.io.File");
        Assert.assertEquals(getMatchedKotlinType(new DateSchema()), "java.time.LocalDate");
        Assert.assertEquals(getMatchedKotlinType(new DateTimeSchema()), "java.time.LocalDateTime");
        Assert.assertEquals(getMatchedKotlinType(new UUIDSchema()), "java.util.UUID");
        Assert.assertEquals(getMatchedKotlinType(new ObjectSchema().type("UUID")), "java.util.UUID");
        Assert.assertEquals(getMatchedKotlinType(new StringSchema().format("URI")), "java.net.URI");
        Assert.assertEquals(getMatchedKotlinType(new ObjectSchema().type("URI")), "java.net.URI");
        Assert.assertEquals(getMatchedKotlinType(new StringSchema().format("password")), "kotlin.String");
        Assert.assertEquals(getMatchedKotlinType(new StringSchema().type("password")), "kotlin.String");
    }

    @Test
    public void testNonMatchedRelation() {
        Assert.assertEquals(getMatchedRelation(new StringSchema()), null);
        Assert.assertEquals(getMatchedRelation(new StringSchema().type("char")), null);
        Assert.assertEquals(getMatchedRelation(new StringSchema().format("char")), null);
        Assert.assertEquals(getMatchedRelation(new BooleanSchema()), null);
        Assert.assertEquals(getMatchedRelation(new IntegerSchema().type(SchemaTypeUtil.BYTE_FORMAT)), null);
        Assert.assertEquals(getMatchedRelation(new IntegerSchema().format(SchemaTypeUtil.BYTE_FORMAT)), null);
        Assert.assertEquals(getMatchedRelation(new IntegerSchema().type("short")), null);
        Assert.assertEquals(getMatchedRelation(new IntegerSchema().format("short")), null);
        Assert.assertEquals(getMatchedRelation(new IntegerSchema()), null);
        Assert.assertEquals(getMatchedRelation(new IntegerSchema().type("integer")), null);
        Assert.assertEquals(getMatchedRelation(new IntegerSchema().format("integer")), null);
        Assert.assertEquals(getMatchedRelation(new IntegerSchema().format(SchemaTypeUtil.INTEGER32_FORMAT)), null);
        Assert.assertEquals(getMatchedRelation(new IntegerSchema().type("long")), null);
        Assert.assertEquals(getMatchedRelation(new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT)), null);
        Assert.assertEquals(getMatchedRelation(new ObjectSchema().type(SchemaTypeUtil.FLOAT_FORMAT)), null);
        Assert.assertEquals(getMatchedRelation(new NumberSchema().format(SchemaTypeUtil.FLOAT_FORMAT)), null);
        Assert.assertEquals(getMatchedRelation(new ObjectSchema().type(SchemaTypeUtil.DOUBLE_FORMAT)), null);
        Assert.assertEquals(getMatchedRelation(new NumberSchema().format(SchemaTypeUtil.DOUBLE_FORMAT)), null);
        Assert.assertEquals(getMatchedRelation(new ObjectSchema().type(SchemaTypeUtil.FLOAT_FORMAT).format(SchemaTypeUtil.DOUBLE_FORMAT)), null);
        Assert.assertEquals(getMatchedRelation(new ObjectSchema().type(SchemaTypeUtil.DOUBLE_FORMAT).format(SchemaTypeUtil.FLOAT_FORMAT)), null);
        Assert.assertEquals(getMatchedRelation(new ObjectSchema().type("real")), null);
        Assert.assertEquals(getMatchedRelation(new NumberSchema().format("real")), null);
        Assert.assertEquals(getMatchedRelation(new NumberSchema().type(SchemaTypeUtil.NUMBER_TYPE)), null);
        Assert.assertEquals(getMatchedRelation(new NumberSchema().type("decimal")), null);
        Assert.assertEquals(getMatchedRelation(new NumberSchema().type("BigDecimal")), null);
        Assert.assertEquals(getMatchedRelation(new ByteArraySchema()), null);
        Assert.assertEquals(getMatchedRelation(new ObjectSchema().type("list")), null);
        Assert.assertEquals(getMatchedRelation(new ObjectSchema().type("set")), null);
        Assert.assertEquals(getMatchedRelation(new ObjectSchema().type("map")), null);
        Assert.assertEquals(getMatchedRelation(new ObjectSchema()), null);
        Assert.assertEquals(getMatchedRelation(new ObjectSchema().type("binary")), null);
        Assert.assertEquals(getMatchedRelation(new ObjectSchema().type("AnyType")), null);
        Assert.assertEquals(getMatchedRelation(new BinarySchema()), null);
        Assert.assertEquals(getMatchedRelation(new FileSchema()), null);
        Assert.assertEquals(getMatchedRelation(new DateSchema()), null);
        Assert.assertEquals(getMatchedRelation(new DateTimeSchema()), null);
        Assert.assertEquals(getMatchedRelation(new UUIDSchema()), null);
        Assert.assertEquals(getMatchedRelation(new ObjectSchema().type("UUID")), null);
        Assert.assertEquals(getMatchedRelation(new StringSchema().format("URI")), null);
        Assert.assertEquals(getMatchedRelation(new ObjectSchema().type("URI")), null);
        Assert.assertEquals(getMatchedRelation(new StringSchema().format("password")), null);
        Assert.assertEquals(getMatchedRelation(new StringSchema().type("password")), null);
    }

    @Test
    public void testMatchedRelation() {
        //foreign keys
        Assert.assertEquals(getMatchedRelation(new ObjectSchema().type("Something")), "something");
        Assert.assertEquals(getMatchedColType(new ObjectSchema().type("Something")), "long");
        Assert.assertEquals(getMatchedRelation(new ObjectSchema().type("UserNamespace.UserClass")), "userNamespaceUserClass");
        Assert.assertEquals(getMatchedColType(new ObjectSchema().type("UserNamespace.UserClass")), "long");
        //arrays are special case, we convert them to 1:N relations
        Assert.assertEquals(getMatchedRelation(new ArraySchema()), "key");
        Assert.assertEquals(getMatchedRelation(new ArraySchema().items(new ObjectSchema().type("Something"))), "something");
        Assert.assertEquals(getMatchedRelation(new ArraySchema().items(new ObjectSchema().type("UserNamespace.UserClass"))), "userNamespaceUserClass");
        Assert.assertEquals(getMatchedRelation(new ArraySchema().items(new IntegerSchema().type(SchemaTypeUtil.BYTE_FORMAT))), "key");
        Assert.assertEquals(getMatchedRelation(new ArraySchema().items(new StringSchema())), "key");
        //blob will be the default type, the template shouldn't include those fields
        Assert.assertEquals(getMatchedColType(new ArraySchema()), "blob");
        Assert.assertEquals(getMatchedColType(new ArraySchema().items(new ObjectSchema().type("Something"))), "blob");
        Assert.assertEquals(getMatchedColType(new ArraySchema().items(new ObjectSchema().type("UserNamespace.UserClass"))), "blob");
        Assert.assertEquals(getMatchedColType(new ArraySchema().items(new IntegerSchema().type(SchemaTypeUtil.BYTE_FORMAT))), "blob");
        Assert.assertEquals(getMatchedColType(new ArraySchema().items(new StringSchema())), "blob");
    }

    @Test
    public void testDefinePrimaryKey() {
        final Schema schema = new Schema()
            .description("a sample model")
            .addProperties("key" , new IntegerSchema())
            .addRequiredItem("key");
        CodegenModel cm = getModel(schema, "key", false);
        Assert.assertEquals(cm.vars.size(), 1);
        CodegenProperty prop = cm.vars.get(0);
        Map<String, Object>  propSchema = getColumnDefinition(getExtension(prop));
        Assert.assertEquals(propSchema.get("colPrimaryKey"), true);
        Assert.assertEquals(propSchema.get("colType"), "int");
    }

    @Test
    public void testDontAddSurrogateKey() {
        final Schema schema = new Schema()
            .description("a sample model")
            .addProperties("key" , new IntegerSchema())
            .addRequiredItem("key");
        CodegenModel cm = getModel(schema, "id", false);
        Assert.assertEquals(cm.vars.size(), 1);
        CodegenProperty prop = cm.vars.get(0);
        Map<String, Object>  propSchema = getColumnDefinition(getExtension(prop));
        Assert.assertEquals(propSchema.get("colPrimaryKey"), false);
        Assert.assertEquals(propSchema.get("colType"), "int");
    }

    @Test
    public void testAddSurrogateKey() {
        final Schema schema = new Schema()
            .description("a sample model")
            .addProperties("key", new IntegerSchema());
        CodegenModel cm = getModel(schema, "id", true);
        Assert.assertEquals(cm.vars.size(), 2);
        CodegenProperty prop = cm.vars.get(0);
        Map<String, Object>  propSchema = getColumnDefinition(getExtension(prop));
        Assert.assertEquals(propSchema.get("colNotNull"), true);
        Assert.assertEquals(propSchema.get("colPrimaryKey"), true);
        Assert.assertEquals(propSchema.get("colName"), "id");
        Assert.assertEquals(propSchema.get("colType"), "long"); //by default
        CodegenProperty prop2 = cm.vars.get(1);
        Map<String, Object>  propSchema2 = getColumnDefinition(getExtension(prop2));
        Assert.assertEquals(propSchema2.get("colNotNull"), false);
        Assert.assertEquals(propSchema2.get("colPrimaryKey"), false);
        Assert.assertEquals(propSchema2.get("colName"), "key");
        Assert.assertEquals(propSchema2.get("colType"), "int");

    }

}
