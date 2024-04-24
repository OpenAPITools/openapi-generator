package org.openapitools.codegen.scala;

import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.MapSchema;
import io.swagger.v3.oas.models.media.ObjectSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;

import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.languages.AbstractScalaCodegen;
import org.openapitools.codegen.utils.ModelUtils;
import org.testng.Assert;
import org.testng.annotations.Test;

public class AbstractScalaCodegenTest {

    private final AbstractScalaCodegen fakeScalaCodegen = new AbstractScalaCodegenTest.P_AbstractScalaCodegen();

    private static class P_AbstractScalaCodegen extends AbstractScalaCodegen {
    }

    @Test
    public void convertVarNameCamelCase() {
        // with default camelCase
        Assertions.assertEquals(CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.camelCase.name(), fakeScalaCodegen.getModelPropertyNaming());
        Assertions.assertEquals(fakeScalaCodegen.toVarName("name"), "name");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("user-name"), "userName");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("user_name"), "userName");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("user|name"), "userName");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("uSername"), "uSername");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("USERNAME"), "USERNAME");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("USER123NAME"), "USER123NAME");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("1"), "`1`");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("1a"), "`1a`");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("1A"), "`1A`");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("1AAAA"), "`1AAAA`");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("1AAaa"), "`1aAaa`");
    }

    @Test
    public void convertVarNamePascalCase() {
        fakeScalaCodegen.setModelPropertyNaming(CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.PascalCase.name());
        Assertions.assertEquals(fakeScalaCodegen.toVarName("name"), "Name");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("user-name"), "UserName");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("user_name"), "UserName");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("user|name"), "UserName");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("uSername"), "USername");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("USERNAME"), "USERNAME");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("USER123NAME"), "USER123NAME");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("1"), "`1`");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("1a"), "`1a`");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("1A"), "`1A`");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("1AAAA"), "`1AAAA`");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("1AAaa"), "`1AAaa`");
    }

    @Test
    public void convertVarNameSnakeCase() {
        fakeScalaCodegen.setModelPropertyNaming(CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.snake_case.name());
        Assertions.assertEquals(fakeScalaCodegen.toVarName("name"), "name");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("user-name"), "user_name");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("user_name"), "user_name");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("user|name"), "user_name");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("uSername"), "u_sername");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("USERNAME"), "USERNAME");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("USER123NAME"), "USER123NAME");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("1"), "`1`");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("1a"), "`1a`");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("1A"), "`1A`");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("1AAAA"), "`1AAAA`");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("1AAaa"), "`1_a_aaa`");
    }

    @Test
    public void convertVarNameOriginalCase() {
        fakeScalaCodegen.setModelPropertyNaming(CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.original.name());
        Assertions.assertEquals(fakeScalaCodegen.toVarName("name"), "name");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("Name"), "Name");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("name-sanitized-to-underscore"), "name_sanitized_to_underscore");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("user_name"), "user_name");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("user|name"), "user_name");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("uSername"), "uSername");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("USERNAME"), "USERNAME");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("USER123NAME"), "USER123NAME");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("1"), "`1`");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("1a"), "`1a`");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("1A"), "`1A`");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("1AAAA"), "`1AAAA`");
        Assertions.assertEquals(fakeScalaCodegen.toVarName("1AAaa"), "`1AAaa`");
    }

    @Test
    public void checkScalaTypeImportMapping() {
        Assertions.assertEquals(fakeScalaCodegen.importMapping().get("Seq"),
                "scala.collection.immutable.Seq", "Seq is immutable collection");
        Assertions.assertEquals(fakeScalaCodegen.importMapping().get("Set"),
                "scala.collection.immutable.Set", "Set is immutable collection");
        Assertions.assertFalse(fakeScalaCodegen.importMapping().containsKey("List"),
                "List is a Scala type and must not be imported");
        Assertions.assertFalse(fakeScalaCodegen.importMapping().containsKey("BigDecimal"),
                "BigDecimal is a Scala type and must not be imported");
        Assertions.assertFalse(fakeScalaCodegen.importMapping().containsKey("BigInt"),
                "BigInt is a Scala type and must not be imported");
    }

    @Test
    void checkTypeDeclarationWithByteString() {
        Schema<?> byteArraySchema = new ObjectSchema();
        byteArraySchema.setType("string");
        byteArraySchema.setFormat("byte");
        byteArraySchema.setDescription("Schema with byte string");

        Assertions.assertEquals(fakeScalaCodegen.getTypeDeclaration(byteArraySchema), "Array[Byte]",
                "OpenApi File type represented as byte string should be represented as Array[Byte] scala type");
    }

    @Test
    void checkTypeDeclarationWithStringToArrayModelMapping() {
        // Create an alias to an array schema
        final Schema<?> nestedArraySchema = new ArraySchema().items(new StringSchema());
        // Create a map schema with additionalProperties type set to array alias
        final Schema<?> mapSchema = new MapSchema().additionalProperties(new Schema().$ref("#/components/schemas/NestedArray"));
        fakeScalaCodegen.setOpenAPI(new OpenAPI().components(new Components().addSchemas("NestedArray", nestedArraySchema)));

        ModelUtils.setGenerateAliasAsModel(false);
        Assertions.assertEquals(fakeScalaCodegen.getTypeDeclaration(mapSchema), "Map[String, List[String]]");

        ModelUtils.setGenerateAliasAsModel(true);
        Assertions.assertEquals(fakeScalaCodegen.getTypeDeclaration(mapSchema), "Map[String, NestedArray]");
    }
}
