package org.openapitools.codegen.scala;

import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.languages.AbstractScalaCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

public class AbstractScalaCodegenTest {

    private final AbstractScalaCodegen fakeScalaCodegen = new AbstractScalaCodegenTest.P_AbstractScalaCodegen();

    private static class P_AbstractScalaCodegen extends AbstractScalaCodegen {
    }

    @Test
    public void convertVarNameCamelCase() {
        // with default camelCase
        Assert.assertEquals(CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.camelCase.name(), fakeScalaCodegen.getModelPropertyNaming());
        Assert.assertEquals(fakeScalaCodegen.toVarName("name"), "name");
        Assert.assertEquals(fakeScalaCodegen.toVarName("user-name"), "userName");
        Assert.assertEquals(fakeScalaCodegen.toVarName("user_name"), "userName");
        Assert.assertEquals(fakeScalaCodegen.toVarName("user|name"), "userName");
        Assert.assertEquals(fakeScalaCodegen.toVarName("uSername"), "uSername");
        Assert.assertEquals(fakeScalaCodegen.toVarName("USERNAME"), "USERNAME");
        Assert.assertEquals(fakeScalaCodegen.toVarName("USER123NAME"), "USER123NAME");
        Assert.assertEquals(fakeScalaCodegen.toVarName("1"), "`1`");
        Assert.assertEquals(fakeScalaCodegen.toVarName("1a"), "`1a`");
        Assert.assertEquals(fakeScalaCodegen.toVarName("1A"), "`1A`");
        Assert.assertEquals(fakeScalaCodegen.toVarName("1AAAA"), "`1AAAA`");
        Assert.assertEquals(fakeScalaCodegen.toVarName("1AAaa"), "`1aAaa`");
    }

    @Test
    public void convertVarNamePascalCase() {
        fakeScalaCodegen.setModelPropertyNaming(CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.PascalCase.name());
        Assert.assertEquals(fakeScalaCodegen.toVarName("name"), "Name");
        Assert.assertEquals(fakeScalaCodegen.toVarName("user-name"), "UserName");
        Assert.assertEquals(fakeScalaCodegen.toVarName("user_name"), "UserName");
        Assert.assertEquals(fakeScalaCodegen.toVarName("user|name"), "UserName");
        Assert.assertEquals(fakeScalaCodegen.toVarName("uSername"), "USername");
        Assert.assertEquals(fakeScalaCodegen.toVarName("USERNAME"), "USERNAME");
        Assert.assertEquals(fakeScalaCodegen.toVarName("USER123NAME"), "USER123NAME");
        Assert.assertEquals(fakeScalaCodegen.toVarName("1"), "`1`");
        Assert.assertEquals(fakeScalaCodegen.toVarName("1a"), "`1a`");
        Assert.assertEquals(fakeScalaCodegen.toVarName("1A"), "`1A`");
        Assert.assertEquals(fakeScalaCodegen.toVarName("1AAAA"), "`1AAAA`");
        Assert.assertEquals(fakeScalaCodegen.toVarName("1AAaa"), "`1AAaa`");
    }

    @Test
    public void convertVarNameSnakeCase() {
        fakeScalaCodegen.setModelPropertyNaming(CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.snake_case.name());
        Assert.assertEquals(fakeScalaCodegen.toVarName("name"), "name");
        Assert.assertEquals(fakeScalaCodegen.toVarName("user-name"), "user_name");
        Assert.assertEquals(fakeScalaCodegen.toVarName("user_name"), "user_name");
        Assert.assertEquals(fakeScalaCodegen.toVarName("user|name"), "user_name");
        Assert.assertEquals(fakeScalaCodegen.toVarName("uSername"), "u_sername");
        Assert.assertEquals(fakeScalaCodegen.toVarName("USERNAME"), "USERNAME");
        Assert.assertEquals(fakeScalaCodegen.toVarName("USER123NAME"), "USER123NAME");
        Assert.assertEquals(fakeScalaCodegen.toVarName("1"), "`1`");
        Assert.assertEquals(fakeScalaCodegen.toVarName("1a"), "`1a`");
        Assert.assertEquals(fakeScalaCodegen.toVarName("1A"), "`1A`");
        Assert.assertEquals(fakeScalaCodegen.toVarName("1AAAA"), "`1AAAA`");
        Assert.assertEquals(fakeScalaCodegen.toVarName("1AAaa"), "`1_a_aaa`");
    }

    @Test
    public void convertVarNameOriginalCase() {
        fakeScalaCodegen.setModelPropertyNaming(CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.original.name());
        Assert.assertEquals(fakeScalaCodegen.toVarName("name"), "name");
        Assert.assertEquals(fakeScalaCodegen.toVarName("Name"), "Name");
        Assert.assertEquals(fakeScalaCodegen.toVarName("name-sanitized-to-underscore"), "name_sanitized_to_underscore");
        Assert.assertEquals(fakeScalaCodegen.toVarName("user_name"), "user_name");
        Assert.assertEquals(fakeScalaCodegen.toVarName("user|name"), "user_name");
        Assert.assertEquals(fakeScalaCodegen.toVarName("uSername"), "uSername");
        Assert.assertEquals(fakeScalaCodegen.toVarName("USERNAME"), "USERNAME");
        Assert.assertEquals(fakeScalaCodegen.toVarName("USER123NAME"), "USER123NAME");
        Assert.assertEquals(fakeScalaCodegen.toVarName("1"), "`1`");
        Assert.assertEquals(fakeScalaCodegen.toVarName("1a"), "`1a`");
        Assert.assertEquals(fakeScalaCodegen.toVarName("1A"), "`1A`");
        Assert.assertEquals(fakeScalaCodegen.toVarName("1AAAA"), "`1AAAA`");
        Assert.assertEquals(fakeScalaCodegen.toVarName("1AAaa"), "`1AAaa`");
    }

    @Test
    public void checkScalaTypeImportMapping() {
        Assert.assertEquals(fakeScalaCodegen.importMapping().get("Seq"),
                "scala.collection.immutable.Seq", "Seq is immutable collection");
        Assert.assertEquals(fakeScalaCodegen.importMapping().get("Set"),
                "scala.collection.immutable.Set", "Set is immutable collection");
        Assert.assertFalse(fakeScalaCodegen.importMapping().containsKey("List"),
                "List is a Scala type and must not be imported");
        Assert.assertFalse(fakeScalaCodegen.importMapping().containsKey("BigDecimal"),
                "BigDecimal is a Scala type and must not be imported");
        Assert.assertFalse(fakeScalaCodegen.importMapping().containsKey("BigInt"),
                "BigInt is a Scala type and must not be imported");
    }
}
