package org.openapitools.codegen.kotlin;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.ComposedSchema;
import io.swagger.v3.oas.models.media.ObjectSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.AbstractKotlinCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.util.HashSet;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import static org.openapitools.codegen.CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.*;
import static org.testng.Assert.*;

public class AbstractKotlinCodegenTest {

    private final AbstractKotlinCodegen codegen = new P_AbstractKotlinCodegen();

    @Test
    public void camlCaseEnumConverter() {
        codegen.setEnumPropertyNaming(camelCase.name());
        assertEquals(codegen.toEnumVarName("long Name", null), "longName");
        assertEquals(codegen.toEnumVarName("1long Name", null), "_1longName");
        assertEquals(codegen.toEnumVarName("not1long Name", null), "not1longName");
    }

    @Test
    public void uppercasEnumConverter() {
        codegen.setEnumPropertyNaming(UPPERCASE.name());
        assertEquals(codegen.toEnumVarName("long Name", null), "LONG_NAME");
        assertEquals(codegen.toEnumVarName("1long Name", null), "_1LONG_NAME");
        assertEquals(codegen.toEnumVarName("not1long Name", null), "NOT1LONG_NAME");
    }
    @Test
    public void snake_caseEnumConverter() {
        codegen.setEnumPropertyNaming(snake_case.name());
        assertEquals(codegen.toEnumVarName("long Name", null), "long_name");
        assertEquals(codegen.toEnumVarName("1long Name", null), "_1long_name");
        assertEquals(codegen.toEnumVarName("not1long Name", null), "not1long_name");
    }

    @Test
    public void originalEnumConverter() {
        codegen.setEnumPropertyNaming(original.name());
        assertEquals(codegen.toEnumVarName("long Name", null), "long_Name");
        assertEquals(codegen.toEnumVarName("1long Name", null), "_1long_Name");
        assertEquals(codegen.toEnumVarName("not1long Name", null), "not1long_Name");
    }
    @Test
    public void pascalCaseEnumConverter() {
        codegen.setEnumPropertyNaming(PascalCase.name());
        assertEquals(codegen.toEnumVarName("long Name", null), "LongName");
        assertEquals(codegen.toEnumVarName("1long Name", null), "_1longName");
        assertEquals(codegen.toEnumVarName("not1long Name", null), "Not1longName");
    }

    @Test
    public void toEnumValue() {
        assertEquals(codegen.toEnumValue("1", "kotlin.Int"), "1");
        assertEquals(codegen.toEnumValue("1", "kotlin.Double"), "1.0");
        assertEquals(codegen.toEnumValue("1.3", "kotlin.Double"), "1.3");
        assertEquals(codegen.toEnumValue("1337", "kotlin.Long"), "1337");
        assertEquals(codegen.toEnumValue("5", "kotlin.Float"), "5f");
        assertEquals(codegen.toEnumValue("1.0", "kotlin.Float"), "1.0f");
        assertEquals(codegen.toEnumValue("data", "Something"), "\"data\"");
    }

    private class P_AbstractKotlinCodegen extends AbstractKotlinCodegen {
        @Override
        public CodegenType getTag() {
            return null;
        }

        @Override
        public String getName() {
            return null;
        }

        @Override
        public String getHelp() {
            return null;
        }
    }

    @Test
    public void isDataTypeString() {
        assertFalse(codegen.isDataTypeString("kotlin.Int"));
        assertTrue(codegen.isDataTypeString("kotlin.String"));
        assertTrue(codegen.isDataTypeString("String"));
    }

    @Test
    public void toModelNameShouldUseProvidedMapping() {
        codegen.importMapping().put("json_myclass", "com.test.MyClass");
        assertEquals("com.test.MyClass", codegen.toModelName("json_myclass"));
    }

    @Test
    public void convertModelNameTitleCase() {
        assertEquals(codegen.toModelName("name"), "Name");
    }

    @Test
    public void convertModelName() {
        assertEquals(codegen.toModelName("$"), "Dollar");
        assertEquals(codegen.toModelName("$$"), "DollarDollar");
        assertEquals(codegen.toModelName("Pony?"), "PonyQuestionMark");
        assertEquals(codegen.toModelName("$name"), "DollarName");
        assertEquals(codegen.toModelName("nam#e"), "NamHashE");
        assertEquals(codegen.toModelName("$another-fake?"), "DollarAnotherMinusFakeQuestionMark");
        assertEquals(codegen.toModelName("Pony>=>="), "PonyGreaterThanEqualGreaterThanEqual");
    }

    @Test
    public void convertVarName() throws Exception {
        assertEquals(codegen.toVarName("name"), "name");
        assertEquals(codegen.toVarName("$name"), "dollarName");
        assertEquals(codegen.toVarName("nam$$e"), "namDollarDollarE");
        assertEquals(codegen.toVarName("user-name"), "userMinusName");
        assertEquals(codegen.toVarName("user_name"), "userName");
        assertEquals(codegen.toVarName("user|name"), "userPipeName");
        assertEquals(codegen.toVarName("Pony?"), "ponyQuestionMark");
        assertEquals(codegen.toVarName("nam#e"), "namHashE");
        assertEquals(codegen.toVarName("Pony>=>="), "ponyGreaterThanEqualGreaterThanEqual");
        assertEquals(codegen.toVarName("uSername"), "uSername");
        assertEquals(codegen.toVarName("USERname"), "usERname");
        assertEquals(codegen.toVarName("USERNAME"), "USERNAME");
        assertEquals(codegen.toVarName("USER123NAME"), "USER123NAME");
    }

    @Test
    public void convertApiNameWithEmptySuffix() {
        assertEquals(codegen.toApiName("Fake"), "FakeApi");
        assertEquals(codegen.toApiName(""), "DefaultApi");
    }

    @Test
    public void convertApiNameWithSuffix() {
        codegen.setApiSuffix("Test");
        assertEquals(codegen.toApiName("Fake"), "FakeTest");
        assertEquals(codegen.toApiName(""), "DefaultApi");
    }

    @Test
    public void apIFileFolder() {
        codegen.setOutputDir("/User/open/api/tools");
        codegen.setSourceFolder("src/folder");
        codegen.setApiPackage("org.openapitools.codegen.api");
        Assert.assertEquals(codegen.apiFileFolder(), "/User/open/api/tools/src/folder/org/openapitools/codegen/api".replace('/', File.separatorChar));
    }

    @Test
    public void apiTestFileFolder() {
        codegen.setOutputDir("/User/open/api/tools");
        codegen.setTestFolder("test/folder");
        codegen.setApiPackage("org.openapitools.codegen.api");
        Assert.assertEquals(codegen.apiTestFileFolder(), "/User/open/api/tools/test/folder/org/openapitools/codegen/api".replace('/', File.separatorChar));
    }

    @Test
    public void processOptsBooleanTrueFromString() {
        codegen.additionalProperties().put(CodegenConstants.SERIALIZABLE_MODEL, "true");
        codegen.processOpts();
        Assert.assertTrue((boolean) codegen.additionalProperties().get(CodegenConstants.SERIALIZABLE_MODEL));
    }

    @Test
    public void processOptsBooleanTrueFromBoolean() {
        codegen.additionalProperties().put(CodegenConstants.SERIALIZABLE_MODEL, true);
        codegen.processOpts();
        Assert.assertTrue((boolean) codegen.additionalProperties().get(CodegenConstants.SERIALIZABLE_MODEL));
    }

    @Test
    public void processOptsBooleanFalseFromString() {
        codegen.additionalProperties().put(CodegenConstants.SERIALIZABLE_MODEL, "false");
        codegen.processOpts();
        Assert.assertFalse((boolean) codegen.additionalProperties().get(CodegenConstants.SERIALIZABLE_MODEL));
    }

    @Test
    public void processOptsBooleanFalseFromBoolean() {
        codegen.additionalProperties().put(CodegenConstants.SERIALIZABLE_MODEL, false);
        codegen.processOpts();
        Assert.assertFalse((boolean) codegen.additionalProperties().get(CodegenConstants.SERIALIZABLE_MODEL));
    }

    @Test
    public void processOptsBooleanFalseFromGarbage() {
        codegen.additionalProperties().put(CodegenConstants.SERIALIZABLE_MODEL, "blibb");
        codegen.processOpts();
        Assert.assertFalse((boolean) codegen.additionalProperties().get(CodegenConstants.SERIALIZABLE_MODEL));
    }

    @Test
    public void processOptsBooleanFalseFromNumeric() {
        codegen.additionalProperties().put(CodegenConstants.SERIALIZABLE_MODEL, 42L);
        codegen.processOpts();
        Assert.assertFalse((boolean) codegen.additionalProperties().get(CodegenConstants.SERIALIZABLE_MODEL));
    }

    @Test
    public void handleInheritance() {
        Schema parent = new ObjectSchema()
                .addProperties("a", new StringSchema())
                .addProperties("b", new StringSchema())
                .addRequiredItem("a")
                .name("Parent");
        Schema child = new ComposedSchema()
                .addAllOfItem(new Schema().$ref("Parent"))
                .addAllOfItem(new ObjectSchema()
                        .addProperties("c", new StringSchema())
                        .addProperties("d", new StringSchema())
                        .addRequiredItem("c"))
                .name("Child");
        OpenAPI openAPI = TestUtils.createOpenAPI();
        openAPI.getComponents().addSchemas(parent.getName(), parent);
        openAPI.getComponents().addSchemas(child.getName(), child);

        final DefaultCodegen codegen = new P_AbstractKotlinCodegen();
        codegen.setOpenAPI(openAPI);

        final CodegenModel pm = codegen
                .fromModel("Child", child);
        Map<String, CodegenProperty> allVarsMap = pm.allVars.stream()
                .collect(Collectors.toMap(CodegenProperty::getBaseName, Function.identity()));
        for (CodegenProperty p : pm.requiredVars) {
            Assert.assertEquals(allVarsMap.get(p.baseName).isInherited, p.isInherited);
        }
        Assert.assertEqualsNoOrder(
            pm.requiredVars.stream().map(CodegenProperty::getBaseName).toArray(),
            new String[] {"a", "c"}
        );
        for (CodegenProperty p : pm.optionalVars) {
            Assert.assertEquals(allVarsMap.get(p.baseName).isInherited, p.isInherited);
        }
        Assert.assertEqualsNoOrder(
            pm.optionalVars.stream().map(CodegenProperty::getBaseName).toArray(),
            new String[] {"b", "d"}
        );
    }
}
