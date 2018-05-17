package org.openapitools.codegen.kotlin;

import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.languages.AbstractKotlinCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import static org.openapitools.codegen.CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.*;

public class AbstractKotlinCodegenTest {

    private final AbstractKotlinCodegen codegen = new P_AbstractKotlinCodegen();

    @Test
    public void camlCaseEnumConverter() {
        codegen.setEnumPropertyNaming(camelCase.name());
        Assert.assertEquals(codegen.toEnumVarName("long Name", null), "longName");
        Assert.assertEquals(codegen.toEnumVarName("1long Name", null), "_1longName");
        Assert.assertEquals(codegen.toEnumVarName("not1long Name", null), "not1longName");
    }

    @Test
    public void uppercasEnumConverter() {
        codegen.setEnumPropertyNaming(UPPERCASE.name());
        Assert.assertEquals(codegen.toEnumVarName("long Name", null), "LONG_NAME");
        Assert.assertEquals(codegen.toEnumVarName("1long Name", null), "_1LONG_NAME");
        Assert.assertEquals(codegen.toEnumVarName("not1long Name", null), "NOT1LONG_NAME");
    }
    @Test
    public void snake_caseEnumConverter() {
        codegen.setEnumPropertyNaming(snake_case.name());
        Assert.assertEquals(codegen.toEnumVarName("long Name", null), "long_name");
        Assert.assertEquals(codegen.toEnumVarName("1long Name", null), "_1long_name");
        Assert.assertEquals(codegen.toEnumVarName("not1long Name", null), "not1long_name");
    }

    @Test
    public void originalEnumConverter() {
        codegen.setEnumPropertyNaming(original.name());
        Assert.assertEquals(codegen.toEnumVarName("long Name", null), "long_Name");
        Assert.assertEquals(codegen.toEnumVarName("1long Name", null), "_1long_Name");
        Assert.assertEquals(codegen.toEnumVarName("not1long Name", null), "not1long_Name");
    }
    @Test
    public void pascalCaseEnumConverter() {
        codegen.setEnumPropertyNaming(PascalCase.name());
        Assert.assertEquals(codegen.toEnumVarName("long Name", null), "LongName");
        Assert.assertEquals(codegen.toEnumVarName("1long Name", null), "_1longName");
        Assert.assertEquals(codegen.toEnumVarName("not1long Name", null), "Not1longName");
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
}