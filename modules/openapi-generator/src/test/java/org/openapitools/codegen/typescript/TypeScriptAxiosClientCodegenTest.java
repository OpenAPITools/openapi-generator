package org.openapitools.codegen.typescript;

import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.languages.TypeScriptAxiosClientCodegen;
import org.testng.annotations.Test;

import static org.testng.Assert.assertEquals;

public class TypeScriptAxiosClientCodegenTest {

    TypeScriptAxiosClientCodegen codegen = new TypeScriptAxiosClientCodegen();

    @Test
    public void testToEnumVarNameOriginalNamingType() {
        codegen.additionalProperties().put(CodegenConstants.ENUM_PROPERTY_NAMING, CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.original.name());
        codegen.processOpts();
        assertEquals(codegen.toEnumVarName("SCIENCE", "string"), "SCIENCE");
        assertEquals(codegen.toEnumVarName("SCIENCE_FICTION", "string"), "SCIENCE_FICTION");
        assertEquals(codegen.toEnumVarName("science", "string"), "science");
        assertEquals(codegen.toEnumVarName("science_fiction", "string"), "science_fiction");
    }

    @Test
    public void testToEnumVarNameCamelCaseNamingType() {
        codegen.additionalProperties().put(CodegenConstants.ENUM_PROPERTY_NAMING, CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.camelCase.name());
        codegen.processOpts();
        assertEquals(codegen.toEnumVarName("SCIENCE", "string"), "science");
        assertEquals(codegen.toEnumVarName("SCIENCE_FICTION", "string"), "scienceFiction");
        assertEquals(codegen.toEnumVarName("science", "string"), "science");
        assertEquals(codegen.toEnumVarName("science_fiction", "string"), "scienceFiction");
    }

    @Test
    public void testToEnumVarNamePascalCaseNamingType() {
        codegen.additionalProperties().put(CodegenConstants.ENUM_PROPERTY_NAMING, CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.PascalCase.name());
        codegen.processOpts();
        assertEquals(codegen.toEnumVarName("SCIENCE", "string"), "Science");
        assertEquals(codegen.toEnumVarName("SCIENCE_FICTION", "string"), "ScienceFiction");
        assertEquals(codegen.toEnumVarName("science", "string"), "Science");
        assertEquals(codegen.toEnumVarName("science_fiction", "string"), "ScienceFiction");
    }

    @Test
    public void testToEnumVarNameSnakeCaseNamingType() {
        codegen.additionalProperties().put(CodegenConstants.ENUM_PROPERTY_NAMING, CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.snake_case.name());
        codegen.processOpts();
        assertEquals(codegen.toEnumVarName("SCIENCE", "string"), "science");
        assertEquals(codegen.toEnumVarName("SCIENCE_FICTION", "string"), "science_fiction");
        assertEquals(codegen.toEnumVarName("science", "string"), "science");
        assertEquals(codegen.toEnumVarName("science_fiction", "string"), "science_fiction");
    }

    @Test
    public void testToEnumVarNameUpperCaseNamingType() {
        codegen.additionalProperties().put(CodegenConstants.ENUM_PROPERTY_NAMING, CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.UPPERCASE.name());
        codegen.processOpts();
        assertEquals(codegen.toEnumVarName("SCIENCE", "string"), "SCIENCE");
        assertEquals(codegen.toEnumVarName("SCIENCE_FICTION", "string"), "SCIENCE_FICTION");
        assertEquals(codegen.toEnumVarName("science", "string"), "SCIENCE");
        assertEquals(codegen.toEnumVarName("science_fiction", "string"), "SCIENCE_FICTION");
    }

}
