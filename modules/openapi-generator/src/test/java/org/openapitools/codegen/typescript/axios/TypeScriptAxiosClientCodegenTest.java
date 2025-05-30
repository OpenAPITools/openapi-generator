package org.openapitools.codegen.typescript.axios;

import static org.assertj.core.api.Assertions.assertThat;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

import java.util.List;
import java.util.Map;

import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenSecurity;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.TypeScriptAxiosClientCodegen;
import org.openapitools.codegen.typescript.TypeScriptGroups;
import org.testng.annotations.Test;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.security.SecurityScheme;

@Test(groups = {TypeScriptGroups.TYPESCRIPT, TypeScriptGroups.TYPESCRIPT_AXIOS})
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
        assertEquals(codegen.toEnumVarName("scienceFiction", "string"), "scienceFiction");
        assertEquals(codegen.toEnumVarName("ScienceFiction", "string"), "ScienceFiction");
        assertEquals(codegen.toEnumVarName("A", "string"), "A");
        assertEquals(codegen.toEnumVarName("b", "string"), "b");
    }

    @Test
    public void testToEnumVarNameCamelCaseNamingType() {
        codegen.additionalProperties().put(CodegenConstants.ENUM_PROPERTY_NAMING, CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.camelCase.name());
        codegen.processOpts();
        assertEquals(codegen.toEnumVarName("SCIENCE", "string"), "science");
        assertEquals(codegen.toEnumVarName("SCIENCE_FICTION", "string"), "scienceFiction");
        assertEquals(codegen.toEnumVarName("science", "string"), "science");
        assertEquals(codegen.toEnumVarName("science_fiction", "string"), "scienceFiction");
        assertEquals(codegen.toEnumVarName("scienceFiction", "string"), "scienceFiction");
        assertEquals(codegen.toEnumVarName("ScienceFiction", "string"), "scienceFiction");
    }

    @Test
    public void testToEnumVarNamePascalCaseNamingType() {
        codegen.additionalProperties().put(CodegenConstants.ENUM_PROPERTY_NAMING, CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.PascalCase.name());
        codegen.processOpts();
        assertEquals(codegen.toEnumVarName("SCIENCE", "string"), "Science");
        assertEquals(codegen.toEnumVarName("SCIENCE_FICTION", "string"), "ScienceFiction");
        assertEquals(codegen.toEnumVarName("science", "string"), "Science");
        assertEquals(codegen.toEnumVarName("science_fiction", "string"), "ScienceFiction");
        assertEquals(codegen.toEnumVarName("scienceFiction", "string"), "ScienceFiction");
        assertEquals(codegen.toEnumVarName("ScienceFiction", "string"), "ScienceFiction");
        assertEquals(codegen.toEnumVarName("A", "string"), "A");
        assertEquals(codegen.toEnumVarName("b", "string"), "B");
    }

    @Test
    public void testToEnumVarNameSnakeCaseNamingType() {
        codegen.additionalProperties().put(CodegenConstants.ENUM_PROPERTY_NAMING, CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.snake_case.name());
        codegen.processOpts();
        assertEquals(codegen.toEnumVarName("SCIENCE", "string"), "science");
        assertEquals(codegen.toEnumVarName("SCIENCE_FICTION", "string"), "science_fiction");
        assertEquals(codegen.toEnumVarName("science", "string"), "science");
        assertEquals(codegen.toEnumVarName("science_fiction", "string"), "science_fiction");
        assertEquals(codegen.toEnumVarName("scienceFiction", "string"), "science_fiction");
        assertEquals(codegen.toEnumVarName("ScienceFiction", "string"), "science_fiction");
        assertEquals(codegen.toEnumVarName("A", "string"), "a");
        assertEquals(codegen.toEnumVarName("b", "string"), "b");
    }

    @Test
    public void testToEnumVarNameUpperCaseNamingType() {
        codegen.additionalProperties().put(CodegenConstants.ENUM_PROPERTY_NAMING, CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.UPPERCASE.name());
        codegen.processOpts();
        assertEquals(codegen.toEnumVarName("SCIENCE", "string"), "SCIENCE");
        assertEquals(codegen.toEnumVarName("SCIENCE_FICTION", "string"), "SCIENCE_FICTION");
        assertEquals(codegen.toEnumVarName("science", "string"), "SCIENCE");
        assertEquals(codegen.toEnumVarName("science_fiction", "string"), "SCIENCE_FICTION");
        assertEquals(codegen.toEnumVarName("scienceFiction", "string"), "SCIENCE_FICTION");
        assertEquals(codegen.toEnumVarName("ScienceFiction", "string"), "SCIENCE_FICTION");
        assertEquals(codegen.toEnumVarName("A", "string"), "A");
        assertEquals(codegen.toEnumVarName("b", "string"), "B");
    }

    @Test
    public void containsESMTSConfigFileInCaseOfES6AndNPM() {
        TypeScriptAxiosClientCodegen codegen = new TypeScriptAxiosClientCodegen();

        codegen.additionalProperties().put("npmName", "@openapi/typescript-axios-petstore");
        codegen.additionalProperties().put("snapshot", false);
        codegen.additionalProperties().put("npmVersion", "1.0.0-SNAPSHOT");
        codegen.additionalProperties().put("supportsES6", true);

        codegen.processOpts();

        assertThat(codegen.supportingFiles()).contains(new SupportingFile("tsconfig.mustache", "", "tsconfig.json"));
        assertThat(codegen.supportingFiles()).contains(new SupportingFile("tsconfig.esm.mustache", "", "tsconfig.esm.json"));
    }

    @Test
    public void doesNotContainESMTSConfigFileInCaseOfES5AndNPM() {
        TypeScriptAxiosClientCodegen codegen = new TypeScriptAxiosClientCodegen();

        codegen.additionalProperties().put("npmName", "@openapi/typescript-axios-petstore");
        codegen.additionalProperties().put("snapshot", false);
        codegen.additionalProperties().put("npmVersion", "1.0.0-SNAPSHOT");
        codegen.additionalProperties().put("supportsES6", false);

        codegen.processOpts();

        assertThat(codegen.supportingFiles()).contains(new SupportingFile("tsconfig.mustache", "", "tsconfig.json"));
        assertThat(codegen.supportingFiles()).doesNotContain(new SupportingFile("tsconfig.esm.mustache", "", "tsconfig.esm.json"));
    }

    @Test
    public void testAppliesDefaultAxiosVersion() {
        TypeScriptAxiosClientCodegen codegen = new TypeScriptAxiosClientCodegen();

        codegen.processOpts();

        assertEquals(codegen.additionalProperties().get("axiosVersion"), TypeScriptAxiosClientCodegen.DEFAULT_AXIOS_VERSION);
    }

    @Test
    public void testAppliesCustomAxiosVersion() {
        TypeScriptAxiosClientCodegen codegen = new TypeScriptAxiosClientCodegen();
        codegen.additionalProperties().put("axiosVersion", "^1.2.3");

        codegen.processOpts();

        assertEquals(codegen.additionalProperties().get("axiosVersion"), "^1.2.3");
    }

    @Test
    public void testDetectsAwsIamAuthenticationWithExtension() {
        OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/typescript-axios/with-aws-iam.yaml");
        TypeScriptAxiosClientCodegen codegen = new TypeScriptAxiosClientCodegen();

        // Call preprocessOpenAPI which will detect AWS and set the property
        codegen.preprocessOpenAPI(openAPI);

        // Should detect AWS V4 signature due to x-amazon-apigateway-authtype extension
        assertTrue((Boolean) codegen.additionalProperties().get(CodegenConstants.WITH_AWSV4_SIGNATURE_COMMENT));
    }

    @Test
    public void testDetectsAwsIamAuthenticationWithNaming() {
        OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/typescript-axios/with-aws-iam.yaml");
        TypeScriptAxiosClientCodegen codegen = new TypeScriptAxiosClientCodegen();

        // Call preprocessOpenAPI which will detect AWS and set the property
        codegen.preprocessOpenAPI(openAPI);

        // Should detect AWS V4 signature due to scheme name patterns and AWS API Gateway URL
        assertTrue((Boolean) codegen.additionalProperties().get(CodegenConstants.WITH_AWSV4_SIGNATURE_COMMENT));
    }

    @Test
    public void testDoesNotDetectAwsIamInRegularPetstore() {
        OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/petstore.yaml");
        TypeScriptAxiosClientCodegen codegen = new TypeScriptAxiosClientCodegen();

        // Call preprocessOpenAPI
        codegen.preprocessOpenAPI(openAPI);

        // Should NOT detect AWS V4 signature in regular petstore
        assertFalse(codegen.additionalProperties().containsKey(CodegenConstants.WITH_AWSV4_SIGNATURE_COMMENT) &&
                (Boolean) codegen.additionalProperties().get(CodegenConstants.WITH_AWSV4_SIGNATURE_COMMENT));
    }

    @Test
    public void testDoesNotDetectAwsIamInSwagger2Petstore() {
        OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/petstore.yaml");
        TypeScriptAxiosClientCodegen codegen = new TypeScriptAxiosClientCodegen();

        // Call preprocessOpenAPI
        codegen.preprocessOpenAPI(openAPI);

        // Should NOT detect AWS V4 signature in Swagger 2.0 petstore either
        assertFalse(codegen.additionalProperties().containsKey(CodegenConstants.WITH_AWSV4_SIGNATURE_COMMENT) &&
                (Boolean) codegen.additionalProperties().get(CodegenConstants.WITH_AWSV4_SIGNATURE_COMMENT));
    }

    @Test
    public void testFromSecuritySetsAwsV4Flag() {
        OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/typescript-axios/with-aws-iam.yaml");
        TypeScriptAxiosClientCodegen codegen = new TypeScriptAxiosClientCodegen();

        // Call preprocessOpenAPI first to set up the openAPI object
        codegen.preprocessOpenAPI(openAPI);

        // Test fromSecurity method directly
        Map<String, SecurityScheme> securitySchemes =
                openAPI.getComponents().getSecuritySchemes();
        List<CodegenSecurity> securities = codegen.fromSecurity(securitySchemes);

        // Should have AWS V4 signature flagged on individual security schemes
        boolean hasAwsV4 = securities.stream().anyMatch(s -> Boolean.TRUE.equals(s.isAWSV4Signature));
        assertTrue(hasAwsV4, "fromSecurity should set isAWSV4Signature=true for AWS schemes");
    }
}
