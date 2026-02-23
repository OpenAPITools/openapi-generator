package org.openapitools.codegen.typescript.axios;

import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.TypeScriptAxiosClientCodegen;
import org.openapitools.codegen.typescript.TypeScriptGroups;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

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
    public void testDefaultUseErasableSyntaxIsFalse() {
        TypeScriptAxiosClientCodegen codegen = new TypeScriptAxiosClientCodegen();

        codegen.processOpts();

        // useErasableSyntax should not be set when not specified (defaults to false behavior)
        assertFalse(codegen.additionalProperties().containsKey("useErasableSyntax"));
    }

    @Test
    public void testUseErasableSyntaxSetToTrue() {
        TypeScriptAxiosClientCodegen codegen = new TypeScriptAxiosClientCodegen();
        codegen.additionalProperties().put("useErasableSyntax", true);

        codegen.processOpts();

        assertEquals(codegen.additionalProperties().get("useErasableSyntax"), true);
    }

    @Test
    public void testUseErasableSyntaxSetToFalse() {
        TypeScriptAxiosClientCodegen codegen = new TypeScriptAxiosClientCodegen();
        codegen.additionalProperties().put("useErasableSyntax", false);

        codegen.processOpts();

        assertEquals(codegen.additionalProperties().get("useErasableSyntax"), false);
    }

    @Test(description = "Verify useErasableSyntax config parameter generates erasable code")
    public void testUseErasableSyntaxGeneratesCorrectCode() throws IOException {
        boolean[] options = {true, false};
        for (boolean useErasableSyntax : options) {
            final File output = Files.createTempDirectory("typescript-axios-erasable-").toFile();
            output.deleteOnExit();

            final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("typescript-axios")
                .setInputSpec("src/test/resources/3_0/petstore.yaml")
                .addAdditionalProperty("useErasableSyntax", useErasableSyntax)
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

            final ClientOptInput clientOptInput = configurator.toClientOptInput();
            final DefaultGenerator generator = new DefaultGenerator();
            final List<File> files = generator.opts(clientOptInput).generate();
            files.forEach(File::deleteOnExit);

            Path basePath = Paths.get(output + "/base.ts");
            TestUtils.assertFileExists(basePath);

            if (useErasableSyntax) {
                // With erasable syntax: explicit property declarations
                TestUtils.assertFileContains(basePath, "protected basePath: string;");
                TestUtils.assertFileContains(basePath, "protected axios: AxiosInstance;");
                // Constructor parameters should NOT have access modifiers
                TestUtils.assertFileContains(basePath, "constructor(configuration?: Configuration, basePath: string = BASE_PATH, axios: AxiosInstance = globalAxios)");
                // Explicit assignments in constructor
                TestUtils.assertFileContains(basePath, "this.basePath = configuration?.basePath ?? basePath;");
                TestUtils.assertFileContains(basePath, "this.axios = axios;");
                // Class should close with } not };
                TestUtils.assertFileNotContains(basePath, "};\n\nexport class RequiredError");

                // RequiredError should have explicit field declaration
                TestUtils.assertFileContains(basePath, "public field: string;");
                TestUtils.assertFileContains(basePath, "this.field = field;");
            } else {
                // Without erasable syntax: parameter properties
                TestUtils.assertFileContains(basePath, "constructor(configuration?: Configuration, protected basePath: string = BASE_PATH, protected axios: AxiosInstance = globalAxios)");
                // Should have if (configuration) check
                TestUtils.assertFileContains(basePath, "if (configuration)");
                // Class should close with };
                TestUtils.assertFileContains(basePath, "};\n\nexport class RequiredError");

                // RequiredError should use parameter property
                TestUtils.assertFileContains(basePath, "constructor(public field: string, msg?: string)");
                TestUtils.assertFileNotContains(basePath, "this.field = field;");
            }
        }
    }
}
