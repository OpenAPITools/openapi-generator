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
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.testng.Assert.assertEquals;

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

    @Test(description = "Verify @deprecated annotation is generated for array-type properties")
    public void testDeprecatedArrayAttribute() throws Exception {
        final File output = Files.createTempDirectory("typescript_axios_deprecated_array_").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("typescript-axios")
                .setInputSpec("src/test/resources/3_1/typescript-axios/deprecated-array-attribute.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        final DefaultGenerator generator = new DefaultGenerator();
        final List<File> files = generator.opts(clientOptInput).generate();
        files.forEach(File::deleteOnExit);

        Path file = Paths.get(output + "/api.ts");
        String content = Files.readString(file);

        // The model has three deprecated properties:
        //   'age' (integer), 'tags' (array of strings), 'oldTags' (array of $ref Tag objects)
        // and one non-deprecated array property: 'nicknames'
        // There should be exactly 3 occurrences of @deprecated in the model file
        assertEquals(TestUtils.countOccurrences(content, "@deprecated"), 3,
                "Expected @deprecated on 'age' (scalar), 'tags' (array of strings), and 'oldTags' (array of $ref objects)");

        // Verify the @deprecated annotation appears in the generated output
        TestUtils.assertFileContains(file, "* @deprecated");

        // Verify the deprecated array property 'tags' (array of primitives) is present with correct type
        TestUtils.assertFileContains(file, "'tags'?: Array<string>");

        // Verify the deprecated array property 'oldTags' (array of $ref objects) is present with correct type
        TestUtils.assertFileContains(file, "'oldTags'?: Array<Tag>");

        // Verify the non-deprecated array property 'nicknames' is also present
        TestUtils.assertFileContains(file, "'nicknames'?: Array<string>");
    }
}
