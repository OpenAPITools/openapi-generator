package org.openapitools.codegen.java.micronaut;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.parser.core.models.ParseOptions;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.languages.JavaMicronautAbstractCodegen;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.regex.Pattern;

import static org.testng.Assert.*;


/**
 * An abstract class with methods useful for testing
 */
public abstract class AbstractMicronautCodegenTest {
    /**
     * Path to a common test configuration file
     */
    protected final String PETSTORE_PATH = "src/test/resources/petstore.json";

    /**
     *
     * @param codegen - the code generator
     * @param configPath - the path to the config starting from src/test/resources
     * @param filesToGenerate - which files to generate - can be CodegenConstants.MODELS, APIS, SUPPORTING_FILES, ...
     * @return - the path to the generated folder
     */
    protected String generateFiles(JavaMicronautAbstractCodegen codegen, String configPath, String... filesToGenerate) {
        File output = null;
        try {
            output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        } catch (IOException e) {
            fail("Unable to create temporary directory for output");
        }
        output.deleteOnExit();

        // Create parser
        String outputPath = output.getAbsolutePath().replace('\\', '/');
        OpenAPI openAPI = new OpenAPIParser()
                .readLocation(configPath, null, new ParseOptions()).getOpenAPI();

        // Configure codegen
        codegen.setOutputDir(outputPath);

        // Create input
        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        // Generate
        DefaultGenerator generator = new DefaultGenerator();
        // by default nothing is generated
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.API_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.API_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");
        // set all the files user wants to generate
        for (String files: filesToGenerate) {
            generator.setGeneratorPropertyDefault(files, "true");
        }

        generator.opts(input).generate();

        return outputPath + "/";
    }

    public static void assertFileContainsRegex(String path, String... regex) {
        String file = readFile(path);
        for (String line: regex)
            assertTrue(Pattern.compile(line.replace(" ", "\\s+")).matcher(file).find());
    }

    public static void assertFileNotContainsRegex(String path, String... regex) {
        String file = readFile(path);
        for (String line: regex)
            assertFalse(Pattern.compile(line.replace(" ", "\\s+")).matcher(file).find());
    }

    public static void assertFileContains(String path, String... lines) {
        String file = linearize(readFile(path));
        for (String line : lines)
            assertTrue(file.contains(linearize(line)), "File does not contain line [" + line + "]");
    }

    public static void assertFileNotContains(String path, String... lines) {
        String file = linearize(readFile(path));
        for (String line : lines)
            assertFalse(file.contains(linearize(line)), "File contains line [" + line + "]");
    }

    public static void assertFileExists(String path) {
        assertTrue(Paths.get(path).toFile().exists(), "File \"" + path + "\" should exist");
    }

    public static void assertFileNotExists(String path) {
        assertFalse(Paths.get(path).toFile().exists(), "File \"" + path + "\" should not exist");
    }

    public static String readFile(String path) {
        String file = null;
        try {
            file = new String(Files.readAllBytes(Paths.get(path)), StandardCharsets.UTF_8);
            assertNotNull(file, "File \"" + path + "\" does not exist");
        } catch (IOException e) {
            fail("Unable to evaluate file " + path);
        }

        return file;
    }

    public static String linearize(String target) {
        return target.replaceAll("\r?\n", "").replaceAll("\\s+", "\\s");
    }
}
