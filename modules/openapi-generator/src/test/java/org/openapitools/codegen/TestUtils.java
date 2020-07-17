package org.openapitools.codegen;

import static org.testng.Assert.assertNotNull;
import static org.testng.Assert.fail;
import static org.testng.Assert.assertTrue;
import static org.testng.Assert.assertFalse;

import com.github.javaparser.ParseProblemException;
import com.github.javaparser.StaticJavaParser;
import com.github.javaparser.ast.CompilationUnit;
import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Paths;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.servers.Server;
import io.swagger.v3.parser.core.models.ParseOptions;

import org.openapitools.codegen.MockDefaultGenerator.WrittenTemplateBasedFile;
import org.openapitools.codegen.utils.ModelUtils;
import org.testng.Assert;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

public class TestUtils {

    /**
     * Helper method for parsing specs as a generator would be presented at runtime (inline models resolved, flattened).
     *
     * @param specFilePath The path to the specification file
     * @return A processed OpenAPI document
     */
    public static OpenAPI parseFlattenSpec(String specFilePath) {
        OpenAPI openAPI = parseSpec(specFilePath);
        InlineModelResolver inlineModelResolver = new InlineModelResolver();
        inlineModelResolver.flatten(openAPI);
        return openAPI;
    }

    /**
     * Helper method for parsing specs into an intermediary OpenAPI structure for pre-processing.
     *
     * Use this method only for tests targeting processing helpers such as {@link org.openapitools.codegen.utils.ModelUtils}
     * or {@link InlineModelResolver}. Using this for testing generators will mean you're not testing the OpenAPI document
     * in a state the generator will be presented at runtime.
     *
     * @param specFilePath The path to the specification file
     * @return A "raw" OpenAPI document
     */
    public static OpenAPI parseSpec(String specFilePath) {
        OpenAPI openAPI = new OpenAPIParser().readLocation(specFilePath, null, new ParseOptions()).getOpenAPI();
        // Invoke helper function to get the original swagger version.
        // See https://github.com/swagger-api/swagger-parser/pull/1374
        // Also see https://github.com/swagger-api/swagger-parser/issues/1369.
        ModelUtils.getOpenApiVersion(openAPI, specFilePath, null);
        return openAPI;
    }

    public static OpenAPI parseContent(String jsonOrYaml) {
        OpenAPI openAPI = new OpenAPIParser().readContents(jsonOrYaml, null, null).getOpenAPI();
        // Invoke helper function to get the original swagger version.
        ModelUtils.getOpenApiVersion(openAPI, jsonOrYaml, null);
        return openAPI;
    }

    public static OpenAPI createOpenAPI() {
        OpenAPI openAPI = new OpenAPI();
        openAPI.setComponents(new Components());
        openAPI.setPaths(new Paths());

        final Info info = new Info();
        info.setDescription("API under test");
        info.setVersion("1.0.7");
        info.setTitle("My title");
        openAPI.setInfo(info);

        final Server server = new Server();
        server.setUrl("https://localhost:9999/root");
        openAPI.setServers(Collections.singletonList(server));
        return openAPI;
    }

    public static OpenAPI createOpenAPIWithOneSchema(String name, Schema schema) {
        OpenAPI openAPI = createOpenAPI();
        openAPI.setComponents(new Components());
        openAPI.getComponents().addSchemas(name, schema);
        return openAPI;
    }

    /**
     * Extract file from {@link MockDefaultGenerator}
     *
     * @param generator Generator
     * @param root root path
     * @param filename filename under root
     *
     * @return a {@link WrittenTemplateBasedFile}
     * @deprecated Since 5.0. Please avoid this method and usage of {@link MockDefaultGenerator}, prefer {@link DefaultGenerator#DefaultGenerator(Boolean)} with dryRun=true.
     */
    @Deprecated
    public static WrittenTemplateBasedFile getTemplateBasedFile(MockDefaultGenerator generator, File root, String filename) {
        String defaultApiFilename = new File(root, filename).getAbsolutePath().replace("\\", "/");
        Optional<WrittenTemplateBasedFile> optional = generator.getTemplateBasedFiles().stream().filter(f -> defaultApiFilename.equals(f.getOutputFilename())).findFirst();
        Assert.assertTrue(optional.isPresent());
        return optional.get();
    }

    public static void ensureContainsFile(List<File> generatedFiles, File root, String filename) {
        Path path = root.toPath().resolve(filename);
        assertTrue(generatedFiles.contains(path.toFile()), "File '" + path.toAbsolutePath().toString() + "' was not found in the list of generated files");
    }

    public static void ensureDoesNotContainsFile(List<File> generatedFiles, File root, String filename) {
        Path path = root.toPath().resolve(filename);
        assertFalse(generatedFiles.contains(path.toFile()), "File '" + path.toAbsolutePath().toString() + "' was found in the list of generated files");
    }

    public static void validateJavaSourceFiles(Map<String, String> fileMap) {
        fileMap.forEach( (fileName, fileContents) -> {
                if (fileName.endsWith(".java")) {
                    assertValidJavaSourceCode(fileContents, fileName);
                }
            }
        );
    }

    public static void validateJavaSourceFiles(List<File> files) {
        files.forEach( f -> {
                    String fileName = f.getName();
                    if (fileName.endsWith(".java")) {
                        String fileContents = "";
                        try {
                            fileContents = new String(Files.readAllBytes(f.toPath()), StandardCharsets.UTF_8);
                        } catch (IOException ignored) {

                        }
                        assertValidJavaSourceCode(fileContents, fileName);
                    }
                }
        );
    }

    public static void assertValidJavaSourceCode(String javaSourceCode, String filename) {
        try {
            CompilationUnit compilation = StaticJavaParser.parse(javaSourceCode);
            assertTrue(compilation.getTypes().size() > 0, "File: " + filename);
        }
        catch (ParseProblemException ex) {
            fail("Java parse problem: " + filename, ex);
        }
    }

    public static void assertFileContains(Path path, String... lines) {
        try {
            String generatedFile = new String(Files.readAllBytes(path), StandardCharsets.UTF_8);
            String file = linearize(generatedFile);
            assertNotNull(file);
            for (String line : lines)
                assertTrue(file.contains(linearize(line)));
        } catch (IOException e) {
            fail("Unable to evaluate file " + path.toString());
        }
    }

    private static String linearize(String target) {
        return target.replaceAll("\r?\n", "").replaceAll("\\s+", "\\s");
    }

    public static void assertFileNotContains(Path path, String... lines) {
        String generatedFile = null;
        try {
            generatedFile = new String(Files.readAllBytes(path), StandardCharsets.UTF_8);
        } catch (IOException e) {
            fail("Unable to evaluate file " + path.toString());
        }
        String file = linearize(generatedFile);
        assertNotNull(file);
        for (String line : lines)
            assertFalse(file.contains(linearize(line)));
    }
}
