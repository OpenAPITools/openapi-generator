package org.openapitools.codegen.python;

import com.google.common.collect.ImmutableMap;
import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.parser.core.models.ParseOptions;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.AbstractPythonConnexionServerCodegen;
import org.openapitools.codegen.languages.PythonAiohttpConnexionServerCodegen;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import static org.assertj.core.api.Assertions.assertThat;
import static org.openapitools.codegen.TestUtils.assertFileContains;
import static org.openapitools.codegen.TestUtils.assertFileExists;
import static org.openapitools.codegen.TestUtils.assertFileNotContains;
import static org.openapitools.codegen.languages.AbstractPythonConnexionServerCodegen.*;

public class AbstractPythonConnexionServerCodegenTest {

    /**
     * Passing "description" as first param to succinctly identify the significance of test parameters.
     */
    @Test(dataProvider = "data")
    public void test(String description, Map<String, Object> additionalProperties, String modelName,
                     ExpectedValues expectedValues) {
        AbstractPythonConnexionServerCodegen codegen = new MockAbstractPythonConnexionServerCodegen("", false);
        codegen.additionalProperties().putAll(additionalProperties);
        codegen.processOpts();
        String pythonSrcRoot = Objects.toString(codegen.additionalProperties().get(PYTHON_SRC_ROOT), null);
        assertThat(pythonSrcRoot).isEqualTo(expectedValues.pythonSrcRoot);
        assertThat(codegen.apiPackage()).isEqualTo(expectedValues.expectedApiPackage);
        assertThat(codegen.modelFileFolder()).isEqualTo(expectedValues.expectedModelFileFolder);
        assertThat(codegen.apiFileFolder()).isEqualTo(expectedValues.expectedApiFileFolder);
        assertThat(codegen.apiTestFileFolder()).isEqualTo(expectedValues.expectedApiTestFileFolder);
        assertThat(codegen.toModelImport(modelName)).isEqualTo(expectedValues.expectedImport);
    }

    @DataProvider
    public Object[][] data() {
        return new Object[][]{
                new Object[]{
                        "Default setup",
                        Collections.emptyMap(),
                        "TestModel",
                        new ExpectedValues("from openapi_server.models.test_model import TestModel",
                                "openapi_server.controllers",
                                platformAgnosticPath("generated-code", "connexion", "openapi_server", "models"),
                                platformAgnosticPath("generated-code", "connexion", "openapi_server", "controllers"),
                                platformAgnosticPath("generated-code", "connexion", "test"),
                                null)
                },
                new Object[]{
                        "Default setup with Python src root",
                        ImmutableMap.of(PYTHON_SRC_ROOT, "test_root"),
                        "TestModel",
                        new ExpectedValues("from openapi_server.models.test_model import TestModel",
                                "openapi_server.controllers",
                                platformAgnosticPath("generated-code", "connexion", "test_root", "openapi_server", "models"),
                                platformAgnosticPath("generated-code", "connexion", "test_root", "openapi_server", "controllers"),
                                platformAgnosticPath("generated-code", "connexion", "test"),
                                "test_root")
                },
                new Object[]{
                        "Python src in import",
                        ImmutableMap.of(PYTHON_SRC_ROOT, "test_root", USE_PYTHON_SRC_ROOT_IN_IMPORTS, "true"),
                        "TestModel",
                        new ExpectedValues("from test_root.openapi_server.models.test_model import TestModel",
                                "test_root.openapi_server.controllers",
                                platformAgnosticPath("generated-code", "connexion", "test_root", "openapi_server", "models"),
                                platformAgnosticPath("generated-code", "connexion", "test_root", "openapi_server", "controllers"),
                                platformAgnosticPath("generated-code", "connexion", "test"),
                                null)
                },
                new Object[]{
                        "Python src in import and tests under python src root",
                        ImmutableMap.of(PYTHON_SRC_ROOT, "test_root", USE_PYTHON_SRC_ROOT_IN_IMPORTS, "true",
                                MOVE_TESTS_UNDER_PYTHON_SRC_ROOT, "true"),
                        "TestModel",
                        new ExpectedValues("from test_root.openapi_server.models.test_model import TestModel",
                                "test_root.openapi_server.controllers",
                                platformAgnosticPath("generated-code", "connexion", "test_root", "openapi_server", "models"),
                                platformAgnosticPath("generated-code", "connexion", "test_root", "openapi_server", "controllers"),
                                platformAgnosticPath("generated-code", "connexion", "test_root", "test"),
                                null)
                },
                new Object[]{
                        "Python src in import with specified package",
                        ImmutableMap.of(PYTHON_SRC_ROOT, "test_root",
                                USE_PYTHON_SRC_ROOT_IN_IMPORTS, "true",
                                CodegenConstants.PACKAGE_NAME, "test_package"),
                        "TestModel",
                        new ExpectedValues("from test_root.test_package.models.test_model import TestModel",
                                "test_root.test_package.controllers",
                                platformAgnosticPath("generated-code", "connexion", "test_root", "test_package", "models"),
                                platformAgnosticPath("generated-code", "connexion", "test_root", "test_package", "controllers"),
                                platformAgnosticPath("generated-code", "connexion", "test"),
                                null)
                },
                new Object[]{
                        "Python src in import with specified package and tests under python src root",
                        ImmutableMap.of(PYTHON_SRC_ROOT, "test_root",
                                USE_PYTHON_SRC_ROOT_IN_IMPORTS, "true",
                                CodegenConstants.PACKAGE_NAME, "test_package",
                                MOVE_TESTS_UNDER_PYTHON_SRC_ROOT, "true"),
                        "TestModel",
                        new ExpectedValues("from test_root.test_package.models.test_model import TestModel",
                                "test_root.test_package.controllers",
                                platformAgnosticPath("generated-code", "connexion", "test_root", "test_package", "models"),
                                platformAgnosticPath("generated-code", "connexion", "test_root", "test_package", "controllers"),
                                platformAgnosticPath("generated-code", "connexion", "test_root", "test"),
                                null)
                }
        };
    }

    private static class MockAbstractPythonConnexionServerCodegen extends AbstractPythonConnexionServerCodegen {
        public MockAbstractPythonConnexionServerCodegen(String templateDirectory, boolean fixBodyNameValue) {
            super(templateDirectory, fixBodyNameValue);
        }
    }

    private static class ExpectedValues {
        public final String expectedImport;
        public final String expectedApiPackage;
        public final String expectedModelFileFolder;
        public final String expectedApiFileFolder;
        public final String expectedApiTestFileFolder;
        public final String pythonSrcRoot;

        public ExpectedValues(String expectedImport, String expectedApiPackage, String expectedModelFileFolder,
                              String expectedApiFileFolder, String expectedApiTestFileFolder, String pythonSrcRoot) {
            this.expectedImport = expectedImport;
            this.expectedApiPackage = expectedApiPackage;
            this.expectedModelFileFolder = expectedModelFileFolder;
            this.expectedApiFileFolder = expectedApiFileFolder;
            this.expectedApiTestFileFolder = expectedApiTestFileFolder;
            this.pythonSrcRoot = pythonSrcRoot != null ? pythonSrcRoot + File.separatorChar : null;
        }
    }

    private static String platformAgnosticPath(String... nodes) {
        return StringUtils.join(nodes, File.separatorChar);
    }

    @Test(description = "python-aiohttp map parameters should iterate .items()")
    public void testAiohttpMapParametersIterateItems() throws IOException {
        String specPath = "src/test/resources/3_1/python/map-and-freeform-params.yaml";

        File aiohttpOutput = Files.createTempDirectory("test-aiohttp-map-params").toFile().getCanonicalFile();
        try {
            CodegenConfigurator aiohttpConfig = new CodegenConfigurator()
                    .setGeneratorName("python-aiohttp")
                    .setInputSpec(specPath)
                    .setOutputDir(aiohttpOutput.getAbsolutePath());
            new DefaultGenerator().opts(aiohttpConfig.toClientOptInput()).generate();

            Path aiohttpController = Paths.get(
                    aiohttpOutput.getAbsolutePath(),
                    "openapi_server",
                    "controllers",
                    "default_controller.py");
            assertFileExists(aiohttpController);
            assertFileContains(
                    aiohttpController,
                    "date_map = {k: util.deserialize_date(v) for k, v in date_map.items()}",
                    "datetime_map = {k: util.deserialize_datetime(v) for k, v in datetime_map.items()}",
                    "model_map = {k: Tag.from_dict(v) for k, v in model_map.items()}");
            assertFileNotContains(
                    aiohttpController,
                    "for k, v in date_map}",
                    "for k, v in datetime_map}",
                    "for k, v in model_map}");
        } finally {
            FileUtils.deleteDirectory(aiohttpOutput);
        }
    }

    @Test(description = "Connexion servers should not call from_dict on free-form or any-type parameters")
    public void testFreeFormAndAnyTypeParametersDoNotCallFromDict() throws IOException {
        String specPath = "src/test/resources/3_1/python/map-and-freeform-params.yaml";

        OpenAPI openAPI = new OpenAPIParser().readLocation(specPath, null, new ParseOptions()).getOpenAPI();
        PythonAiohttpConnexionServerCodegen codegen = new PythonAiohttpConnexionServerCodegen();
        codegen.setOpenAPI(openAPI);
        List<Parameter> parameters = openAPI.getPaths().get("/test").getGet().getParameters();
        CodegenParameter freeFormParam = codegen.fromParameter(parameters.get(3), new HashSet<>());
        CodegenParameter anyValParam = codegen.fromParameter(parameters.get(4), new HashSet<>());

        assertThat(freeFormParam.paramName).isEqualTo("free_form");
        assertThat(freeFormParam.isFreeFormObject).isTrue();
        assertThat(freeFormParam.isAnyType).isFalse();
        assertThat(freeFormParam.isContainer).isFalse();
        assertThat(freeFormParam.isPrimitiveType).isFalse();
        assertThat(freeFormParam.dataType).isEqualTo("object");

        assertThat(anyValParam.paramName).isEqualTo("any_val");
        assertThat(anyValParam.isFreeFormObject).isFalse();
        assertThat(anyValParam.isAnyType).isTrue();
        assertThat(anyValParam.isContainer).isFalse();
        assertThat(anyValParam.isPrimitiveType).isFalse();
        assertThat(anyValParam.dataType).isEqualTo("object");

        File aiohttpOutput = Files.createTempDirectory("test-aiohttp-freeform-params").toFile().getCanonicalFile();
        try {
            CodegenConfigurator aiohttpConfig = new CodegenConfigurator()
                    .setGeneratorName("python-aiohttp")
                    .setInputSpec(specPath)
                    .setOutputDir(aiohttpOutput.getAbsolutePath());
            new DefaultGenerator().opts(aiohttpConfig.toClientOptInput()).generate();

            Path aiohttpController = Paths.get(
                    aiohttpOutput.getAbsolutePath(),
                    "openapi_server",
                    "controllers",
                    "default_controller.py");
            assertFileExists(aiohttpController);
            assertFileNotContains(
                    aiohttpController,
                    "free_form = ",
                    "any_val = ");
        } finally {
            FileUtils.deleteDirectory(aiohttpOutput);
        }

        File blueplanetOutput = Files.createTempDirectory("test-blueplanet-freeform-params").toFile().getCanonicalFile();
        try {
            CodegenConfigurator blueplanetConfig = new CodegenConfigurator()
                    .setGeneratorName("python-blueplanet")
                    .setInputSpec(specPath)
                    .setOutputDir(blueplanetOutput.getAbsolutePath());
            new DefaultGenerator().opts(blueplanetConfig.toClientOptInput()).generate();

            Path blueplanetController = Paths.get(
                    blueplanetOutput.getAbsolutePath(),
                    "app",
                    "openapi_server",
                    "controllers",
                    "default_controller.py");
            assertFileExists(blueplanetController);
            assertFileNotContains(
                    blueplanetController,
                    "free_form = ",
                    "any_val = ");
        } finally {
            FileUtils.deleteDirectory(blueplanetOutput);
        }

        File flaskOutput = Files.createTempDirectory("test-flask-freeform-params").toFile().getCanonicalFile();
        try {
            CodegenConfigurator flaskConfig = new CodegenConfigurator()
                    .setGeneratorName("python-flask")
                    .setInputSpec(specPath)
                    .setOutputDir(flaskOutput.getAbsolutePath());
            new DefaultGenerator().opts(flaskConfig.toClientOptInput()).generate();

            Path flaskController = Paths.get(
                    flaskOutput.getAbsolutePath(),
                    "openapi_server",
                    "controllers",
                    "default_controller.py");
            assertFileExists(flaskController);
            assertFileNotContains(
                    flaskController,
                    "free_form =",
                    "any_val =");
        } finally {
            FileUtils.deleteDirectory(flaskOutput);
        }
    }

}
