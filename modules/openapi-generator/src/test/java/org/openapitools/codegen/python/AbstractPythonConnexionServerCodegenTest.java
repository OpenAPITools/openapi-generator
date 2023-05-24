package org.openapitools.codegen.python;

import static org.assertj.core.api.Assertions.assertThat;
import static org.openapitools.codegen.languages.AbstractPythonConnexionServerCodegen.MOVE_TESTS_UNDER_PYTHON_SRC_ROOT;
import static org.openapitools.codegen.languages.AbstractPythonConnexionServerCodegen.PYTHON_SRC_ROOT;
import static org.openapitools.codegen.languages.AbstractPythonConnexionServerCodegen.USE_PYTHON_SRC_ROOT_IN_IMPORTS;

import java.io.File;
import java.util.Collections;
import java.util.Map;
import java.util.Objects;

import com.google.common.collect.ImmutableMap;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.languages.AbstractPythonConnexionServerCodegen;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

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

}
