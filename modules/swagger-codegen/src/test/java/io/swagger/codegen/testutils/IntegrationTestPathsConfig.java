package io.swagger.codegen.testutils;

import java.nio.file.Path;
import java.nio.file.Paths;

public class IntegrationTestPathsConfig {
    private static final Path INTEGRATION_TEST_PATH = Paths.get("target/test-classes/integrationtests").toAbsolutePath();
    private final Path outputPath;
    private final Path specPath;
    private final Path expectedPath;
    private final Path ignoreFilePath;

    public IntegrationTestPathsConfig(String location) {
        this(location + "-spec.json", location + "-result", location + "-expected", location + ".ignore");
    }

    public IntegrationTestPathsConfig(String specLocation, String outputLocation, String expectedLocation, String ignoreFileLocation) {
        outputPath = INTEGRATION_TEST_PATH.resolve(outputLocation);
        expectedPath = INTEGRATION_TEST_PATH.resolve(expectedLocation);
        specPath = INTEGRATION_TEST_PATH.resolve(specLocation);
        ignoreFilePath = INTEGRATION_TEST_PATH.resolve(ignoreFileLocation);
    }

    public Path getOutputPath() {
        return outputPath;
    }

    public Path getSpecPath() {
        return specPath;
    }

    public Path getExpectedPath() {
        return expectedPath;
    }

    public Path getIgnoreFilePath() { return ignoreFilePath; }
}
